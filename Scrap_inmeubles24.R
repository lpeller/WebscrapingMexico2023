library(rvest)
library(stringr)
library(plyr)
library(xml2)

options(digits=16)

#INITIALISATION
MC_data <- NULL   #dataframe qui contient toutes les informations extraites
id <- 0

#les url des pages d'offres sotn sous la forme :
#https://www.inmuebles24.com/*type de logement*-en-venta-en-*municipe*.html
#ex : https://www.inmuebles24.com/casas-en-venta-en-gustavo-a.-madero.html

#les municipes et les états sont listés et mis en forme dans le fichier csv DATA/municipes_noms.csv (données brutes : DATA/municipes_noms.txt)
#source : https://es.wikipedia.org/wiki/Zona_metropolitana_del_valle_de_M%C3%A9xico

df_municipios <- read.csv("DATA/municipes_noms.csv", stringsAsFactors = FALSE)
df_municipios[df_municipios == "gustavo-a-madero"] <- "gustavo-a.-madero"   #correction pour le cas du municipe de Gustavo A. Madero

#Lise de municipes du transect
df_municipios <- df_municipios[df_municipios$municipio %in% c("cuauhtemoc", "gustavo-a.-madero", "ecatepec-de-morelos", "tecamac", "temascalapa", "benito-juarez", "zumpango"),]

#exemple de test
# df_municipios <- data.frame(id=1,municipe="apaxco",estado="mexico")
# df_municipios <- rbind(df_municipios, data.frame(id=2,municipe="cocotitlan", estado="mexico"))
# df_municipios <- rbind(df_municipios, data.frame(id=3,municipe="tepotzotlan", estado="mexico"))

#TRAITEMENT
nb_municipes <- nrow(df_municipios)
mun_range <- 1:nb_municipes

#types de biens vendus sur le site
l_type <- c("casa","duplex","casa-en-condominio","departamento-compartido","rancho","villa",
            "casa-uso-de-suelo","huerta","inmueble-productivo-urbano","local-en-centro-comercial",
            "departamentos","terreno-comercial","bodegas-comerciales","nave-industrial",
            "terreno-industrial","terrenos","desarrollo-horizontal","desarrollo-vertical",
            "desarrollo-horizontal","oficinas","locales-comerciales","edificio")

#types de biens recherchés, parmis ceux disponibles
l_type_wanted <- l_type

url_base <- "https://www.inmuebles24.com/"
for (t in 1:length(l_type_wanted)){
  if (length(l_type_wanted) == 1){
    url_base <- paste(url_base,l_type_wanted[t],sep="")
  }else if(t == length(l_type_wanted)){
    url_base <- paste(url_base,l_type_wanted[t],sep="")
  }else{
    url_base <- paste(url_base,l_type_wanted[t],"-o-",sep="")
  }
}

for (mun in mun_range){
  print(paste("Scrapping goods for municipio",df_municipios[mun,2]))
  url_start <- paste(url_base,"-en-venta-en-",df_municipios[mun,2],sep="")
  
  url_page <- paste(url_start,".html", sep ="")
  id_page <- 1 #numero de la page d'offre en cours pour un municipe
  nb_good <- 1 #numero du bien pour un municipe
  
  try_default(html_page <- read_html(url_page), html_page <- NA)
  
  try_conn <- 0
  conn <- TRUE  #sécurité au cas ou un nom de municipe serait mal orthographié
  
  if(is.na(html_page)){
    while(is.na(html_page) & try_conn <= 8){
      try_conn <- try_conn + 1
      print("An error has occured during the page loading, reloading the page")
      sleepDuration <- runif(n = 1, min = 4, max = 12)
      print(paste("sleeping",sleepDuration,"seconds"))
      Sys.sleep(sleepDuration)
      try_default(html_page <- read_html(url_page), html_page <- NA)
    }
    if(try_conn >= 6){
      print(paste("Connection failed for municipio",df_municipios[mun,2]))
      conn <- FALSE
    }else{print("Connection success...")}
  }
  
  max_good <- as.integer(gsub("[^0-9.]", "",html_page %>% 
                                html_node(xpath='//*[@class="sc-1oqs0ed-0 guPmPw"]') %>% 
                                html_text()))
  
  max_pages <- max_good %/% 20 + 1
  print(paste("There are",max_good,"goods avaliable, within",max_pages,"pages"))
  
  if(is.na(max_good)){
    conn <- FALSE
  }
  
  if (conn == TRUE){
    while(is.na(html_page)==FALSE & id_page <= max_pages){
      print(paste("Page",id_page))
      #liste des URL vers chaque bien de la page
      detailedListings <-  html_page %>%
        html_nodes(xpath='//*[@class="sc-i1odl-0 cYmZqs"]')%>%
        html_attr("data-to-posting")
      
      nb_offer = length(detailedListings) #nombre d'offres sur la page
      
      offer_range <- 1:nb_offer
      
      #on parcours les offres de la page
      if (identical(detailedListings, character(0)) == FALSE){  #si le site ne propose pas d'offre pour un municipe, alors on passe à la suite
        for (i in offer_range){
          #Récupération des différentes informations pour un bien :
          id <- id+1
          print(paste("Next offer, id = ",id))
          #extraction de l'url du bien
          
          #id de l'offre
          id_offer <- as.integer(gsub("[^0-9]", "",substr(detailedListings[i], str_locate_all(detailedListings[i],"-")[[1]][length(str_locate_all(detailedListings[i],"-")[[1]])], str_locate_all(detailedListings[i],".html")[[1]])))
          
          detailedListings[i] <- paste("https://www.inmuebles24.com",detailedListings[i], sep = "")
          try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
          
          try_conn <- 0
          #test de la connection
          if(is.na(html_good)){
            while(is.na(html_good) & try_conn <=6){
              try_conn <- try_conn + 1
              print("An error has occured during the page loading, reloading the page")
              sleepDuration <- runif(n = 1, min = 4, max = 12)
              print(paste("sleeping",sleepDuration,"seconds"))
              Sys.sleep(sleepDuration)
              try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
            }
            print("Connection success...")
          }else{print("Connection success...")}
          print(paste("URL to see the offer :",detailedListings[i]))
          
          if(!is.na(html_good)){
            
            #adresse du bien
            adress <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/hgroup/h2") %>% 
              html_text()
            adress <- substr(adress, str_locate(adress, "\n\t\t")[2]+1, str_locate(adress, " \n\t\t\n\t\t\t\n")[2]-9)
            estado <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/ul/li[4]/a") %>% 
              html_attr("title")
            municipio <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/ul/li[5]/a") %>% 
              html_attr("title")
            colonia <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/ul/li[6]/a") %>% 
              html_attr("title")
            
            #caractéristiques du bien (surface, chambres, ...)
            l_caract <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/hgroup/ul") %>% 
              html_nodes(xpath='//*[@class="icon-feature"]')
            
            surface_good <- NA
            surface_grnd <- NA
            nb_room <- NA
            nb_bathroom <- NA
            
            for (car in l_caract){
              options(warn=-1)  #cacher les messages warning qui polluent le visuel
              feature <- substr(car,str_locate_all(car,'class="')[[1]][2,2]+1,str_locate(car,'"></i>')[1]-1)
              if (feature == "icon-stotal"){
                surface_grnd <- as.double(gsub("[^0-9.]", "",car))
              }else if(feature == "icon-scubierta"){
                surface_good <- as.double(gsub("[^0-9.]", "",car))
              }else if(feature =="icon-bano"){
                nb_bathroom <- as.integer(gsub("[^0-9.]", "",car))
              }else if(feature == "icon-dormitorio"){
                nb_room <-  as.integer(gsub("[^0-9.]", "",car))
              }
              options(warn=0)
            }
            
            
            #prix de l'offre
            price <- as.integer(gsub("[^0-9.]", "",html_good %>% 
                                       html_node(xpath="/html/body/div[2]/main/div/div/article/div/div[1]/div[1]/div/div/div/div[2]/span/span") %>% 
                                       html_text()))
            
            #monnaie
            currency <- gsub("[^A-Z]", "",html_good %>%
                               html_node(xpath="/html/body/div[2]/main/div/div/article/div/div[1]/div[1]/div/div/div/div[2]/span/span") %>% 
                               html_text())
            
            #type de logement
            type <- html_good %>% 
              html_node(xpath="/html/body/div[2]/main/div/div/article/div/div[1]/div[1]/h2") %>% 
              html_text()
            type <- substr(type, 1, str_locate(type, " · ")[1]-1)
            
            #coordonnées du logement
            location_text <- xml_attrs(html_good %>%
                                         html_node(xpath='//*[@id="static-map"]'))[["src"]]
            latlng <- str_split(substr(location_text, str_locate(location_text, "markers=")[2]+1, str_locate(location_text, "&key=")[1]-1), ",", simplify = TRUE)
            lat <- as.double(latlng[1])
            lng <- as.double(latlng[2])
            
            #description du bien
            desc_good <- gsub('[\t\n]','',html_good %>% 
                                html_node(xpath='/html/body/div[2]/main/div/div/article/div/div[4]/section[1]') %>% 
                                html_text())
            
            print(paste("offer recap :"))
            print(paste("id:",id))
            print(paste("id_offer:",id_offer))
            print(paste("adress :",adress))
            print(paste("type :",type))
            print(paste("price :",price))
            print(paste("currency :",currency))
            print(paste("good surface :",surface_good))
            print(paste("ground surface :",surface_grnd))
            print(paste("room count :",nb_room))
            print(paste("bathroom count :",nb_bathroom))
            print(paste("lat :",lat))
            print(paste("lng :",lng))
            print(paste("colonia :",colonia))
            print(paste("municipio :",municipio))
            print(paste("estado :",estado))
            
            
            MC_data <- rbind(MC_data, data.frame(id_offer,adress,type,price,currency,surface_grnd,surface_good,nb_room,nb_bathroom,lat,lng,colonia,municipio,estado,desc_good,detailedListings[i]))
          }
          #pause for a random duration between 4 and 12 seconds to stay stealth
          sleepDuration <- runif(n = 1, min = 4, max = 10)
          print(paste("sleeping",sleepDuration,"seconds"))
          Sys.sleep(sleepDuration)
          
          id_last <- id_offer
        }
        #url de la page suivante :
        id_page <- id_page + 1
        url_next <- paste(url_start,"-pagina-",id_page,".html",sep="")
        
        try_default(html_page <- read_html(url_next), html_page <- NA)
        
        try_conn <- 0
        
        if(is.na(html_page)){
          while(is.na(html_page) & try_conn <= 6){
            try_conn <- try_conn + 1
            print("An error has occured during the page loading, reloading the page")
            sleepDuration <- runif(n = 1, min = 4, max = 12)
            print(paste("sleeping",sleepDuration,"seconds"))
            Sys.sleep(sleepDuration)
            try_default(html_page <- read_html(url_next), html_page <- NA)
          }
          if(try_conn >= 6){
            print(paste("Connection failed for municipio",df_municipios[mun,2]))
            conn <- FALSE
          }else{print("Connection success...")}
        }
        
        #ecriture du fichier csv
        write.csv(MC_data,"inmeubles24.csv")
        
      }else{html_page <- NA}
      sleepDuration <- runif(n = 1, min = 4, max = 10)
      print(paste("sleeping",sleepDuration,"seconds"))
      Sys.sleep(sleepDuration)
    }
  }
}

