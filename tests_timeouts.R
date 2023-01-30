# Essai du scraping de metroscubicos avec différentes options

library(rvest)
library(stringr)
library(plyr)
library(xml2)
library(tidyverse)

df_municipios <- read.csv("DATA/municipes_noms.csv", stringsAsFactors = FALSE)
#le site metroscubicos utilise l'appellation "distrito-federal" pour l'état de "ciudad-de-mexico"
df_municipios[df_municipios == "ciudad-de-mexico"] <- "distrito-federal"

nb_municipes <- nrow(df_municipios)
mun_range <- 1:nb_municipes
l_type <- c("bodegas","casas","cuartos","departamentos","edificios","haciendas",
            "ranchos","oficinas","locales-comerciales","terrenos","otros")

#types de biens recherchés par l'utilisateur
l_type_wanted <- l_type

#site désiré : mercadolibre ou metroscubicos
url_base <- "https://inmuebles.mercadolibre.com.mx/"
#url_base <- "https://inmuebles.metroscubicos.com/"

#on parcourt la liste de tous les municipes
for (mun in mun_range){
  print(paste("Municipio",df_municipios[mun,2],":"))
  url_page_noType <- paste(url_base,"venta/",df_municipios[mun,3],"/",df_municipios[mun,2],"/",sep="")
  
  try_default(html_page_noType <- read_html(url_page_noType), html_page_noType <- NA)
  
  nb_try <- 0
  if(is.na(html_page_noType)){
    while(is.na(html_page_noType) & nb_try <= 8){
      nb_try <- nb_try + 1
      print("An error has occured during the page loading, reloading the page")
      sleepDuration <- sample(4:12,1)
      print(paste("sleeping",sleepDuration,"seconds"))
      Sys.sleep(sleepDuration)
      try_default(html_page_noType <- read_html(url_page_noType), html_page_noType <- NA)
    }
    if(nb_try<=8){print("Connection success...")
    }else{print("Connection failed...")}
  }
  
  
  l_titres_disp <- html_page_noType %>% 
    html_nodes(xpath='/html/body/main/div/div[2]/aside/section[2]') %>% 
    html_children()
  
  pos <- 1
  for (i in 1:length(l_titres_disp)){
    exam <- l_titres_disp[[i]] %>% html_text()
    if(grepl("Inmueble",exam,fixed = TRUE)){
      pos <- i
    }
  }
  
  #types de biens disponibles dans la municipe
  l_type_avaliable <- strsplit(gsub('[()]',';',gsub('[0-9,]+', '', tolower(html_page_noType %>% 
                                                                             html_node(xpath=paste('/html/body/main/div/div[2]/aside/section[2]/div[',pos,']/ul')) %>% 
                                                                             html_text()))), split = ';;' )
  
  #création d'une liste contenant les types de biens voulus disponibles dans la municipe
  l_type_wanted_avaliable <- c()
  print("Avaliable goods types :")
  for (t in l_type_wanted){
    if (t %in% l_type_avaliable[[1]]){
      l_type_wanted_avaliable <- c(l_type_wanted_avaliable, t)
    }
  }
  print(l_type_wanted_avaliable)
  #on parcourt les types de biens voulus
  for (t in l_type_wanted_avaliable){
    print(paste("Scrapping",t,"for municipio",df_municipios[mun,2]))
    url_page <- paste(url_base,t,"/venta/",df_municipios[mun,3],"/",df_municipios[mun,2],"/",sep="")
    
    #boucle qui parcours les pages du site
    while(is.na(url_page) == FALSE){
      
      try_default(html_page <- read_html(url_page), html_page <- NA)
      
      nb_try <- 0
      if(is.na(html_page)){
        while(is.na(html_page) & nb_try<=8){
          nb_try <- nb_try + 1
          print("An error has occured during the page loading, reloading the page")
          sleepDuration <- sample(4:12,1)
          print(paste("sleeping",sleepDuration,"seconds"))
          Sys.sleep(sleepDuration)
          try_default(html_page <- read_html(url_page), html_page <- NA)
        }
        if(nb_try<=8){print("Connection success...")
        }else{print("Connection failed...")}
      }
      
      #liste des URL vers chaque bien de la page (48 max par page)
      detailedListings <-  html_page %>%
        html_nodes(xpath='//*[@class="ui-search-result__content ui-search-link"]')%>%
        html_attr("href")
      
      nb_offer = length(detailedListings)
      
      offer_range <- 1:nb_offer
      if (identical(detailedListings, character(0)) == FALSE){
        for(i in offer_range){
          
          #Récupération des différentes informations pour un bien :
          id <- id+1
          print(paste("Next offer, id = ",id))
          #extraction de l'url du bien
          try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
          #html_good <- detailedListings[i] %>% GET(., timeout(10)) %>% read_html
          
          #test de la connection, relance de la connection si elle n'est pas effective (pour éviter lmes error 404)
          nb_try <- 0
          if(is.na(html_good)){
            while(is.na(html_good) & nb_try <=8){
              nb_try <- nb_try + 1
              print("An error has occured during the page loading, reloading the page")
              sleepDuration <- sample(4:12,1)
              print(paste("sleeping",sleepDuration,"seconds"))
              Sys.sleep(sleepDuration)
              try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
            }
            if(nb_try<=8){print("Connection success...")
            }else{print("Connection failed...")}
          }else{print("Connection success...")}
          print(paste("URL to see the offer :",detailedListings[i]))
          
          #identifiant de l'offre
          id_offer <- as.integer(gsub("[^0-9.-]", "",html_good %>% 
                                        html_node(xpath='/html/body/main/div/div[5]/div[1]/div/p/span') %>% 
                                        html_text()))
          
          #type de bien
          type <- html_good %>% 
            html_node(xpath='//*[@class="ui-pdp-subtitle"]') %>% 
            html_text()
          
          #prix du bien
          price <-  as.integer(str_remove_all(html_good %>% 
                                                html_node(xpath='//*[@class="andes-money-amount__fraction"]') %>% 
                                                html_text(), ","))
          
          #monnaie du bien : $ (pesos) ou U$S (dollar US)
          currency <- html_good %>% 
            html_node(xpath='/html/body/main/div/div[4]/div/div[1]/div[1]/div/div[2]/div/div/span/span[2]') %>% 
            html_text
          
          #Pour la localisation , le site web fournit une carte en image centrée sur la localisation du bien. Les coordonnées du centre
          #de la carte sont disponible dans l'url de l'image
          try_default(location_text <- xml_attrs(html_good %>%
                                                   html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[1]/div/div[2]/div/img'))[["src"]],
                      location_text <- NA)
          
          #si la carte ne charge pas, recharger la page permet d'obtenir les informations souhaitées
          #si la carte ne charge pas au bout de 6 essais, elle est considérée comme manquante
          nb_try <- 0
          if(is.na(location_text)){
            while(is.na(location_text) & nb_try <= 6){
              nb_try <- nb_try +1
              print("An error has occured (map searching), reloading the page")
              sleepDuration <- sample(4:12,1)
              print(paste("sleeping",sleepDuration,"seconds"))
              Sys.sleep(sleepDuration)
              
              #rechargement de la page
              try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
              
              #on teste si la page est bien chargée
              if(is.na(html_good)){
                while(is.na(html_good)){
                  print("An error has occured during the page loading, reloading the page")
                  sleepDuration <- sample(4:12,1)
                  print(paste("sleeping",sleepDuration,"seconds"))
                  Sys.sleep(sleepDuration)
                  try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
                }
                print("Connection success...")
              }
              
              try_default(location_text <- xml_attrs(html_good %>%
                                                       html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[1]/div/div[2]/div/img'))[["src"]],
                          location_text <- NA)
              
            }
            if(nb_try <= 6){print("Map successfully loaded")
            }else{print("Error while map loading...")}
          }
          
          #la fonction gsub() permet de ne conserver que les caractères numériques
          try_default(lat <- as.double(gsub("[^0-9.-]", "", substr(location_text, str_locate(location_text, "center=")[2]+1, str_locate(location_text, "%2C")[2]-3))),
                      lat <- NA)
          try_default(lng <- as.double(gsub("[^0-9.-]", "", substr(location_text, str_locate(location_text, "%2C")[2]+1, str_locate(location_text, "%2C")[2]+13))),
                      lng <- NA)
          
          
          #adresse
          adress <- html_good %>% 
            html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[1]/div/div[1]/div/p') %>% 
            html_text()
          
          #extraction des différentes parties de l'adresse
          adress_div <- str_split(adress, ", ", simplify = TRUE)
          estado <- adress_div[length(adress_div)]
          municipio <- adress_div[length(adress_div)-1]
          colonia <- adress_div[length(adress_div)-2]
          
          #si aucune carte n'a été chargée, la forme du site change beaucoup et l'annonce n'est plus exploitable
          #il s'agit souvent d'annonces peu renseignées, l'adresse est souven manquante, ce qui crée des erreurs dans le code
          if(identical(colonia, character(0))){
            estado <- NA
            municipio <- NA
            colonia <- NA
          }
          
          #description du bien
          desc_good <- html_good %>% 
            html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[3]/div/p') %>% 
            html_text()
          
          #surface au sol (construction + jardin, parking, ...)
          #Problème avce les surfaces : 1 fois sur 2, la table n'est pas récuprée par l'url
          surface_grnd <- as.integer(str_remove(html_good %>% 
                                                  html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[2]/div/div[1]/table/tbody/tr[1]/td/span') %>% 
                                                  html_text(), " m²"))
          #surface totale du bien (sol + étages éventuels)
          surface_good <-  as.integer(str_remove(html_good %>% 
                                                   html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[2]/div/div[1]/table/tbody/tr[2]/td') %>% 
                                                   html_text(), " m²"))
          
          
          #test de la présence des éléménts dans  la table (surface, ...)
          nb_try = 0
          if(is.na(surface_good)){
            while(is.na(surface_good) & nb_try <= 10){
              #si le test échoue 10 fois, c'est que l'infomation est incomplète, on passe à la suite
              nb_try <- nb_try + 1
              print("An error has occured (surface searching), reloading the page")
              sleepDuration <- sample(4:8,1)
              print(paste("sleeping",sleepDuration,"seconds"))
              Sys.sleep(sleepDuration)
              
              #rechargement de la page
              try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
              
              #on teste si la page est bien chargée
              if(is.na(html_good)){
                while(is.na(html_good)){
                  print("An error has occured during the page loading, reloading the page")
                  sleepDuration <- sample(4:12,1)
                  print(paste("sleeping",sleepDuration,"seconds"))
                  Sys.sleep(sleepDuration)
                  try_default(html_good <- read_html(detailedListings[i]), html_good <- NA)
                }
                print(print("Connection success..."))
              }
              #surface au sol (construction + jardin, parking, ...)
              surface_grnd <- as.integer(str_remove(html_good %>% 
                                                      html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[2]/div/div[1]/table/tbody/tr[1]/td/span') %>% 
                                                      html_text(), " m²"))
              #surface totale du bien (sol + étages éventuels)
              surface_good <-  as.integer(str_remove(html_good %>% 
                                                       html_node(xpath='/html/body/main/div/div[4]/div/div[2]/div[2]/div[2]/div/div[1]/table/tbody/tr[2]/td') %>% 
                                                       html_text(), " m²"))
            }
            if(nb_try <= 10){print("Surface data successfully loaded")
            }else{print("Error while loading the surface datas, NAs will be introduced in result")}
            
          }
          
          #nombre de chambres
          nb_room <- html_good %>% 
            html_node(xpath='/html/body/main/div/div[4]/div/div[1]/div[1]/div/div[3]/div/div[2]/span') %>% 
            html_text()
          
          #nombre de salles de bain
          nb_bathroom <- html_good %>% 
            html_node(xpath='/html/body/main/div/div[4]/div/div[1]/div[1]/div/div[3]/div/div[3]/span') %>% 
            html_text()
          
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
          
          MC_data <- rbind(MC_data, data.frame(id_offer,adress,type,price,currency,surface_grnd,surface_good,nb_room,
                                               nb_bathroom,lat,lng,colonia,municipio,estado,desc_good,detailedListings[i]))
          
          #pause for a random duration between 4 and 12 seconds to stay stealth
          sleepDuration <- sample(4:12,1)
          print(paste("sleeping",sleepDuration,"seconds"))
          Sys.sleep(sleepDuration)
        }
        
        #url de la page suivante :
        url_page <- html_page %>% 
          html_node(xpath='/html/body/main/div/div[2]/section/div[8]/ul/li[3]/a') %>% 
          html_attr("href")
        
        #ecriture du fichier csv
        write.csv(MC_data,"metroscubicos.csv")
      }else{
        html_page <- NA
        sleepDuration <- sample(4:12,1)
        print(paste("sleeping",sleepDuration,"seconds"))
        Sys.sleep(sleepDuration)}
    }
  }
}


# 1 - Récupération des urls des municipios et types

nb_municipes <- nrow(df_municipios)
mun_range <- 1:nb_municipes

#types de biens vendus sur le site
l_type <- c("bodegas","casas","cuartos","departamentos","edificios","haciendas",
            "ranchos","oficinas","locales-comerciales","terrenos","otros")

#types de biens recherchés par l'utilisateur
l_type_wanted <- l_type

#site désiré : mercadolibre ou metroscubicos
url_base <- "https://inmuebles.mercadolibre.com.mx/"

#on parcourt la liste de tous les municipes
liste_urls_municipes_types <- tibble( url = character(), type = character(), estado = character(), municipio = character())

get_available_types <- function(html_content){
  l_titres_disp <- html_content %>% 
    html_nodes(xpath='/html/body/main/div/div[2]/aside/section[2]') %>% 
    html_children()
  filter_position <- l_titres_disp %>% html_text() %>% grep(pattern = "Inmueble", fixed = TRUE, value = FALSE)
  
  #types de biens disponibles dans la municipe
  l_type_avaliable <- html_content %>% 
    html_node(xpath=paste('/html/body/main/div/div[2]/aside/section[2]/div[',filter_position,']/ul')) %>% 
    html_children() %>%
    html_text() %>%
    tolower() %>%
    str_remove_all(pattern = "[([0-9]*)]")
  l_type_avaliable
  
  #création d'une liste contenant les types de biens voulus disponibles dans la municipe
  l_type_wanted_avaliable <- l_type_avaliable[l_type_avaliable %in% l_type_wanted]
  return(l_type_wanted_avaliable)
}

for (mun in mun_range){
  print(paste("Municipio",df_municipios[mun,2],":"))
  url_page_noType <- paste(url_base,"venta/",df_municipios[mun,3],"/",df_municipios[mun,2],"/",sep="")
  
  library(httr)
  html_page_noType <- httr::GET(url_page_noType, httr::timeout(5), user_agent("httr"))
  html_page_noType <- read_html(html_page_noType)
  
  l_type_wanted_avaliable <- get_available_types(html_page_noType)
  
  url_types <- glue::glue("{url_base}{l_type_wanted_avaliable}/venta/{df_municipios[mun,3]}/{df_municipios[mun,2]}/")
  
  this_row <- tibble(url = url_types, type = l_type_wanted_avaliable, estado = {df_municipios[mun,3]}, municipio = {df_municipios[mun,2]})
  liste_urls_municipes_types <- liste_urls_municipes_types %>%
    bind_rows(this_row)
  Sys.sleep(runif(n = 1, min = 4, max = 12))
}
write_csv(liste_urls_municipes_types, "tests_robin/liste_urls_municipes_types.csv")

# Municipios sans résultats à scraper :
df_municipios %>% filter(!municipio %in% liste_urls_municipes_types$municipio)
# => Ce sont 3 municipios où le filtre par type n'est pas disponible, avec chacun (très) peu d'annonces (1 à 7)

# 2 - Récupération des URLs des biens dans chaque page de municipios/type/index_de_page

# A - On charge chaque page

# B - On récupère les urls des annonces individuelles

# C - On regarde si il y a une page suivante

# D - On va sur cette page suivante

# B - C - (D)


foo <- httr::GET("https://inmuebles.mercadolibre.com.mx/venta/estado-de-mexico/chalco/",
                 user_agent("httr"), timeout(10))


all_announces <- c()
get_page_announces_urls <- function(get_page_announces_urls){
  thoses_announces <- html_content %>%
  html_nodes(xpath='//*[@class="ui-search-result__content ui-search-link"]')%>%
    html_attr("href") %>%
    str_split(pattern = "#", simplify = TRUE) %>%
    as.data.frame() %>%
    pull(1)
  all_announces <<- c(all_announces, thoses_announces)
  
  next_page_url <- html_content %>%
    html_node(xpath='/html/body/main/div/div[2]/section/div[8]/ul/li[3]/a') %>% 
    html_attr("href")
  
  if (!is.na(next_page_url)){
    print("There's a next page, waiting for sleep")
    Sys.sleep(runif(n = 1, min = 4, max = 12))
    print("Scraping next page")
    print(next_page_url)
    next_page_content <-  httr::GET(next_page_url,
                                    user_agent("httr"), timeout(10)) %>%
      read_html()
    get_page_announces_urls(next_page_content)
  }
}

get_page_announces_urls(foo %>% read_html())
