#Code réalisé part Louis Peller sur la base du code de Constance Barban, avec l'aide de Robin Cura

library(rvest)
library(stringr)
library(plyr)
library(xml2)
library(httr)

require(httr)

#package RSelenium : https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf

options(digits=16)

#INITIALISATION
MC_data <- NULL   #dataframe qui contient toutes les informations extraites
id <- 0

url_base <- 'https://tecnocasa.mx/busca-tu-casa.php'
url_base_good <- 'https://tecnocasa.mx/'
b_motivo = c('VENTA', 'RENTA')
b_estado = c('EDOMEX', 'CDMX') #seuls 3 municipes sont possibles pour l'étatd de mexico, tous situés dans la ZMVM

#on parcourt chaque mode de location
for(b_mot in b_motivo){
  #on parcourt chauqe état au sein desquels des offres sont disponibles
  for (b_est in b_estado){
    print(paste("Scrapping goods : mode =",b_mot,", estado =",b_est))
    cookies = c(
      'PHPSESSID' = 'eid453g47voobkmvolotto9lg0'
    )
    
    headers = c(
      `User-Agent` = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:108.0) Gecko/20100101 Firefox/108.0',
      `Accept` = '*/*',
      `Accept-Language` = 'fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3',
      `Accept-Encoding` = 'gzip, deflate, br',
      `Content-Type` = 'application/x-www-form-urlencoded; charset=UTF-8',
      `X-Requested-With` = 'XMLHttpRequest',
      `Origin` = 'https://tecnocasa.mx',
      `Connection` = 'keep-alive',
      `Referer` = 'https://tecnocasa.mx/busca-tu-casa.php',
      `Sec-Fetch-Dest` = 'empty',
      `Sec-Fetch-Mode` = 'cors',
      `Sec-Fetch-Site` = 'same-origin'
    )
    
    data = list(
      `busca_motivo` = b_mot,
      `busca_estado` = b_est,
      `busca_municipio_CDMX` = 'X',
      `busca_municipio_EDOMEX` = 'X',
      `busca_municipio_PUEBLA` = 'X',
      `busca_municipio_JALISCO` = 'X',
      `busca_colonia` = '',
      `busca_tipo_inmueble` = 'X',
      `busca_precio_min` = '',
      `busca_precio_max` = '',
      `busca_superficie` = '',
      `busca_dormitorio` = 'X',
      `busca_banos` = 'X',
      `busca_antiguedad` = 'X',
      `busca_estacionamiento` = 'X',
      `busca_nivel` = 'X',
      `resultados_por_pagina` = '10',
      `ordenar_resultados` = 'PRECIO|DESC',
      `pagina_busqueda` = '1',
      `tipo_busqueda` = 'busca_tu_casa'
    )
    
    #premiere requête rapide pour obtenir le nombre dd'offres disponibles
    res <- httr::POST(url = 'https://tecnocasa.mx/busca-tu-casa.php', httr::add_headers(.headers=headers),
                      httr::set_cookies(.cookies = cookies), body = data, encode = 'form')
    
    html_page <- read_html(res)
    
    nb_good <- xml_child(
      xml_child(
        xml_child(
          xml_child(
            html_page, 1), 1), 2), 1) %>% 
      html_text()
    
    data = list(
      `busca_motivo` = b_mot,
      `busca_estado` = b_est,
      `busca_municipio_CDMX` = 'X',
      `busca_municipio_EDOMEX` = 'X',
      `busca_municipio_PUEBLA` = 'X',
      `busca_municipio_JALISCO` = 'X',
      `busca_colonia` = '',
      `busca_tipo_inmueble` = 'X',
      `busca_precio_min` = '',
      `busca_precio_max` = '',
      `busca_superficie` = '',
      `busca_dormitorio` = 'X',
      `busca_banos` = 'X',
      `busca_antiguedad` = 'X',
      `busca_estacionamiento` = 'X',
      `busca_nivel` = 'X',
      `resultados_por_pagina` = as.character(nb_good),
      `ordenar_resultados` = 'PRECIO|DESC',
      `pagina_busqueda` = '1',
      `tipo_busqueda` = 'busca_tu_casa'
    )
    
    sleepDuration <- runif(n = 1, min = 4, max = 6)
    print(paste("sleeping",sleepDuration,"seconds"))
    Sys.sleep(sleepDuration)
    
    #deuxième requête pour obtenir toutes les offres sur une seule page
    res <- httr::POST(url = 'https://tecnocasa.mx/busca-tu-casa.php', httr::add_headers(.headers=headers),
                      httr::set_cookies(.cookies = cookies), body = data, encode = 'form')
    
    html_page <- read_html(res)
    for (id_good in 1:nb_good){
      id <- id + 1
      url_good <- xml_child(
        xml_child(
          xml_child(
            xml_child(
              xml_child(
                xml_child(
                  html_page, 1), 2), 1), id_good #valeur à parcourir sur la page des offres pour accéder aux biens
          ), 1), 1) %>% 
        html_attr("href")
      if (!is.na(url_good)){
        url_good <- paste(url_base_good, url_good, sep = "")
        print(paste("URL to see the offer :",url_good))
        #scrapping de la page du bien étudié :
        html_good <- read_html(url_good)
        
        #identifiant de l'offre sur le site
        id_offer <- html_good %>% 
          html_node(xpath = "/html/body/div/div[1]/div/div/div/div[1]/div[6]/div[1]/div[1]/div[3]") %>% 
          html_text()
        
        #adresse du bien
        adress <- html_good %>% 
          html_node(xpath="/html/body/div/div[1]/div/div/div/div[1]/div[6]/div[1]/div[1]/div[1]") %>% 
          html_text()
        
        #type de bien
        type <- html_good %>% 
          html_node(xpath = "/html/body/div/div[1]/div/div/div/div[1]/div[6]/div[1]/div[1]/div[2]") %>% 
          html_text()
        try_default(type <- substr(type, 0, str_locate(type, " - Col.")[1]-1), type <- type)
        
        #prix et monnaie du bien
        text_price <- gsub('[\t\n]','',html_good %>% 
                             html_node(xpath = '//*[@id="escaparate_detalles_precio"]') %>% 
                             html_text())
        price <- as.integer(gsub("[^0-9.]", "",text_price))
        currency <- gsub("[^A-Z.]", "",text_price)
        
        #description du bien
        desc_good <- html_good %>% 
          html_node(xpath = '/html/body/div/div[1]/div/div/div/div[1]/div[6]/div[2]') %>% 
          html_text()
        
        #mode (vente ou location)
        mode <- b_mot
        
        #état dans lequel est située l'offre
        estado <- b_est
        
        #détails de l'offre
        surface_good <- NA
        surface_grnd <- NA
        nb_bathroom <- NA
        nb_room <- NA
        municipio <- NA
        colonia <- NA
        
        detail <- html_good %>% 
          html_nodes(xpath='/html/body/div/div[1]/div/div/div/div[1]/div[8]/div[2]') %>% 
          html_children()
        
        for (det in detail){
          d <- substr(html_text(det), 0, str_locate(html_text(det), ': ')[1]-1)
          if (identical(d,"Superficie")){
            surface_good <- as.integer(gsub("[^0-9.]", "", html_text(det)))
          }
          if (identical(d,"Baños")){
            nb_bathroom <- as.integer(gsub("[^0-9.]", "", html_text(det)))
          }
          if (identical(d,"Recámaras")){
            nb_room <- as.integer(gsub("[^0-9.]", "", html_text(det)))
          }
          if (identical(d,"Delegación / Municipio")){
            municipio <- substr(html_text(det), str_locate(html_text(det), ': ')[2]+1, nchar(html_text(det)))
          }
          if (identical(d,"Colonia")){
            colonia <- substr(html_text(det), str_locate(html_text(det), ': ')[2]+1, nchar(html_text(det)))
          }
        }
        #coordonnées de l'offre
        lat <- NA
        lng <- NA
        #rvest ne premet pas de récupérer directement les scripts du code source de la page, qui contiennent les coordonnées
        #on récupère le texte brut du code de la page
        flat_html <- readLines(con = url_good)
        #on recherche dans le code brut la ligne contenant les informations de la localisation du centre du cercle indiquant le logement
        for (line in flat_html){
          if (startsWith(line, "\t\t\t\t\tcenter: ")){
            lat <- as.double(gsub("[^0-9.]", "", substr(line, str_locate(line, "lat:")[2]+1, str_locate(line, ", lng:")[1]-1)))
            lng <- as.double(gsub("[^0-9.-]", "", substr(line, str_locate(line, "lng:")[2]+1, str_locate(line, fixed("},"))[1])))
          }
        }
        
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
        
        MC_data <- rbind(MC_data, data.frame(id_offer,adress,type,price,currency,surface_grnd,surface_good,nb_room,nb_bathroom,lat,lng,colonia,municipio,estado,desc_good,url_good,mode))
        
        #pause for a random duration between 4 and 12 seconds to stay stealth
        sleepDuration <- runif(n = 1, min = 4, max = 10)
        print(paste("sleeping",sleepDuration,"seconds"))
        Sys.sleep(sleepDuration)
      }
    }
    write.csv(MC_data,"tecnocasa.csv")
  }
}
