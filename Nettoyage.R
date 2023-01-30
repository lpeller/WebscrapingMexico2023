library(stringr)
library(plyr)
library(dplyr)

unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


nettoyage <- function(site, file){
  if(site == "mercadolibre" | site == "metroscubicos"){
    df_metroscubicos <- read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
    
    count_init_mc <- nrow(df_metroscubicos)
    
    #ajout de la colonne "mode" qui définit si l'offre est une location ou une vente
    df_metroscubicos$mode <- NA
    df_metroscubicos$mode[endsWith(df_metroscubicos$type, "Venta")] <- "Venta"
    df_metroscubicos$mode[endsWith(df_metroscubicos$type, "Renta")] <- "Renta"
    
    #NETTOYAGE DES DONNEES
    
    #correction du probleme du nombre de salles de bains quand le nombre de chambres n'est pas renseigné (ex : natiment industriels)
    df_metroscubicos$nb_bathroom <- ifelse(endsWith(as.character(df_metroscubicos$nb_room), "baños")
                                           & !is.na(df_metroscubicos$nb_room),
                                           df_metroscubicos$nb_room, df_metroscubicos$nb_bathroom)
    df_metroscubicos$nb_room[endsWith(as.character(df_metroscubicos$nb_room), "baños")] <- NA
    
    #meme chose lorsqu'on est au singulier (1 salle de bain)...
    df_metroscubicos$nb_bathroom <- ifelse(endsWith(as.character(df_metroscubicos$nb_room), "baño")
                                           & !is.na(df_metroscubicos$nb_room),
                                           df_metroscubicos$nb_room, df_metroscubicos$nb_bathroom)
    df_metroscubicos$nb_room[endsWith(as.character(df_metroscubicos$nb_room), "baño")] <- NA
    
    #suppressions des "privado" et "privados" qui remplacent les chambres, souvent pour les caves et les "officina"
    df_metroscubicos$nb_room[endsWith(as.character(df_metroscubicos$nb_room), "privado")] <- NA
    df_metroscubicos$nb_room[endsWith(as.character(df_metroscubicos$nb_room), "privados")] <- NA
    
    #suppression des données avvichant un nombre imprécis de chambres/salles de baisn (ex: 2-4 recameras")
    df_metroscubicos$nb_room[grepl("-",df_metroscubicos$nb_room, fixed = TRUE)] <- NA
    df_metroscubicos$nb_bathroom[grepl("-",df_metroscubicos$nb_bathroom, fixed = TRUE)] <- NA
    
    #passage du nombre de chambres et de salles de bains en entiers
    df_metroscubicos$nb_room <- try_default(as.integer(gsub("[^0-9]","",df_metroscubicos$nb_room)),NA)
    df_metroscubicos$nb_bathroom <- try_default(as.integer(gsub("[^0-9]","",df_metroscubicos$nb_bathroom)),NA)
    
    df_metroscubicos[,"nb_bathroom"] <- as.numeric(as.character(df_metroscubicos[,"nb_bathroom"]))
    df_metroscubicos[,"nb_room"] <- as.numeric(as.character(df_metroscubicos[,"nb_room"]))
    
    df_metroscubicos$municipio <- chartr(paste(names(unwanted_array), collapse=''),
                                         paste(unwanted_array, collapse=''),df_metroscubicos$municipio)
    
    write.csv(df_metroscubicos, "Donnees_nettoyees/metroscubicos_clean.csv", fileEncoding = "UTF-8")
    
  }
  else if(site == "inmuebles24"){
    
    df_inmuebles <- read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
    
    count_init_mc <- nrow(df_inmuebles)
    
    #ajout de la colonne "mode" qui définit si l'offre est une location ou une vente
    df_inmuebles$mode <- "Venta"
    
    df_inmuebles$municipio <- chartr(paste(names(unwanted_array), collapse=''),
                                         paste(unwanted_array, collapse=''),df_inmuebles$municipio)
    
    write.csv(df_inmuebles, "Donnees_nettoyees/inmuebles24_clean.csv", fileEncoding = "UTF-8")
    
  }
  else if(site == "tecnocasa"){
    df_tecnocasa <- read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
    
    count_init_mc <- nrow(df_tecnocasa)
    
    
    df_tecnocasa$mode[endsWith(df_tecnocasa$mode, "VENTA")] <- "Venta"
    df_tecnocasa$mode[endsWith(df_tecnocasa$mode, "RENTA")] <- "Renta"
    
    
    df_tecnocasa$type[endsWith(df_tecnocasa$type, "DEPARTAMENTO")] <- "Departamento en Venta"
    df_tecnocasa$type[endsWith(df_tecnocasa$type, "CASA")] <- "Casa en Venta"
    df_tecnocasa$type[endsWith(df_tecnocasa$type, "TERRENO")] <- "Terreno en Venta"
    #NETTOYAGE DES DONNEES
    
    df_tecnocasa$desc_good[df_tecnocasa$desc_good == ""] <- NA
    
    df_tecnocasa$surface_grnd <- df_tecnocasa$surface_good
    
    df_tecnocasa$municipio <- chartr(paste(names(unwanted_array), collapse=''),
                                         paste(unwanted_array, collapse=''),df_tecnocasa$municipio)
    
    write.csv(df_tecnocasa, "Donnees_nettoyees/tecnocasa_clean.csv", fileEncoding = "UTF-8")
  }
  else if(site == "metroscubicos2015"){
    
    file = "donnes_2015/BD_ANNON_MC_2015.csv"
    df_metroscubicos <- read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE, sep = ";", dec = "," )
    
    df_met_2015 <- NULL
    df_met_2015$id_offer <- df_metroscubicos$alfaclave
    df_met_2015 <- as.data.frame(df_met_2015)
    
    df_met_2015$adress <- df_metroscubicos$calle
    df_met_2015$type <- df_metroscubicos$tipoinmueb
    
    df_met_2015$type <- case_when(
      startsWith(df_met_2015$type, "Casa") ~ "Casa en Venta",
      startsWith(df_met_2015$type, "Departamento") ~ "Departamento en Venta"
    )
    
    df_met_2015$price <- df_metroscubicos$preciov
    df_met_2015$currency <- df_metroscubicos$monedav
    df_met_2015$currency <- ifelse(df_met_2015$currency == "P", "$", "U$S")
    df_met_2015$surface_grnd <- df_metroscubicos$m2cons
    df_met_2015$surface_good <- df_metroscubicos$m2terr
    
    df_met_2015$surface_grnd[df_met_2015$surface_grnd == 0] <- NA
    df_met_2015$surface_good[df_met_2015$surface_good == 0] <- NA
    
    df_met_2015$nb_room <- df_metroscubicos$recamaras
    df_met_2015$nb_bathroom <- df_metroscubicos$banos
    df_met_2015$lat <- df_metroscubicos$lat
    df_met_2015$lng <- df_metroscubicos$lng
    df_met_2015$colonia <- df_metroscubicos$colonia
    df_met_2015$municipio <- df_metroscubicos$municipio
    
    df_met_2015$municipio[df_met_2015$municipio == "Gustavo A. Madero"] <- "Gustavo A Madero"
    df_met_2015$municipio[df_met_2015$municipio == "Ecatepec"] <- "Ecatepec de Morelos"
    
    df_met_2015$estado <- df_metroscubicos$estado
    df_met_2015$desc_good <- df_metroscubicos$alfaclave
    df_met_2015$lien <- NA
    df_met_2015$mode <- "Venta"
    
    df_met_2015$municipio <- chartr(paste(names(unwanted_array), collapse=''),
                                         paste(unwanted_array, collapse=''),df_met_2015$municipio)
    
    write.csv(df_met_2015, "Donnees_nettoyees/metroscubicos2015_clean.csv", fileEncoding = "UTF-8")
  }
}

#nettoyage des données scrappées
#
#site : string du nom du site d'origine :
#valeurs possibles : "mercadolibre", "metroscubicos", "inmuebles24", "tecnocasa","metroscubicos2015"
#file : fichier contenant les données en sortie du webscraping

nettoyage(site = "mercadolibre", file = "metroscubicos_venta_partiel.csv")

nettoyage(site = "metroscubicos2015", file = "donnes_2015/BD_ANNON_MC_2015.csv")


