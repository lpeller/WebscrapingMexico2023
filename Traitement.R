library(stringr)
library(plyr)
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(tidyr)
library(viridis)
library(RColorBrewer)

unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

#récupératiuon des données nettoyées
df_metroscubicos <- read.csv("Donnees_nettoyees/metroscubicos_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
df_tecnocasa <- read.csv("Donnees_nettoyees/tecnocasa_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

df_metroscubicos2015 <- read.csv("Donnees_nettoyees/metroscubicos2015_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)


#récupération des couches shp
contour_colonias <- read_sf("Colonias/colonias.shp") %>% 
  st_transform(contour_colonias, crs = 6372)

contour_ageb <- read_sf("AGEB/AGEB_2020.shp") %>% 
  st_transform(contour_ageb, crs = 6372)

contour_mun <- read_sf("Municipios/ZMVM_MUN_2020.shp") %>% 
  st_transform(contour_mun, crs = 6372)
contour_mun$NOMGEO <- toupper(contour_mun$NOMGEO)
contour_mun$NOMGEO <- ifelse(startsWith(contour_mun$NOMGEO, "GUSTAVO A. MADERO"), 
                     "GUSTAVO A MADERO",contour_mun$NOMGEO)
dfnoms_colonias <- contour_colonias[,c("SETT_NAME", "MUN_NAME")] %>% st_drop_geometry()


contour_colonias$MUN_NAME <- chartr(paste(names(unwanted_array), collapse=''),
                                    paste(unwanted_array, collapse=''),contour_colonias$MUN_NAME)

contour_mun$NOMGEO <- chartr(paste(names(unwanted_array), collapse=''),
                                    paste(unwanted_array, collapse=''),contour_mun$NOMGEO)


df_metroscubicos2015_p <- df_metroscubicos2015[df_metroscubicos2015$municipio %in% c("Cuauhtemoc", "Gustavo A Madero", "Ecatepec de Morelos", "Tecamac", "Temascalapa", "Benito Juarez", "Zumpango"),]


################# FONCTIONS DE TRAITEMENT DES DONNEES #################

traitement <-function(df, mode, tolerance_dist, source, keep_false = TRUE, echelle = "MUN"){
  #Entrées :
  #
  #df : dataframe contenant les offres extraites
  #mode : mode de paiement des offres. "V" pour vente, "R" pour une location
  #tolerance_dist : distance de tolérance pour la localisation des offres dans les municipes
  #source : nom du site web à la source des données (utile seulement pour nommer les fichiers de sortie)
  #keep_false : TRUE si l'on veut conserver les données mal localisées (avec un champ test_sit qui vaut TRUE si la donnée est bien localisée, FALSE sinon)
  #           , FALSE sinon
  #echelle : echelle à laquelle on veut travailler : "MUN" pour les municipes, "COL" pour les colonies
  #
  #Sortie :
  #
  #Objet de type sf contenant les données traitées et localisées, écriture d'un fichier CSV et d'un SHP
  
  
  #selection du mode de paiement voulu
  df <- df[grepl(mode, df$mode),]
  nb_init <- nrow(df)
  #traitement des surfaces aberrantes : on enlève les logements pour lesquels la surface est de 1m²
  df_clean <- df[
    (df$surface_grnd > 1 & df$surface_good > 1 &
       !is.na(df$surface_grnd) & !is.na(df$surface_good)) |
      (is.na(df$surface_grnd) & df$surface_good > 1 &
         !is.na(df$surface_good)) |
      (df$surface_grnd > 1 & is.na(df$surface_good) &
         !is.na(df$surface_grnd)), ]
  

  #suppression des données qui n'ont pas de lat lng
  df_clean <- df_clean[!is.na(df_clean$lat) | !is.na(df_clean$lng),]

  #suppression des données sans prix
  df_clean <- df_clean[!is.na(df_clean$price),]

  #suppression des données sans surfaces
  df_clean <- df_clean[!is.na(df_clean$surface_grnd),]

  #conversion des prix en Pesos (MXN) pour les prix en $US : 1USD = 19.1425MXN au 10/01/2022 (source : https://www.boursorama.com/bourse/devises/taux-de-change-dollar-pesodumexique-USD-MXN/)
  changeUSD_MXN <- 19.1425
  df_clean$price <- ifelse(grepl("U$S",df_clean$currency, fixed = TRUE), df_clean$price* changeUSD_MXN, df_clean$price)
  df_clean$price <- ifelse(grepl("USD",df_clean$currency, fixed = TRUE), df_clean$price* changeUSD_MXN, df_clean$price)
  
  #seuils de prix vente/location bas
  s_venta = 100000
  s_renta = 100
  
  #suppression des logements à prix trop faible
  if(identical(mode,"V")){
    df_clean <- df_clean[df_clean$price >= s_venta,]
  }else{
    df_clean <- df_clean[df_clean$price >= s_renta,]
  }

  #suppression des doublons
  df_clean <- df_clean %>% distinct(id_offer, .keep_all = TRUE)

  #suppression des offres qui ont la meme description, le meme municipe et la meme surface
  df_clean <- df_clean %>%
    distinct(desc_good,municipio,surface_grnd, colonia,.keep_all = TRUE)

  #supression des coordonnées aberrantes
  df_clean <- df_clean[(18.9 < df_clean$lat)&(20.1> df_clean$lat)&(-99.6 < df_clean$lng)&(-98.4> df_clean$lng),]
  #formatage des noms des municipes et des colonies - certaines colonies ont un nom indiqué qui ne correspond pas au nom officiel
  df_clean <- format_noms_col(df_clean)

  #suppression des surfaces aberrantes (surfaces trop grandes, qui peuvent fausser les résultats)
  df_clean <- df_clean[df_clean$surface_grnd < 100000000,]
  
  nb_final = nrow(df_clean)
  print("AVANT VALIDATION GEOGRAPHIQUE :")
  print(paste(nb_init,"offres avant traitement,",nb_final,"offres après traitement"))
  print(paste(nb_init-nb_final,"offres supprimées, soit",(nb_init-nb_final)/nb_init*100,"%"))
  
  #validation géographique
  if (identical(echelle, "MUN")){
    df_clean <- validation_geo_mun(df_clean, tolerance_dist, contour_mun, keep_false = keep_false)
  }else if (identical(echelle, "COL")){
    df_clean <- validation_geo_col(df_clean, tolerance_dist, contour_colonias, keep_false = keep_false)
  }
  #ecriture d'un fichier csv
  st_write(df_clean, paste("Donnees_traitees/",source,"_offers_",mode,".csv", sep = ""), layer_options = c("GEOMETRY=AS_XY", "ENCODING=UTF-8"),delete_dsn = T)
  
  #ecriture d'un fichier shp
  st_write(df_clean, paste("SHP_Traites/",source,"_",mode,"clean",".shp",sep = ""),layer_options = "ENCODING=UTF-8",delete_dsn = T)
  
  print(paste("Au total, ",(nb_init-nrow(df_clean))/nb_init*100,"% des offres ont été supprimées.",sep = ""))
  return(df_clean)
}

format_noms_col <- function(df){
  #passage en majuscules des noms de municipes et de colonies
  df$municipio <- toupper(df$municipio)
  df$colonia <- toupper(df$colonia)
  
  #renommer certains municipes/colonies
  df$municipio <- ifelse(grepl("GUSTAVO A. MADERO",df$municipio, fixed = TRUE), "GUSTAVO A MADERO", df$municipio)
  df$colonia <- ifelse(grepl("AMPLIACIÓN",df$colonia, fixed = TRUE), 
                       paste("AMPL ",substr(df$colonia, 12, nchar(df$colonia)),sep = ""),
                       df$colonia)
  df$colonia <- chartr(paste(names(unwanted_array), collapse=''),
                       paste(unwanted_array, collapse=''),df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "CENTRO DE LA CIUDAD DE MEXICO"), 
                       "CENTRO",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "COLONIA DEL VALLE"), 
                       "DEL VALLE",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "DEL VALLE NORTE"), 
                       "DEL VALLE",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "HIPODROMO CONDESA"), 
                       "HIPODROMO DE LA CONDESA",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "JARDINES DE MORELOS"), 
                       "JARDINES DE MORELOS",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "LINDAVISTA VALLEJO"), 
                       "UNIDAD LINDAVISTA VALLEJO",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "LINDAVISTA NORTE"), 
                       "LINDAVISTA",df$colonia)
  df$colonia <- ifelse(startsWith(df$colonia, "VILLA GUSTAVO A. MADERO"), 
                      "GUSTAVO A MADERO",df$colonia)
  #df <- df[(df$colonia %in% n_colonias),]
  df <- df[!is.na(df$municipio),]
  
  return(df)
}

validation_geo_mun <- function(df, tolerance_dist, contours, keep_false = TRUE){
  #fonction qui ajoute un champ test_sit à une liste d'offres, et le remplis TRUE si l'offre est située à une distance donnée du 
  #municipe dans lequel elle est censée se trouver, FALSE sinon
  #l'argument keep_false permet de choisir si l'on veut conserver les données mal localisées (avec un argument FALSE pour le champ test_dis)
  #         si keep_false == TRUE, alors on garde ces données
  #         si keep_false == FALSE, on les supprime
  
  #champ de test de distance
  df$test_sit <- FALSE
  
  #création d'un éléménet de type sf
  df_sf <- st_as_sf(df, coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(df_sf, crs = 6372)
  
  output_data <- NULL
  
  for (i in 1:nrow(contours)){
    #parcours des noms des municipe
    nom_mun <- contours[i,]$NOMGEO
    #selection des données indiquées dans l'offre comme étant dans le municipe
    df_sf_i <- df_sf[df_sf$municipio == nom_mun,]
    
    #remplissage du champ mun_reel, qui contient le nom du municipe dans lequel l'offre est réellement localiséee
    df_sf_i$mun_reel <- apply(st_is_within_distance(contours[i,], df_sf_i, dist = tolerance_dist,sparse = FALSE), 2, 
                            function(col) {contours[i,][which(col), ]$NOMGEO})
    #remplissage du champ test_sit, qui vaut TRUE si le municipe indiqué dans l'annonce est celui dans lequel le logement est réellement situé
    df_sf_i$test_sit <- ifelse(df_sf_i$municipio == df_sf_i$mun_reel, TRUE, FALSE)
    output_data <- rbind(output_data, df_sf_i)
  }
  output_non_lim <- output_data [output_data$test_sit == FALSE,]
  print("VALIDATION GEOGRAPHIQUE")
  print(paste("Pour ",nrow(df_sf)," offres initiales, ",nrow(df_sf)-nrow(output_non_lim)," sont localisées dans le bon Municipe (à distance au plus de ",tolerance_dist," m)", sep = ""))
  print(paste(nrow(output_non_lim)," offres ont été supprimées au total, soit ",(nrow(output_non_lim))/nrow(df_sf)*100," %", sep = ""))
  if(keep_false == FALSE){
    #suppression des données mal localisées
    output_data <- output_data [output_data$test_sit == TRUE,]
  }
  return(output_data)
}

validation_geo_col <- function(df, tolerance_dist, contours, keep_false = TRUE){
  #fonction qui ajoute un champ à une liste d'offres, et le remplis TRUE si l'offre est située à une distance donnée de la
  #colonie dans lequel elle est censée se trouver, FALSE sinon
  #l'argument keep_false permet de choisir si l'on veut conserver les données mal localisées (avec un argument FALSE pour le champ test_dis)
  #         si keep_false == TRUE, alors on garde ces données
  #         si keep_false == FALSE, on les supprime
  
  #noms des colonies pour chauqe municipe
  names_colonias <- contours[,c("SETT_NAME", "MUN_NAME")] %>%
    st_drop_geometry() %>%
    group_by(MUN_NAME) %>%
    summarise(SETT_NAME = list(SETT_NAME))
  
  #champ de test de distance
  df$test_sit <- FALSE
  
  #champ de test si la colonie indiquée est reconnue
  df$test_colonie <- FALSE
  df$col_reel <- FALSE
  
  #création d'un éléménet de type sf
  df_sf <- st_as_sf(df, coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(df_sf, crs = 6372)
  
  output_data <- NULL
  #le but de la boucle est de parcourir toutes les colonies, et, pour les offres indiéues comme étant situées dans la colonie, de vérifier la localisation
  #parcours des noms des municipes
  for (i in 1:nrow(names_colonias)){
    l_col <- names_colonias[i,]$SETT_NAME
    mun <- names_colonias[i,]$MUN_NAME
    #parcours des noms des colonies
    for (j in 1:length(l_col[[1]])){
      col <- l_col[[1]][j]
      
      #logement indiqués comme étant dans la colonie
      df_sf_i <- df_sf[df_sf$colonia == col & df_sf$municipio == mun,]
      
      if(nrow(df_sf_i)>0){
        
        #la colonie indiquée existe bien
        df_sf$test_colonie <- TRUE
        geom_col <- contours[contours$SETT_NAME == col & contours$MUN_NAME == mun,]
        #on teste si la colonie est située dans la colonie indiquée
        if (any(st_is_within_distance(geom_col, df_sf_i, dist = tolerance_dist,sparse = FALSE))){
          #le champ col_reel contient le nom de la colonie dans laquelle l'offre est réellement située
          df_sf_i$col_reel <- apply(st_is_within_distance(geom_col, df_sf_i, dist = tolerance_dist,sparse = FALSE), 2,
                                    function(var) {geom_col[which(var), ]$SETT_NAME})
          #remplissage du cham test_sit, qui vaut TRUE si le nom d ela colonie indiquée sur l'offre est identique à celui dans lequel elle est réellement située
          df_sf_i$test_sit <- ifelse(df_sf_i$colonia == df_sf_i$col_reel, TRUE, FALSE)
        }
        output_data <- rbind(output_data, df_sf_i)
      }
    }
  }
  #remplissage du nom d ela coonie réelle pour toutes les offres
  output_data$col_reel <- apply(st_intersects(contour_colonias, output_data, sparse = FALSE), 2,
                                function(col) {contour_colonias[which(col), ]$SETT_NAME})
  #output_non_lim <- output_data [output_data$test_sit == FALSE,]
  
  print("VALIDATION GEOGRAPHIQUE")
  n_init <- nrow(df_sf)
  n_final <- nrow(output_data [output_data$test_sit == TRUE,])
  print(paste("Pour ",n_init," offres initiales, ",n_final," sont localisées dans la bonne colonie (à distance au plus de ",tolerance_dist," m)", sep = ""))
  print(paste(n_init-n_final," offres ont été supprimées au total, soit ",(n_init-n_final)/n_init*100," %", sep = ""))
  if(keep_false == FALSE){
    output_data <- output_data [output_data$test_sit == TRUE,]
  }
  
  return(output_data)
}

df_MC_V_clean <- traitement(df_metroscubicos, "V", 500, "mercadolibre", keep_false = FALSE, echelle = "MUN")
df_TC_V_clean <- traitement(df_tecnocasa, "V", 500, "tecnocasa", keep_false = FALSE, echelle = "MUN")

df_MC2015_V_clean <- traitement(df_metroscubicos2015_p, "V", 500, "mercadolibre2015", keep_false = FALSE, echelle = "MUN")

df_MC2015_V_complete <- traitement(df_metroscubicos2015, "V", 500, "mercadolibre2015", keep_false = FALSE, echelle = "MUN")

################# FONCTIONS DE VISUALISATION ET D'AFFICHAGE ################# 



#GRILLE
reduire_grille <- function(grille, data, size, size_min, nb_min = 1){
  #fonction récursive permettant de créer des sous mailles dans une grille, si la maille possède plsu de 20 offres
  #séparation des mailles contenant plus de 20 offres des autres
  grille_s20 <- filter(grille, nb_offers >= 20)
  grille_m20<- filter(grille, nb_offers < 20)
  #création d'une grille de maille de taille = size
  n_grille <- st_make_grid(grille_s20, cellsize = size, crs = 6372) 
  #ajout des id à chaque maille
  n_grille <- st_sf(n_grille) %>% 
    mutate(grid_id = 1:length(lengths(n_grille)))
  #renommer la géométrie, afin d'assurer la compatibilité de la nouvelle grille avec celle de départ
  st_geometry(n_grille) <-  "grid_init"
  #comptage du nombre de points dans chaque maille
  n_grille$nb_offers = lengths(st_intersects(n_grille, data))
  #supression des mailles qui ne sont pas situées dans le mailllage principal
  n_grille = n_grille[n_grille$grid_id %in% Reduce(c,st_contains(grille_s20, n_grille)),]
  #si la taille des mailles est supérieure à celle du maillage minimal, alors on applique de nouveau l'algorithme, à un maillage deux fois plus petit
  if (size > size_min){
    n_grille <- reduire_grille(n_grille, data, size/2, size_min, nb_min)
  }
  grille_fin <- rbind(grille_m20,n_grille)
  
  # suppression des mailles qui ne contiennent pas assez de points
  grille_fin = filter(grille_fin, nb_offers >= nb_min)
  
  return(grille_fin)
}

creer_grille <- function(contour, offres, size_max, size_min, nb_min = 1){
  #Fonction permettant la création d'une grille contenant les informations des offres situées dans les carreaux
  #
  #ENTREES :
  #contour : objet sf conteant les géométries des municipes
  #offres : dataframe sf contenant les offres de biens préalablement nettoyées
  #size_max : taille maximale d'une maille de la grille en m
  #size_min : taille de la plus petite maille voulue.
  #     si size_min = size_max, la grille sera une grille simple, avec des mailles de même taille
  #     si size_min < size_max, la grille sera composée de mailles de différentes tailles (voir reduire_grille) : chaque maille sera divisée en 4 si elle contient plus de 20 offres
  #nb_min : nombre minimum de logements pour qu'une maille soit retenue
  #
  #SORTIE :
  #objet sf, contenant les informations de toutes les mailles
  
  #ajout d'un ID à chanque offre
  offres$ID <- seq.int(nrow(offres))
  #création d'une première grille
  grid_init <- st_make_grid(contour, cellsize = size_max, crs = 6372)
  sf_grid_init = st_sf(grid_init) %>%
    # ajout d'un ID pour chaque maille
    mutate(grid_id = 1:length(lengths(grid_init)))
  #comptage du nombre de points dans la grille
  sf_grid_init$nb_offers = lengths(st_intersects(sf_grid_init, offres))
  # suppression des mailles dont le nombre d'offres est trop faible
  sf_grid_init = filter(sf_grid_init, nb_offers >= nb_min)
  
  g = 3   #position de la colonne de geometrie
  if (size_min < size_max){
    #création de sous mailles pour les mailles de la grille principale
    n_grille <- reduire_grille(sf_grid_init, offres, size_max/2, size_min, nb_min)}
  else{
    n_grille <- sf_grid_init
    g = 1
  }
  #ajout, pour chaque maille, des id des offres qui sont localisées dans la maille
  n_grille$id_offers <- st_intersects(n_grille, offres)
  n_grille$avg_price <- NA
  n_grille$avg_surf <- NA
  n_grille$area <- NA
  n_grille$part_ter <- NA
  n_grille$part_dep <- NA
  n_grille$part_cas <- NA
  #cette boucle parcourt chaque maille de la grille, afin de calculer les différentes valeurs moyennes
  for (i in 1:length(n_grille$id_offers)){
    count <- length(n_grille$id_offers[[i]])
    #calcul de la valeur moyenne des prix
    avg_price = mean(offres[offres$ID %in% n_grille$id_offers[[i]],]$price)
    n_grille[[5]][i] <- avg_price
    #calcul de la surface moyenne dans la maille
    avg_surf = mean(offres[offres$ID %in% n_grille$id_offers[[i]],]$surface_grnd)
    n_grille[[6]][i] <- avg_surf
    
    n_grille[[7]][i] <- st_area(n_grille[[g]][i])
    
    offres_ter <- offres[offres$type == "Terreno en Venta",]
    offres_dep <- offres[offres$type == "Departamento en Venta",]
    offres_cas <- offres[offres$type == "Casa en Venta",]
    
    n_grille[[8]][i] <- lengths(st_intersects(n_grille[[g]][i], offres_ter))
    n_grille[[9]][i] <- lengths(st_intersects(n_grille[[g]][i], offres_dep))
    n_grille[[10]][i] <- lengths(st_intersects(n_grille[[g]][i], offres_cas))
    }
  #suppression de la colonne inutile id_offers
  n_grille <- n_grille %>% select(-c(id_offers))
  #calcul du prix au m² d'un bien
  n_grille$price_m2 <- n_grille$avg_price / n_grille$avg_surf
  #calcul du nombre d'offres au km² dans la zone
  n_grille$offers_km2 <- n_grille$nb_offers / n_grille$area * 1000000
  
  n_grille$part_ter <- n_grille$part_ter / n_grille$nb_offers
  n_grille$part_dep <- n_grille$part_dep / n_grille$nb_offers
  n_grille$part_cas <- n_grille$part_cas / n_grille$nb_offers
  
  return(n_grille)
}

scale_fill_viridis_opt <- function(x){
  #créer une échelle pour un gradient de couleurs (obsolète)
  x <- sort(unique(x))
  x <- (x[-1] + x[-length(x)])/2
  y <- (x - min(x))/diff(range(x))
  scale_fill_gradientn(values = y, colours = viridis::magma(length(x)))
}  

gen_quantile <- function(var, nb_classes = 6){
  #génération de bornes de quantiles pour l'affichage
  return(quantile(var, probs=seq(0, 1, 1/nb_classes)))
}

class_values <- function(data, value = "avg_price", nb_min = 1, nb_classes = 6, source = NULL){
  #fonction permettant de formater l'affichage, afin d'obtenir des données propres
  #
  #ENTRES :
  #data : dataframe contenant les géométries créées (grille ou éléments géographiques)
  #value : string indiquant le nom de la variable étudiée
  #nb_classes : nombre de classes voulues pour la discrétisation
  #
  #SORTIE
  #ajout d'une colonne class, qui permet de discrétiser selon la valeur choisie
  
  if(is.null(source)){
    source <- data
  }
  
  quant =case_when (
    value == "avg_price" ~ gen_quantile(source$avg_price, nb_classes),
    value == "avg_surf" ~ gen_quantile(source$avg_surf, nb_classes),
    value == "nb_offers" ~ gen_quantile(source$nb_offers, nb_classes),
    value == "price_m2" ~ gen_quantile(source$price_m2, nb_classes),
    value == "offers_km2" ~ gen_quantile(source$offers_km2, nb_classes),
    value == "part_ter" ~ gen_quantile(source$part_ter, nb_classes),
    value == "part_dep" ~ gen_quantile(source$part_dep, nb_classes),
    value == "part_cas" ~ gen_quantile(source$part_cas, nb_classes)
  )
  if(is.null(data$class)){
    data$class <- NA
  }
  
  if(value == "avg_price"){
    quant <- round(quant,-2)
    data$class = case_when(
      data$avg_price < quant[2] ~ paste("1. Moins de ",quant[2]," $", sep=""),
      quant[2] <= data$avg_price & data$avg_price < quant[3] ~ paste("2. Entre ",quant[2]," et ",quant[3], " $",sep=""),
      quant[3] <= data$avg_price & data$avg_price < quant[4] ~ paste("3. Entre ",quant[3]," et ",quant[4], " $",sep=""),
      quant[4] <= data$avg_price & data$avg_price < quant[5] ~ paste("4. Entre ",quant[4]," et ",quant[5], " $",sep=""),
      quant[5] <= data$avg_price & data$avg_price < quant[6] ~ paste("5. Entre ",quant[5]," et ",quant[6], " $",sep=""),
      data$avg_price >= quant[6] ~ paste("6. Plus de ",quant[6]," $", sep="")
    )
  }else if(value == "avg_surf"){
    quant <- round(quant,-2)
    data$class = case_when(
      data$avg_surf < quant[2] ~ paste("1. Moins de ",quant[2]," m²", sep=""),
      quant[2] <= data$avg_surf & data$avg_surf < quant[3] ~ paste("2. Entre ",quant[2]," et ",quant[3], " m²",sep=""),
      quant[3] <= data$avg_surf & data$avg_surf < quant[4] ~ paste("3. Entre ",quant[3]," et ",quant[4], " m²",sep=""),
      quant[4] <= data$avg_surf & data$avg_surf < quant[5] ~ paste("4. Entre ",quant[4]," et ",quant[5], " m²",sep=""),
      quant[5] <= data$avg_surf & data$avg_surf < quant[6] ~ paste("5. Entre ",quant[5]," et ",quant[6], " m²",sep=""),
      data$avg_surf >= quant[6] ~ paste("6. Plus de ",quant[6]," m²", sep="")
    )
  }else if (value == "nb_offers"){
    quant <- round(quant,0)
    data$class = case_when(
      data$nb_offers < quant[2] ~ paste("1. Entre ",quant[1]," et ",quant[2]," offres", sep=""),
      quant[2] <= data$nb_offers & data$nb_offers < quant[3] ~ paste("2. Entre ",quant[2]," et ",quant[3], " offres",sep=""),
      quant[3] <= data$nb_offers & data$nb_offers < quant[4] ~ paste("3. Entre ",quant[3]," et ",quant[4], " offres",sep=""),
      quant[4] <= data$nb_offers & data$nb_offers < quant[5] ~ paste("4. Entre ",quant[4]," et ",quant[5], " offres",sep=""),
      quant[5] <= data$nb_offers & data$nb_offers < quant[6] ~ paste("5. Entre ",quant[5]," et ",quant[6], " offres",sep=""),
      data$nb_offers >= quant[6] ~ paste("6. Plus de ",quant[6]," offres", sep="")
    )
  }else if(value == "price_m2"){
    quant <- round(quant,-2)
    data$class = case_when(
      data$price_m2 < quant[2] ~ paste("1. Moins de ",quant[2]," $/m²", sep=""),
      quant[2] <= data$price_m2 & data$price_m2 < quant[3] ~ paste("2. Entre ",quant[2]," et ",quant[3], " $/m²",sep=""),
      quant[3] <= data$price_m2 & data$price_m2 < quant[4] ~ paste("3. Entre ",quant[3]," et ",quant[4], " $/m²",sep=""),
      quant[4] <= data$price_m2 & data$price_m2 < quant[5] ~ paste("4. Entre ",quant[4]," et ",quant[5], " $/m²",sep=""),
      quant[5] <= data$price_m2 & data$price_m2 < quant[6] ~ paste("5. Entre ",quant[5]," et ",quant[6], " $/m²",sep=""),
      data$price_m2 >= quant[6] ~ paste("6. Plus de ",quant[6]," $/m²", sep="")
    )
  }else if(value == "offers_km2"){
    quant <- round(quant,0)
    data$class = case_when(
      data$offers_km2 < quant[2] ~ paste("1. Moins de ",quant[2]," offres/km²", sep=""),
      quant[2] <= data$offers_km2 & data$offers_km2 < quant[3] ~ paste("2. Entre ",quant[2]," et ",quant[3], " offres/km²",sep=""),
      quant[3] <= data$offers_km2 & data$offers_km2 < quant[4] ~ paste("3. Entre ",quant[3]," et ",quant[4], " offres/km²",sep=""),
      quant[4] <= data$offers_km2 & data$offers_km2 < quant[5] ~ paste("4. Entre ",quant[4]," et ",quant[5], " offres/km²",sep=""),
      quant[5] <= data$offers_km2 & data$offers_km2 < quant[6] ~ paste("5. Entre ",quant[5]," et ",quant[6], " offres/km²",sep=""),
      data$offers_km2 >= quant[6] ~ paste("6. Plus de ",quant[6]," offres/km²", sep="")
    )
  }else if(value == "part_ter"){
    quant <- round(quant,4)
    data$class = case_when(
      data$part_ter < quant[2] ~ paste("1. Moins de ",quant[2]*100," % de terrains", sep=""),
      quant[2] <= data$part_ter & data$part_ter < quant[3] ~ paste("2. Entre ",quant[2]*100," % et ",quant[3]*100, " % de terrains",sep=""),
      quant[3] <= data$part_ter & data$part_ter < quant[4] ~ paste("3. Entre ",quant[3]*100," % et ",quant[4]*100, " % de terrains",sep=""),
      quant[4] <= data$part_ter & data$part_ter < quant[5] ~ paste("4. Entre ",quant[4]*100," % et ",quant[5]*100, " % de terrains",sep=""),
      quant[5] <= data$part_ter & data$part_ter < quant[6] ~ paste("5. Entre ",quant[5]*100," % et ",quant[6]*100, " % de terrains",sep=""),
      data$part_ter >= quant[6] ~ paste("6. Plus de ",quant[6]*100," % de terrains", sep="")
    )
  }else if(value == "part_dep"){
    quant <- round(quant,4)
    data$class = case_when(
      data$part_dep < quant[2] ~ paste("1. Moins de ",quant[2]*100," % d'appartements", sep=""),
      quant[2] <= data$part_dep & data$part_dep < quant[3] ~ paste("2. Entre ",quant[2]*100," % et ",quant[3]*100, " % d'appartements",sep=""),
      quant[3] <= data$part_dep & data$part_dep < quant[4] ~ paste("3. Entre ",quant[3]*100," % et ",quant[4]*100, " % d'appartements",sep=""),
      quant[4] <= data$part_dep & data$part_dep < quant[5] ~ paste("4. Entre ",quant[4]*100," % et ",quant[5]*100, " % d'appartements",sep=""),
      quant[5] <= data$part_dep & data$part_dep < quant[6] ~ paste("5. Entre ",quant[5]*100," % et ",quant[6]*100, " % d'appartements",sep=""),
      data$part_dep >= quant[6] ~ paste("6. Plus de ",quant[6]*100," % d'appartements", sep="")
    )
  }else if(value == "part_cas"){
    quant <- round(quant,4)
    data$class = case_when(
      data$part_cas < quant[2] ~ paste("1. Moins de ",quant[2]*100," % de maisons", sep=""),
      quant[2] <= data$part_cas & data$part_cas < quant[3] ~ paste("2. Entre ",quant[2]*100," % et ",quant[3]*100, " % de maisons",sep=""),
      quant[3] <= data$part_cas & data$part_cas < quant[4] ~ paste("3. Entre ",quant[3]*100," % et ",quant[4]*100, " % de maisons",sep=""),
      quant[4] <= data$part_cas & data$part_cas < quant[5] ~ paste("4. Entre ",quant[4]*100," % et ",quant[5]*100, " % de maisons",sep=""),
      quant[5] <= data$part_cas & data$part_cas < quant[6] ~ paste("5. Entre ",quant[5]*100," % et ",quant[6]*100, " % de maisons",sep=""),
      data$part_cas >= quant[6] ~ paste("6. Plus de ",quant[6]*100," % de maisons", sep="")
    )
  }
  
  #catégorie spéciale pour les cases dont le nombre d'infos est trop faible
  data$class <- ifelse(data$nb_offers < nb_min, "0. Pas assez de valeurs", data$class)
  return(data)
}

gen_colors <- function(data, pal = "YlOrBr", seuil_nb_offers = TRUE){
  rep = 1
  if(seuil_nb_offers == FALSE){
    return(colorRampPalette(brewer.pal(8, pal))(length(unique(data$class))))
  }else{
    return(c("gray",colorRampPalette(brewer.pal(8, pal))(length(unique(data$class))-1)))
  }
}



#Pour créer une visualisation : 
#     1. Créer une grille avec la fonction creer_grille(contour de la zone géographique, données traitées, taille de la plus grande maille, taille de la plus petite maille)
#         * si la taille de la plus petite maille est égale à celle de la plus grade, on génère une grille simple
#
#     2. Classer les valeurs en quantiles avec la fonction class_values(données a classer, valeur a classer, nb min d'offres voulu par carreau, nb de classes, source de la discretisation si différents de la donnée)
#         * la source de discrétisation permet de représenter les données avcec une discrétisation quantile d'une autre donnée (ex : représenter les données de 2015 en quantile des données 2023 pour comparer 2 cartes)
#
#     3. Représenter les données avec ggplot :
#ggplot(contours a afficher en fond de carte) +
#   geom_sf() +
#   geom_sf(data = donnée à représenter, aes(fill = class))
#   scale_fill_manual(values=gen_colors(donnée à représenter, palette de couleurs RColorBrewer))

#couleurs :
#   nb offres (stock) = "PuRd"
#   prix au m2 : "YlOrBr"
#   offres/km2 : "BuGn"
#   part d'un type de logement :  "BuPu"


##### GRILLES VARIABLES 2000-5000 #####
na_grille <- creer_grille(contour_mun, df_MC2015_V_clean,2000,500)
na_grille2023 <- creer_grille(contour_mun, df_MC_V_clean,2000,500)

na_grille2015_complete <- creer_grille(contour_mun, df_MC2015_V_complete,2000,500)

na_grille_cas <- creer_grille(contour_mun, df_MC2015_V_clean[df_MC2015_V_clean$type == "Casa en Venta",],2000,500)
na_grille_dep <- creer_grille(contour_mun, df_MC2015_V_clean[df_MC2015_V_clean$type == "Departamento en Venta",],2000,500)

na_grille_cas2023 <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Casa en Venta",],2000,500)
na_grille_dep2023 <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Departamento en Venta",],2000,500)

#offres/km2 et prix/m2, grille 2000-500, 2015
na_grille_cas2015_ofkm2 <- class_values(na_grille_cas, "offers_km2",nb_min = 3, source = na_grille_cas2023)
na_grille_dep2015_ofkm2 <- class_values(na_grille_dep, "offers_km2",nb_min = 3, source = na_grille_dep2023)

na_grille_cas2015_prm2 <- class_values(na_grille_cas, "price_m2",nb_min = 3, source = na_grille_cas2023)
na_grille_dep2015_prm2 <- class_values(na_grille_dep, "price_m2",nb_min = 3, source = na_grille_dep2023)

#offres/km2 et prix/m2, grille 2000-500, 2023
na_grille_cas2023_ofkm2 <- class_values(na_grille_cas2023, "offers_km2",nb_min = 3)
na_grille_dep2023_ofkm2 <- class_values(na_grille_dep2023, "offers_km2",nb_min = 3)

na_grille_cas2023_prm2 <- class_values(na_grille_cas2023, "price_m2",nb_min = 3)
na_grille_dep2023_prm2 <- class_values(na_grille_dep2023, "price_m2",nb_min = 3)

#repartition des biens, grille 2000-500, 2023
na_grille_repart <- class_values(na_grille2023, "part_dep",nb_min = 3)


#graphique eb barres
ggplot(na_grille_dep2015_prm2, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_manual(values=gen_colors(na_grille_dep2023_prm2, "YlOrBr"))


(contour_mun) +
  geom_sf() +
  geom_sf(data = na_grille_repart, aes(fill = class), lwd = 0.1)+
  scale_fill_manual(values=gen_colors(na_grille_repart, "BuPu"))

##### GRILLES SIMPLES, 2000m et 1000m #####
grille_simple_cas <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Casa en Venta",],2000,2000)
grille_simple_dep <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Departamento en Venta",],2000,2000)

grille_simple_cas1000 <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Casa en Venta",],1000,1000)
grille_simple_dep1000 <- creer_grille(contour_mun, df_MC_V_clean[df_MC_V_clean$type == "Departamento en Venta",],1000,1000)

grille_simple <- creer_grille(contour_mun, df_MC2015_V_clean,2000,2000)
grille_simple2023 <- creer_grille(contour_mun, df_MC_V_clean,2000,2000)

#offres/km2 et prix/km2, grille simple 2000 m
grille_simple_cas_ofkm2 <- class_values(grille_simple_cas, "offers_km2", nb_min = 3)
grille_simple_dep_ofkm2 <- class_values(grille_simple_dep, "offers_km2", nb_min = 3)

grille_simple_cas_prm2 <- class_values(grille_simple_cas, "price_m2", nb_min = 3)
grille_simple_dep_prm2 <- class_values(grille_simple_dep, "price_m2", nb_min = 3)

#offres/km2 et prix/km2, grille simple 1000 m
grille_simple_cas1000_ofkm2 <- class_values(grille_simple_cas1000, "offers_km2", nb_min = 3)
grille_simple_dep1000_ofkm2 <- class_values(grille_simple_dep1000, "offers_km2", nb_min = 3)

grille_simple_cas1000_prm2 <- class_values(grille_simple_cas1000, "price_m2", nb_min = 3)
grille_simple_dep1000_prm2 <- class_values(grille_simple_dep1000, "price_m2", nb_min = 3)

#nombre d'offres (stock), 2023 et 2015
grille_simple <- class_values(grille_simple, "nb_offers", nb_min = 1, source = grille_simple2023)
grille_simple2023 <- class_values(grille_simple2023, "nb_offers", nb_min = 1)




ggplot(contour_mun) +
  geom_sf() +
  geom_sf(data = grille_simple, aes(fill = class), lwd = 0.1)+
  scale_fill_manual(values=gen_colors(grille_simple, "PuRd", seuil_nb_offers = FALSE))



ggplot(contour_mun) +
  geom_sf() +
  geom_sf(data = grille_simple2023, aes(fill = class), lwd = 0.1)+
  scale_fill_brewer(palette="PuRd")



ggplot(contour_mun) +
  geom_sf() +
  geom_sf(data = grille_simple_cas, aes(fill = class), lwd = 0.1)+
  scale_fill_manual(values=gen_colors(grille_simple_cas, "BuGn"))

ggplot(contour_mun) +
  geom_sf() +
  geom_sf(data = grille_simple_dep, aes(fill = class), lwd = 0.1)+
  scale_fill_manual(values=gen_colors(grille_simple_dep, "BuGn"))

##### PAR MUNICIPE #####
remplir_mun <- function(sf_mun, offres, nb_min = 100){
  #fonction permettant d'ajouter aux géométries des municipes les valeurs des poinst qu'ils contiennent :
  #prix moyen, surface moyenne, nombre d'offres et prix au m² pour chaque division
  offres$ID <- seq.int(nrow(offres))
  
  sf_mun$nb_offers = lengths(st_intersects(sf_mun, offres))
  # remove grid without value of 0 (i.e. no points in side that grid)
  sf_mun = filter(sf_mun, nb_offers >= nb_min)
  
  sf_mun$id_offers <- st_intersects(sf_mun, offres)
  sf_mun$avg_price <- NA
  sf_mun$avg_surf <- NA
  sf_mun$area <- NA
  for (i in 1:length(sf_mun$id_offers)){
    count <- length(sf_mun$id_offers[[i]])
    
    avg_price = mean(offres[offres$ID %in% sf_mun$id_offers[[i]],]$price)
    sf_mun[[8]][i] <- avg_price
    
    avg_surf = mean(offres[offres$ID %in% sf_mun$id_offers[[i]],]$surface_grnd)
    sf_mun[[9]][i] <- avg_surf
    
    sf_mun[[10]][i] <- st_area(sf_mun[[5]][i])
    }
  
  sf_mun <- sf_mun %>% select(-c(id_offers))
  
  sf_mun$price_m2 <- sf_mun$avg_price / sf_mun$avg_surf
  sf_mun$offers_km2 <- sf_mun$nb_offers / sf_mun$area * 1000000
  
  return(sf_mun)
}

mun <- remplir_mun(contour_mun, df_MC_V_clean, nb_min = 50)
mun <- class_values(mun, "offers_km2")

ggplot(mun) +
  geom_sf(aes(fill = class))+
  scale_fill_brewer(palette="YlOrBr")

##### PAR AGEB #####
remplir_ageb <- function(sf_ageb, offres, nb_min = 5){
  offres$ID <- seq.int(nrow(offres))
  
  sf_ageb$nb_offers = lengths(st_intersects(sf_ageb, offres))
  # remove grid without value of 0 (i.e. no points in side that grid)
  sf_ageb = filter(sf_ageb, nb_offers >= nb_min)
  
  sf_ageb$id_offers <- st_intersects(sf_ageb, offres)
  sf_ageb$avg_price <- NA
  sf_ageb$avg_surf <- NA
  sf_ageb$area <- NA
  
  for (i in 1:length(sf_ageb$id_offers)){
    count <- length(sf_ageb$id_offers[[i]])
    
    avg_price = mean(offres[offres$ID %in% sf_ageb$id_offers[[i]],]$price)
    sf_ageb[[12]][i] <- avg_price
    
    avg_surf = mean(offres[offres$ID %in% sf_ageb$id_offers[[i]],]$surface_grnd)
    sf_ageb[[13]][i] <- avg_surf
    
    sf_ageb[[14]][i] <- st_area(sf_ageb[[9]][i])
    }
  
  sf_ageb <- sf_ageb %>% select(-c(id_offers))
  
  sf_ageb$price_m2 <- sf_ageb$avg_price / sf_ageb$avg_surf
  sf_ageb$offers_km2 <- sf_ageb$nb_offers / sf_ageb$area * 1000000
  
  return(sf_ageb)
}

ageb <- remplir_ageb(contour_ageb, df_MC_V_clean)

ageb_cas <- remplir_ageb(contour_ageb, df_MC_V_clean[df_MC_V_clean$type == "Casa en Venta",])
ageb_dep <- remplir_ageb(contour_ageb, df_MC_V_clean[df_MC_V_clean$type == "Departamento en Venta",])



tc_ageb_cas <- remplir_ageb(contour_ageb, df_TC_V_clean[df_TC_V_clean$type == "Casa en Venta",],1)
tc_ageb_dep <- remplir_ageb(contour_ageb, df_TC_V_clean[df_TC_V_clean$type == "Departamento en Venta",],1)

ageb_cas_ofkm2 <- class_values(ageb_cas, "offers_km2")
ageb_dep_ofkm2 <- class_values(ageb_dep, "offers_km2")

ageb_cas_prm2 <- class_values(ageb_cas, "price_m2")
ageb_dep_prm2 <- class_values(ageb_dep, "price_m2")


ggplot(contour_mun) +
  geom_sf()+
  geom_sf(data = ageb_cas_ofkm2, aes(fill = class), lwd = 0.1)+
  scale_fill_manual(values=gen_colors(ageb_cas_ofkm2, "BuGn", seuil_nb_offers = FALSE))


ggplot(contour_mun) +
  geom_sf()+
  geom_sf(data = ageb_dep, aes(fill = class), lwd = 0.1)+
  scale_fill_brewer(palette="BuGn")

##### PAR COLONIE #####
remplir_col <- function(sf_col, offres, nb_min = 5){
  offres$ID <- seq.int(nrow(offres))
  
  sf_col$nb_offers = lengths(st_intersects(sf_col, offres))
  # remove grid without value of 0 (i.e. no points in side that grid)
  sf_col = filter(sf_col, nb_offers >= nb_min)
  
  sf_col$id_offers <- st_intersects(sf_col, offres)
  sf_col$avg_price <- NA
  sf_col$avg_surf <- NA
  sf_col$area <- NA
  
  for (i in 1:length(sf_col$id_offers)){
    count <- length(sf_col$id_offers[[i]])
    
    avg_price = mean(offres[offres$ID %in% sf_col$id_offers[[i]],]$price)
    sf_col[[13]][i] <- avg_price
    
    avg_surf = mean(offres[offres$ID %in% sf_col$id_offers[[i]],]$surface_grnd)
    sf_col[[14]][i] <- avg_surf
    
    sf_col[[15]][i] <- st_area(sf_col[[10]][i])
    }
  
  sf_col <- sf_col %>% select(-c(id_offers))
  
  sf_col$price_m2 <- sf_col$avg_price / sf_col$avg_surf
  sf_col$offers_km2 <- sf_col$nb_offers / sf_col$area * 1000000
  
  return(sf_col)
}

colonia <- remplir_col(contour_colonias, df_MC_V_clean)
colonia <- class_values(colonia, "offers_km2")

ggplot(contour_mun) +
  geom_sf()+
  geom_sf(data = colonia, aes(fill = class), lwd = 0.1)+
  scale_fill_brewer(palette="YlOrBr")


##### PAR POINTS #####

ggplot(contour_mun)+
  geom_sf()#+
  geom_sf(data = df_MC2015_V_clean, aes(color = type), size = 1)+
  scale_color_brewer(palette = "Dark2")

################# RESUME DES DONNEES ################# 

resume <- function(list){
  print(paste("moyenne :",mean(list)))
  print(paste("ecart type :",sd(list)))
  print(paste("variance :", var(list)))
  print(paste("max :",max(list)))
  print(paste("min :",min(list)))
  quant <- quantile(list, probs=seq(0, 1, 1/4))
  dec <- quantile(list, probs=seq(0, 1, 1/10))
  print(paste("q1 :",quant[2]))
  print(paste("mediane :",quant[3]))
  print(paste("q3 :",quant[4]))
  print(paste("d1 :",dec[2]))
  print(paste("d9 :",dec[10]))
  print(paste("ecart inter-quartile :",quant[4]-quant[2]))
  print(paste("ecart inter-decile :",dec[10]-dec[2]))
}


