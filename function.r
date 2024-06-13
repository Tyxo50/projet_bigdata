library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(tidyverse)

to_utf8 <- function(data) {
  for (identifier in colnames(data)) {
    data[[identifier]] = iconv(data[[identifier]], from = "latin1", to = "UTF-8")
    
  }
  return(data)
}

convert_data <- function(data, types = c('numeric', 'numeric', 'integer', 'date', 'character', 'character', 'character', 'character', 'integer', 'numeric', 'numeric', 'integer', 'character', 'character', 'character', 'character', 'character', 'bool', 'character', 'date', 'integer', 'integer', 'integer', 'date', 'character', 'character', 'date', 'character', 'character', 'character', 'character', 'date', 'character', 'date', 'character', 'character', 'bool')) {
  i = 1
  for(col in colnames(data)){

    if(types[i] == 'numeric'){
      data[[col]] = as.numeric(data[[col]])
    }else if (types[i] == 'integer'){
      data[[col]] = as.integer(data[[col]])
    }else if (types[i] == 'character'){
      data[[col]] = as.character(data[[col]])
    }else if (types[i] == 'bool'){
      data[[col]] = as.factor(data[[col]])
    }else if (types[i] == 'date'){
      data[[col]] = as.Date(data[[col]], format="%Y/%m/%d")
    }else{
      print("Type non connu?!")
    }
    i=i+1
  }
  return(data)
}

remove_na_on_x_y <- function(data){
  data = data[!is.na(data$X), ]
  data = data[!is.na(data$Y), ]
  return(data)
}



remove_outliers_age_estime = function(data){ # enleve les valeurs aberrantes de age_estim
  # src : https://www.r-bloggers.com/2021/09/how-to-remove-outliers-in-r-4/
  #Q1 <- quantile(data$age_estim, 0.001, na.rm = TRUE) # place des quantiles
  Q3 <- quantile(data$age_estim, 0.9999, na.rm = TRUE)
  IQR <- IQR(data$age_estim, na.rm = TRUE)
  
  data$age_estim[data$age_estim > (Q3 + 1.5*IQR)] = mean(data$age_estim < (Q3 + 1.5*IQR), na.rm = TRUE) # remplace les valeurs en dehors des bornes par la moyenne (moyenne sans les valeurs extremes)
  return(data)
}


remove_outliers_age_estime_2 = function(data){ # enleve les valeurs aberrantes de age_estim, avec palier fixe
  data$age_estim[data$age_estim > 1000] = mean(data$age_estim < 500, na.rm = TRUE) # remplace les valeurs en dehors des bornes par la moyenne (moyenne sans les valeurs extremes)
  return(data)
}

norme_euclidienne = function(x1, y1, x2, y2){
  return(sqrt((x2-x1)^2+(y2-y1)^2))
}

to_factor = function(data){ # transforme ces colonnes en facteurs pour simplifier la suite IA peut etre
  data$clc_quartier = as.factor(data$clc_quartier)
  data$fk_arb_etat = as.factor(data$fk_arb_etat)
  data$fk_stadedev = as.factor(data$fk_stadedev)
  data$fk_port = as.factor(data$fk_port)
  data$fk_pied = as.factor(data$fk_pied)
  data$fk_situation = as.factor(data$fk_situation)
  data$fk_revetement = as.factor(data$fk_revetement)
  data$feuillage = as.factor(data$feuillage)
  data$nomfrancais = as.factor(data$nomfrancais)
  data$nomfrancais = as.factor(data$nomlatin)
  data$remarquable = as.factor(data$remarquable)
  
  return(data)
}


put_na_if_empty <- function(data){
  for (col in colnames(data)) {
    data[[col]][data[[col]] == ""] <- NA
  }
  return(data)
}


remove_empty_line13 <- function(data){
  data = data[rowSums(is.na(data)) < 13, ]
  return(data)
  
}

remove_object_id <- function(data){
  data$OBJECTID = NULL
  return(data)
}

to_lower <- function(data){
  for (col in colnames(data)) {
    data[[col]] = tolower(data[[col]])
  }
  return(data)
}

remove_doublon <- function(data){ 
  data = data[!duplicated(data[c("X", "Y", "fk_arb_etat")]), ]
  return(data)
}


remplace_na_par_mediane <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  x[is.na(x)] <- median_value
  return(x)
}



remove_ras <- function(data, colonne) {
  data <- data[data[[colonne]] != "RAS", ]
  return(data)
}


remove_na_feuillage <- function(data, colonne) {
  data <- data[!is.na(data[[colonne]]), ]
  return(data)
}

get_quartier_from_coords <- function(data) {
  coords <- st_transform(st_as_sf(data, coords = c("X", "Y"), crs =  3949), crs = 4326)
  coords <- st_coordinates(coords)
  coords <- coords[, c("X", "Y")]
  data$Y=coords[,1]
  data$X=coords[,2]
  for (i in 1:nrow(data)) {
    if(is.na(data$clc_quartier[i])) {
      url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f", data$X[i], data$Y[i])
      response <- GET(url)
      if (status_code(response) == 200) {
        data_quartier <- fromJSON(rawToChar(response$content))
        print(paste("aa",i,"bbbb",data_quartier$address))
        data$clc_quartier[i] <- data_quartier$address
      } else {
        print(paste("Erreur lors de la requête à l'API pour la ligne", i, ". Code d'erreur:", status_code(response)))
      }
    }
  }
  
  return(data)
}
to_factor = function(data){ # transforme ces colonnes en facteurs pour simplifier la suite IA peut etre
  data$clc_quartier = as.factor(unlist(data$clc_quartier))
  data$fk_arb_etat = as.factor(data$fk_arb_etat)
  data$fk_stadedev = as.factor(data$fk_stadedev)
  data$fk_port = as.factor(data$fk_port)
  data$fk_pied = as.factor(data$fk_pied)
  data$fk_situation = as.factor(data$fk_situation)
  data$fk_revetement = as.factor(data$fk_revetement)
  data$feuillage = as.factor(data$feuillage)
  data$nomfrancais = as.factor(data$nomfrancais)
  data$remarquable = as.factor(data$remarquable)

  return(data)
}

predict_tronc_diam <- function(data) {
  data_temp <- data[!is.na(data$haut_tronc) & !is.na(data$haut_tot) & !is.na(data$feuillage) & !is.na(data$fk_stadedev) & !is.na(data$age_estim), ]
  
  model <- lm(tronc_diam ~ haut_tronc + haut_tot + fk_stadedev + feuillage + age_estim, data = data_temp)
  print(summary(model))
  
  rows_to_predict <- data[is.na(data$tronc_diam), ]
  
  if(nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict)
    
    data$tronc_diam[is.na(data$tronc_diam)] <- round(predictions)
  }
  
  return(data)
}

predict_age <- function(data) {
  data_temp <- data[!is.na(data$age_estim) & !is.na(data$tronc_diam) & !is.na(data$haut_tot) & !is.na(data$fk_stadedev) & !is.na(data$haut_tronc), ]
  
  model <- lm(age_estim ~ tronc_diam + haut_tot + haut_tronc + fk_stadedev, data = data_temp)
  print(summary(model))
  rows_to_predict <- data[is.na(data$age_estim), ]
  
  if(nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict)
    
    data$age_estim[is.na(data$age_estim)] <- round(predictions)
  }
  
  return(data)
}

predict_remarquable <- function(data) {
  data_temp <- data[!is.na(data$remarquable) & !is.na(data$tronc_diam) & !is.na(data$haut_tot) & !is.na(data$fk_stadedev) & !is.na(data$nomfrancais), ]
  print(data$remarquable)
  model <- glm(remarquable ~ tronc_diam + haut_tot + fk_stadedev + nomfrancais, data = data_temp, family = "binomial")
  rows_to_predict <- data[is.na(data$remarquable), ]
  
  if (nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict, type = "response")
    predicted_classes <- ifelse(predictions >= 0.5, 1, 0)
    data$remarquable[is.na(data$remarquable)] <- predicted_classes
  }
  return(data)
}


predict_tronc_diam <- function(data) {
  data_temp <- data[!is.na(data$haut_tronc) & !is.na(data$haut_tot) & !is.na(data$feuillage) & !is.na(data$fk_stadedev) & !is.na(data$age_estim) , ]
  
  model <- lm(tronc_diam ~ haut_tronc + haut_tot + fk_stadedev + feuillage + age_estim, data = data_temp)
  print(summary(model))
  
  rows_to_predict <- data[is.na(data$tronc_diam), ]
  
  if(nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict)
    
    data$tronc_diam[is.na(data$tronc_diam)] <- round(predictions)
  }
  
  return(data)
}

predict_age <- function(data) {
  data_temp <- data[!is.na(data$age_estim) & !is.na(data$tronc_diam) & !is.na(data$haut_tot) & !is.na(data$haut_tronc), ]
  
  model <- lm(age_estim ~ tronc_diam + haut_tot + haut_tronc, data = data_temp)
  print(summary(model))
  rows_to_predict <- data[is.na(data$age_estim), ]
  
  if(nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict)
    
    data$age_estim[is.na(data$age_estim)] <- round(predictions)
  }
  
  return(data)
}

predict_remarquable <- function(data) {
  data$remarquable <- ifelse(data$remarquable == "oui", 1, ifelse(data$remarquable == "non" | data$remarquable == "", 0, NA))
  
  data_temp <- data[!is.na(data$remarquable) & !is.na(data$tronc_diam) & !is.na(data$haut_tot) & !is.na(data$fk_stadedev) & !is.na(data$nomfrancais), ]
  
  model <- glm(remarquable ~ tronc_diam + haut_tot + fk_stadedev + nomfrancais, data = data_temp, family = "binomial")
  print(summary(model))
  rows_to_predict <- data[is.na(data$remarquable), ]
  
  if (nrow(rows_to_predict) > 0) {
    predictions <- predict(model, newdata = rows_to_predict, type = "response")
    data$remarquable[is.na(data$remarquable)] <- round(predictions)
  }
  
  data$remarquable <- ifelse(data$remarquable == 1, "oui", "non")
  
  return(data)
}


lien_variable_quantitative <- function(data){
  
  ggplot(data,aes(x = haut_tot,y = age_estim))+
    geom_point()+
    geom_smooth(method ="lm", col="blue")+
    labs(title = "Relation entre la hauteur totale et l'âge de l'arbre",
         x = "Hauteur Totale",
         y = "Âge de l'Arbre")
  print(paste("Test de corrélation",cor(data$haut_tot,data$age_estim)))
  
  
  ggplot(data,aes(x = tronc_diam,y = age_estim))+
    geom_point()+
    geom_smooth(method ="lm", col="blue")+
    labs(title = "Relation entre le diamètre du tronc et l'âge de l'arbre",
         x = "Diamètre du tronc",
         y = "Âge de l'Arbre")
  print(paste("Test de corrélation",cor(data$tronc_diam,data$age_estim)))
  
  ggplot(data,aes(x = haut_tot,y = tronc_diam))+
    geom_point()+
    geom_smooth(method ="lm", col="blue")+
    labs(title = "Relation entre la hauteur totale et diamètre du tronc",
         x = "Hauteur Totale",
         y = "Diamètre du tronc")
  print(paste("Test de corrélation",cor(data$haut_tot,data$tronc_diam)))
}

lien_variable_qualitative <- function(data){
  
  tab <- table(data$remarquable, data$fk_stadedev)
  
  chi2_test <- chisq.test(tab)
  print(paste("Test chi2 entre remarquable et stade dev", chi2_test))
  
  mosaicplot(tab,main ="Mosaicplot entre remarquable et stade dev")
  
  tab2 <- table(data$remarquable, data$feuillage)
  
  chi2_test2 <- chisq.test(tab2)
  print(paste("Test chi2 entre remarquable et feuillage", chi2_test2))
  
  mosaicplot(tab,main ="Mosaicplot entre remarquable et feuillage")
}

lien_variable_quan_qual<- function(data){
  
  ggplot(data,aes(x=feuillage,y=age_estim))+
    geom_boxplot()+
    labs(title ="Distribution entre l'âge et remarquable",
         x = "Âge",
         y = "Remarquable")
  anova_test <- aov(age_estim ~ remarquable,data=data)
  print(summary(anova_test))
}



display_map_arbres_par_quartier <- function (data){
  # =========== DEBUT AFFICHAGE ALL MAP PAR QUARTIER
  # Initialisation de la carte
map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles()

# valeurs uniques non NA, = nom des quartiers
quartiers <- unique(na.omit(data$clc_quartier))
# Définir un vecteur de couleurs
colors <- colorRampPalette(c("red", "blue", "green", "orange", "purple", "cyan"))(length(quartiers))

for(i in seq_along(quartiers)) {
  quartier <- quartiers[i]
  #print(quartier)

  # Filtrer les données pour le quartier en cours
  quartier_data <- data %>% filter(clc_quartier == quartier)

  quartier_data$tronc_diam <- as.numeric(quartier_data$tronc_diam)
  #print(quartier_data$tronc_diam)
  # Création de la data frame pour les points du quartier
  # new_points <- data.frame(
  #   id = 1:nrow(quartier_data),
  #   lat = quartier_data$X,
  #   lon = quartier_data$Y,
  #   taille = quartier_data$haut_tot
  # )
  # Ajout des cercles à la carte avec une couleur différente
  #View(quartier_data$tronc_diam)
  map <- map %>% addCircles(data = quartier_data,
                            radius = ~ifelse(is.na(tronc_diam), 1, tronc_diam/(2*pi)/20),# taille de la pastille en fonction du diametre
                            lat=quartier_data$X,
                            lng=quartier_data$Y,
                            color = ~ifelse(remarquable == "oui", "black", colors[i]), # les arbres remarquables en noir
                            group = quartier,
                            popup = ~paste("Taille : ", quartier_data$haut_tot, "<br>Quartier : ", quartier, "<br>Diam : ", quartier_data$tronc_diam)) # infos en cliquant sur la pasatille
}

# Ajout de la légende à la carte
map <- map %>% addLegend(
  position = "bottomright",
  colors = colors,
  labels = quartiers,
  title = "Quartiers"
)
map
# ------ FIN AFFICHER ALL MAP PAR QUARTIERS
}


display_map_arbres_par_feuillage <- function (data){
  # =========== DEBUT AFFICHAGE ALL MAP PAR QUARTIER
  # Initialisation de la carte
map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles()

# valeurs uniques non NA, = nom des quartiers
feuillages <- unique(na.omit(data$feuillage))
# Définir un vecteur de couleurs
colors <- colorRampPalette(c("red", "blue"))(length(feuillages))

for(i in seq_along(feuillages)) {
  feuil <- feuillages[i]
  #print(quartier)

  # Filtrer les données pour le quartier en cours
  feuillage_data <- data %>% filter(feuillage == feuil)

  #feuillage_data <- as.numeric(feuillage_data$feuillage)

  map <- map %>% addCircles(data = feuillage_data,
                            radius = ~ifelse(is.na(tronc_diam), 1, tronc_diam/(2*pi)/20),# taille de la pastille en fonction du diametre
                            lat=feuillage_data$X,
                            lng=feuillage_data$Y,
                            color = ~ifelse(remarquable == "oui", "black", colors[i]), # les arbres remarquables en noir
                            group = feuil,
                            popup = ~paste("Feuillage : ", feuillage_data$feuillage,"<br>Taille : ", feuillage_data$haut_tot, "<br>Quartier : ", feuillage_data$clc_quartier, "<br>Diam : ", feuillage_data$tronc_diam)) # infos en cliquant sur la pasatille
}

# Ajout de la légende à la carte
map <- map %>% addLegend(
  position = "bottomright",
  colors = colors,
  labels = feuillages,
  title = "Feuillages"
)
map
# ------ FIN AFFICHER ALL MAP PAR QUARTIERS
}
clc_zone_indus <- function (data){
  data$clc_quartier[data$clc_secteur == tolower("Zone industrielle le Royeux (A.Europe)")] <- tolower("Zone industrielle le Royeux")
  data$clc_quartier[data$clc_secteur == tolower("Zone industrielle le Royeux (G.Pompidou)")] <- tolower("Zone industrielle le Royeux")
  data$clc_quartier[data$clc_secteur == tolower("Zone industrielle le Royeux (G.Eiffel)")] <- tolower("Zone industrielle le Royeux")
  data$clc_quartier[data$clc_secteur == tolower("Zone industrielle le Royeux (bassin)")] <- tolower("Zone industrielle le Royeux")
  return(data)
}









# display_map_arbres_par_stade_dev <- function (data){
#   # =========== DEBUT AFFICHAGE ALL MAP PAR QUARTIER
#   # Initialisation de la carte
# map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles()
#
# # valeurs uniques non NA, = nom des quartiers
# stade_devs <- unique(na.omit(data$stade_dev))
# # Définir un vecteur de couleurs
# colors <- colorRampPalette(c("red", "blue", "green", "orange", "purple", "cyan"))(length(stade_devs))
#
# for(i in seq_along(stade_devs)) {
#   sdv <- stade_devs[i]
#   #print(quartier)
#
#   # Filtrer les données pour le quartier en cours
#   stade_dev_data <- data %>% filter(stade_dev == sdv)
#
#   #feuillage_datatronc_diam <- as.numeric(feuillage_data$feuillage)
#
#   map <- map %>% addCircles(data = stade_dev_data,
#                             radius = ~ifelse(is.na(tronc_diam), 1, tronc_diam/(2*pi)/20),# taille de la pastille en fonction du diametre
#                             lat=stade_dev_data$X,
#                             lng=stade_dev_data$Y,
#                             color = ~ifelse(remarquable == "oui", "black", colors[i]), # les arbres remarquables en noir
#                             group = feuil,
#                             popup = ~paste("Feuillage : ", stade_dev_data$feuillage,"<br>Taille : ", stade_dev_data$haut_tot, "<br>Quartier : ", stade_dev_data$clc_quartier, "<br>Diam : ", stade_dev_data$tronc_diam)) # infos en cliquant sur la pasatille
# }
#
# # Ajout de la légende à la carte
# map <- map %>% addLegend(
#   position = "bottomright",
#   colors = colors,
#   labels = stade_devs,
#   title = "Stade de dev"
# )
# map
# # ------ FIN AFFICHER ALL MAP PAR QUARTIERS
# }
