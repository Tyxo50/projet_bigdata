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

library(leaflet)
library(httr)
library(jsonlite)
library(sf)


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




