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
      data[[col]] = as.logical(data[[col]])
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


all_str_to_lower = function(data){
  for(col in colnames(data)){
    if(unique(sapply(data[[col]], class)) == "character"){
      data[[col]] = tolower(data[[col]])
    }
  }
  return(data)
}


display_map_arbres_par_quartier <- function (data){
  # =========== DEBUT AFFICHAGE ALL MAP PAR QUARTIER
  # Initialisation de la carte
map <- leaflet() %>% addTiles()

# valeurs uniques non NA, = nom des quartiers
quartiers <- unique(na.omit(data$clc_quartier))
# Définir un vecteur de couleurs
colors <- colorRampPalette(c("red", "blue", "green", "orange", "purple", "cyan"))(length(quartiers))

for(i in seq_along(quartiers)) {
  quartier <- quartiers[i]
  print(quartier)

  # Filtrer les données pour le quartier en cours
  quartier_data <- data %>% filter(clc_quartier == quartier)

  # Création de la data frame pour les points du quartier
  new_points <- data.frame(
    id = 1:nrow(quartier_data),
    x = quartier_data$X,
    y = quartier_data$Y
  ) %>%
  st_as_sf(coords = c("x", "y"), crs = 3949) %>%
  st_transform(4326)

  # Ajout des cercles à la carte avec une couleur différente
  map <- map %>% addCircles(data = new_points, color = colors[i], group = quartier)
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



