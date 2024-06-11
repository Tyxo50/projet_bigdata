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

put_na_if_empty <- function(data){
  for (col in colnames(data)) {
    data[[col]][data[[col]] == ""] <- NA
  }
  return(data)
}


remove_empty_line13 <- function(data){
    #remove the line 

   
}
















# # Étape 1: Supposons que 'data' est votre dataframe et qu'il contient des colonnes 'X' et 'Y'

# # Étape 2: Conversion de votre dataframe en un objet sf
# data_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 3949) # Remplacez 4326 par le CRS de vos données si différent

# # Étape 3: Lecture des shapefiles
# quartier_sf <- st_read("chemin/vers/le/shapefile/quartier.shp")
# secteur_sf <- st_read("chemin/vers/le/shapefile/secteur.shp")

# # Étape 4: Jointure spatiale pour retrouver le quartier et le secteur
# data_quartier <- st_join(data_sf, quartier_sf, join = st_within)
# data_secteur <- st_join(data_sf, secteur_sf, join = st_within)

# # Étape 5: Ajout des informations de quartier et secteur au dataframe
# # Assurez-vous que vos shapefiles contiennent des colonnes pour les noms de quartier et de secteur
# data$quartier <- data_quartier$nom_quartier
# data$secteur <- data_secteur$nom_secteur

# # Votre dataframe 'data' contient maintenant deux nouvelles colonnes : 'quartier' et 'secteur'