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




