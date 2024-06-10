data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")


to_utf8 <- function(data) {
    for (identifier in colnames(data)) {
        data[[identifier]] = iconv(data[[identifier]], from = "latin1", to = "UTF-8")
    }
    return(data)
}

convert_data <- function(data, types = c('numeric', 'numeric', 'integer', 'date', 'character', 'character', 'character', 'character', 'integer', 'numeric', 'numeric', 'integer', 'character', 'character', 'character', 'character', 'character', 'bool', 'character', 'date', 'integer', 'integer', 'integer', 'date', 'character', 'character', 'date', 'character', 'character', 'character', 'character', 'date', 'character', 'date', 'character', 'character', 'bool')) {
  i = 1
  for(col in colnames(data)){
<<<<<<< HEAD
    # print(i)
    # print(col)
=======
    
>>>>>>> 23b5f939c4801185e3791bddc070d7811faba853
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

add_year_only_column = function(data){
  years = as.Date(data$created_date, format="%Y")
  data[["year"]] = years
  return(data)
}



data = to_utf8(data)
data2 = convert_data(data)

<<<<<<< HEAD
data_plus_years = add_year_only_column(data2)

# print(summary(data))
# 
=======

>>>>>>> 23b5f939c4801185e3791bddc070d7811faba853
sapply(data2, class)

View(data2)




remove_outliers_age_estime = function(data){
  # src : https://www.r-bloggers.com/2021/09/how-to-remove-outliers-in-r-4/
  #Q1 <- quantile(data$age_estim, 0.001, na.rm = TRUE) # place des quantiles
  Q3 <- quantile(data$age_estim, 0.9999, na.rm = TRUE)
  IQR <- IQR(data$age_estim, na.rm = TRUE)
  
  data$age_estim[data3$age_estim > (Q3 + 1.5*IQR)] = mean(data3$age_estim < (Q3 + 1.5*IQR), na.rm = TRUE) # remplace les valeurs en dehors des bornes par la moyenne (moyenne sans les valeurs extremes)
  return(data)
}


remove_outliers_age_estime_2 = function(data){
  data$age_estim[data3$age_estim > 1000] = mean(data3$age_estim < 500, na.rm = TRUE) # remplace les valeurs en dehors des bornes par la moyenne (moyenne sans les valeurs extremes)
  return(data)
  }

data3 = data2
d = remove_outliers_age_estime_2(data3)
View(d)
head(data3)

boxplot(d$age_estim)





boxplot(data3$age_estim)
head(data3$age_estim)
View(data3)




# data3$haut_tot[isZero(data3$haut_tot)] = mean(data3$haut_tot, na.rm = TRUE)
data3$haut_tot[data3$haut_tot==0] = mean(data3$haut_tot[data3$haut_tot != 0], na.rm = TRUE) # remplace les hauteurs de 0 par la moyenne

model = lm(formula = data3$haut_tot ~ data3$age_estim)
summary(model)

plot(haut_tot ~ age_estim, data=data3)
abline(model)




no_outliers = subset(data, data2$age_estim > (Q1 - 1.5*IQR) & data2$age_estim < (Q3 + 1.5*IQR))
dim(no_outliers)
boxplot(no_outliers)

