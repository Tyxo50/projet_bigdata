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
    # print(i)
    # print(col)
    
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

data_plus_years = add_year_only_column(data2)

# print(summary(data))
# 


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
  data$age_estim[data$age_estim > 1000] = mean(data$age_estim < 500, na.rm = TRUE) # remplace les valeurs en dehors des bornes par la moyenne (moyenne sans les valeurs extremes)
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

plot(data$X, data$Y)
vv=c(11228:11232)
points(data$X[vv], data$Y[vv], col="red")
points(data$X[data$clc_quartier==""], data$Y[data$clc_quartier==""], col="violet")




norme_euclidienne = function(x1, y1, x2, y2){
  return(sqrt((x2-x1)^2+(y2-y1)^2))
}

hist(data$haut_tot)

to_factor = function(data){
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


all_str_to_lower = function(data){
  for(col in colnames(data)){
    print(unique(sapply(data[[col]], class)))
    if(unique(sapply(data[[col]], class)) == "character"){
      data[[col]] = tolower(data[[col]])
    }
  }
  return(data)
}

data = all_str_to_lower(data)



# ================= PLOTS ==================
# barplot arbres par quartier
barplot(prop.table(table(data$feuillage)), las=2)

# boxplot diametre tronc
boxplot(data$tronc_diam)

# plot arbre apar stade developpement
plot(data$X, data$Y)

points(data$X[data$fk_stadedev=="adulte"], data$Y[data$fk_stadedev=="adulte"], col="violet")
#points(data$X[data$fk_stadedev=="Adulte"], data$Y[data$fk_stadedev=="Adulte"], col="violet")
#points(data$X[data$fk_stadedev=="Jeune"], data$Y[data$fk_stadedev=="Jeune"], col="green")
points(data$X[data$fk_stadedev=="jeune"], data$Y[data$fk_stadedev=="jeune"], col="green")
points(data$X[data$fk_stadedev=="senescent"], data$Y[data$fk_stadedev=="senescent"], col="blue")
points(data$X[data$fk_stadedev=="vieux"], data$Y[data$fk_stadedev=="vieux"], col="brown")

legend("topleft", legend=c("adulte", "jeune", "senescent", "vieux"), pch=16, col=c("violet", "green", "blue", "brown"))

# barplot arbres par stade de developpement
barplot(prop.table(table(data$fk_stadedev)), las=2)


# arbres par type de feuillage
barplot(prop.table(table(data$feuillage)), las=1)


# animals <- c("cat", "dog",  "dog", "dog", "dog", "dog", "dog", "dog", "cat", "cat", "bird")
# animalFactor <- as.factor(animals)
# #hist(table(animalFactor), freq=TRUE, xlab = levels(animalFactor), ylab = "Frequencies")
# barplot(prop.table(table(animals)))

n = 10
x = sample(data$X, n)
y = sample(data$Y, n)
data$Y[data$clc_quartier == "harly"][!is.na(data$Y[data$clc_quartier == "harly"])]

# pour tous les points
c = st_transform(st_as_sf(data, coords =c("X", "Y"), crs=3949), crs=4326)

# pour un quartier en particulier
coo <- data.frame(id= c(1:length(data$X[which(data$clc_quartier == "harly")])), x= data$X[which(data$clc_quartier == "harly")], 
                   y=data$Y[which(data$clc_quartier == "harly")]) %>%
  st_as_sf(coords=c("x", "y"), crs=3949) %>%
  st_transform(4326) 


  map = leaflet(coo) %>%
  addTiles() %>%
  addCircles(color="red")



# ------

new_points <- data.frame(id= c(1:length(data$X[which(data$clc_quartier == "quartier de neuville")])), x= data$X[which(data$clc_quartier == "quartier de neuville")],
                  y=data$Y[which(data$clc_quartier == "quartier de neuville")]) %>%
  st_as_sf(coords=c("x", "y"), crs=3949) %>%
  st_transform(4326) 


map %>%
  addCircles(data = new_points, color="green")







View(coo)

 # ------------
# head(c)
# head(data)
# print(dim(c))
# print(dim(data))


# l = data[which(data$clc_quartier == "harly",)]
# View(l)

