source('function.r')
# data <- read.csv(file="lesaintcsv2.csv", sep = ",")
# Cleaning data part
# print(paste("Data dim start",dim(data)))
# print(dim(data))
# data = to_utf8(data)
# data = to_lower(data)
# data = convert_data(data)
# data = put_na_if_empty(data)
# data = remove_na_on_x_y(data)
# data = remove_doublon(data)
# data = clc_zone_indus(data)
# data = remove_empty_line13(data)
# data = remove_na_feuillage(data, "feuillage")
#
# data = get_quartier_from_coords(data)
# data = convert_data(data)
# data = to_factor(data)
#
# data = predict_tronc_diam(data)
# data = predict_age(data)
# data = predict_remarquable(data)

lien_variable_quantitative(data)
lien_variable_qualitative(data)
lien_variable_quan_qual(data)


print(dim(data))
print(dim(data))
View(data)


import_csv_avec_api <- function (){
  data <- read.csv(file="Patrimoine_Arbore.csv", sep = ",")
  data <- to_utf8(data)
  data <- to_lower(data)
  data <- convert_data(data)
  data <- put_na_if_empty(data)
  data <- remove_na_on_x_y(data)
  data <- remove_doublon(data)
  data <- clc_zone_indus(data)
  data <- remove_outliers_age_estime_2(data)
  data <- remove_empty_line13(data)
  data <- remove_na_feuillage(data, "feuillage")

  data <- get_quartier_from_coords(data)
  data <- convert_data(data)
  data <- to_factor(data)

  return(data)
}

import_csv_propre <- function (){
  data <- read.csv(file="lesaintcsv4.csv", sep = ",")
  #data$X.1 <- NULL
  data <- to_lower(data)
  data <- convert_data(data)
  data <- to_factor(data)

  return(data)
}

data <- import_csv_avec_api()
#data <- import_csv_propre()

# Decommenter pour voir après avoir prepare le dataframe data.
# display_map_arbres_par_quartier(data)
# display_map_arbres_par_feuillage(data)
# display_map_arbres_par_stadedev(data)
