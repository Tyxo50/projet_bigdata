source('function.r')
data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")
# Cleaning data part
print(paste("Data dim start",dim(data)))

data = to_utf8(data)
data = convert_data(data)
data = put_na_if_empty(data)


data = remove_na_on_x_y(data)
data = remove_outliers_age_estime_2(data)
data = data[rowSums(is.na(data)) < 13, ]
data <- remove_na_feuillage(data, "feuillage")
print(dim(data))
data = remove_empty_line13(data)
#data = all_str_to_lower(data)
data = to_factor(data)
print(dim(data))
data = remove_na_on_x_y(data)
data = remove_empty_line13(data)
data <- remove_na_feuillage(data, "feuillage")




print(dim(data))
print(dim(data))
View(data)
