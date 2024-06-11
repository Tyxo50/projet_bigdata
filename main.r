source('function.r')

data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")

print(summary(data))
# Cleaning data part
print(paste("Data dim start",dim(data)))
data = put_na_if_empty(data)
data = to_utf8(data)
data = convert_data(data)
data = remove_na_on_x_y(data)
data = remove_outliers_age_estime_2(data)
data = data[rowSums(is.na(data)) < 13, ]
data = remove_ras(data, "nomfrançais")
return(data)
print(dim(data))
print(dim(data))

View(data)
