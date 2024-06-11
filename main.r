source('function.r')
data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")
# Cleaning data part
print(paste("Data dim start",dim(data)))

data = to_utf8(data)
data = convert_data(data)
data = put_na_if_empty(data)
data = remove_na_on_x_y(data)
data = remove_empty_line13(data)
data <- remove_na_feuillage(data, "feuillage")




print(dim(data))
print(dim(data))
View(data)
