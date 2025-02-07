source('function.r')

data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")

print(summary(data))
# Cleaning data part
print(paste("Data dim start",dim(data)))
data = to_utf8(data)
data = convert_data(data)
data = remove_na_on_x_y(data)
print(dim(data))
View(data)

print(sum(is.na(data$created_date)))