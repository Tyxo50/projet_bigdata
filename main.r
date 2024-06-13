source('function.r')
data = read.csv(file="lesaintcsv2.csv", sep = ",")
# Cleaning data part
print(paste("Data dim start",dim(data)))
print(dim(data))
data = to_utf8(data)
data = to_lower(data)
data = convert_data(data)
data = put_na_if_empty(data)
data = remove_na_on_x_y(data)
data = remove_doublon(data)
data = clc_zone_indus(data)
data = remove_empty_line13(data)
data = remove_na_feuillage(data, "feuillage")

data = get_quartier_from_coords(data)
data = convert_data(data)
data = to_factor(data)

data = predict_tronc_diam(data)
data = predict_age(data)
data = predict_remarquable(data)

lien_variable_quantitative(data)
lien_variable_qualitative(data)
lien_variable_quan_qual(data)


print(dim(data))
print(dim(data))
View(data)
