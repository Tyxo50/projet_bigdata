data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")


data = iconv(data, from = "latin1", to = "UTF-8")

print(head(data))






