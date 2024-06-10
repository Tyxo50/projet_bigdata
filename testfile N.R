data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")


data = iconv(data, from = "latin1", to = "UTF-8")

print(head(data))

summary(data)

#nettoyage des données 

#trouver les colonnes avec des valeurs manquantes 

list_na <- colnames(data)[ apply(data, 2, anyNA) ]
list_na

# obtention mediane pour remplacement 

median_missing <- apply(data[,colnames(data) %in% list_na],2,median,na.rm =  TRUE)

# remplacement des valeurs manquantes par mediane 

data_replace <- data 

for (col in list_na) {
  # parcours les elem de la colonne 
  for (i in 1:nrow(data_replace)) {
    if (is.na(data_replace[i, col])) {
      # si la valeur manque on remplace par la mediane
      data_replace[i, col] <- median_missing[col]
    } else {
      # sinon on garde la valeur d'origine 
      data_replace[i, col] <- data[i, col]
    }
  }
}

head(data_replace)


# suppression des doublons 

# duplicated renvoie les doublons et data_unique est egale a data sans les doublons trouvés par duplicated 
data_unique <- data[!duplicated(data), ]

print(data_unique

# gestion des valeurs aberrantes 

remplace_abberant_par_mediane <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  outliers <- x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
  median_value <- median(x, na.rm = TRUE)
  x[outliers] <- median_value
  return(x)
}




