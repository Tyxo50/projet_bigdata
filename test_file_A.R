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



View(data)

View(data2)

data3 = data2
data3$haut_tot[is.zero(data3$haut_tot)] = mean(data3$haut_tot, na.rm = TRUE)

model = lm(formula = )