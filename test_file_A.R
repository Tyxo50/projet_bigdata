data = read.csv(file="Patrimoine_Arbore.csv", sep = ",")


to_utf8 <- function(data) {
    for (identifier in colnames(data)) {
        data[[identifier]] = iconv(data[[identifier]], from = "latin1", to = "UTF-8")
    }
    return(data)
}

convert_data <- function(data) {
    for (identifier in colnames(data)) {
        if (is.numeric(data[[identifier]])) {
            data[[identifier]] = as.numeric(data[[identifier]])
        }
    }
    return(data)
}
data = convert_to_integer(data)

print(summary(data))


