install.packages("class")

library(class)

library(data.table)

training <- as.data.frame(fread(input = "Dtrainingmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training$clase <- clase ### despues de escalar, hago que la columna clase vuelva a los valores normales


knn.cv(training, cl = clase , k = 1, l = 0, prob = TRUE, use.all = TRUE)

