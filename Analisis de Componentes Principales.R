
library(data.table)

training <- as.data.frame(fread(input = "Dtrainingmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training$clase <- clase ### despues de escalar, hago que la columna clase vuelva a los valores normales

library(factoextra) ## cargo el paquete factoextra

res.pca <- prcomp(x= training[, -1] , center = TRUE, scale. = TRUE) ## hago componentes principales

plot(res.pca, type ="l") ### me grafica la varianza en funcion de la cantidad de componentes

fviz_pca_ind(res.pca, geom="point") ## grafico todos los puntos, para ver la distribucion rustica

p <- fviz_pca_ind(res.pca, label="none", habillage=training$clase, addEllipses=TRUE, ellipse.level=0.95, ellipse.type = "convex" , pointsize = 2 , alpha.ind = 0.1) ### grafico en los componentes principales pero diferenciando los grupos  de activos y inactivos

print(p) ### imprimo el grafico armado en el paso anterior

