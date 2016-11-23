
#####################################################################################

#               GRAFICOS DE ANALISIS DE COMPONENTES PRINCIPALES             

#####################################################################################


library(data.table)

datos <- "Dtrainingmiristoil.csv"  ## aca va el nombre del archivo con los datos

training <- as.data.frame(fread(input = datos, check.names = TRUE)) #leo el archivo con mis descriptores 

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

training$clase <- as.factor(training$clase) ## me aseguro que la columna clase este guardada como factor

res.pca <- prcomp(x= training[, -1] , center = TRUE, scale. = TRUE) ## hago componentes principales. Escalo y centro los datos, dado que sino me va a dar diferente resultado

plot(res.pca, type ="l") ### me grafica la varianza en funcion de la cantidad de componentes

library(factoextra) ## cargo el paquete factoextra, que contiene las siguientes funciones para graficar

fviz_pca_ind(res.pca, geom="point") ## grafico todos los puntos, para ver la distribucion rustica

plot1 <- fviz_pca_ind(res.pca, label="none", habillage=training$clase, addEllipses=TRUE, ellipse.level=0.95, ellipse.type = "convex" , pointsize = 2 ) ### grafico en los componentes principales pero diferenciando los grupos  de activos y inactivos, juntandolos con hull

plot1 ## imprimo plot1

plot2 <- fviz_pca_ind(res.pca, label="none", habillage=training$clase, addEllipses=TRUE, ellipse.level=0.95, ellipse.type = "norm" , pointsize = 2 ) ### grafico en los componentes principales pero diferenciando los grupos  de activos y inactivos, juntandolos con elipses

plot2 # imprimo plot2

plotly1 <- ggplotly(plot1) ## lo vuelvo interactivo al plot1

plotly1

plotly2 <- ggplotly(plot2) ## lo vuelvo interactivo al plot1

plotly2
