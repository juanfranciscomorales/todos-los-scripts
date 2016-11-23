#####################################################


### PARTIAL LEAST SQUARES - DISCRIMINAL ANALYSIS ##

   #                    PLS-DA                  #

#####################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("mixOmics") == FALSE) {install.packages("mixOmics")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

#####    LECTURA DE DATOS Y LIMPIEZA DE DATOS     #################################

training.set <- "Dtrainingmiristoil.csv" # nombre del archivo con el training set

test.set <- "Dtestmiristoil.csv" # nombre del archivo con el test set

library(data.table)

library(mixOmics)

library(pROC)

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training <- training[,-c(1,2)] # elimino las columnas clase y nombre, dejo solo las columnas con los descriptores

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza$Position] ## elimino las columnas que tiene varianza cercana a cero

training.matrix <- as.matrix(training) ### lo transformo a matrix

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test.matrix <- as.matrix(test) ## lo transformo en matrix

test.matrix.limpia <- test.matrix[,colnames(test.matrix)[colnames(test.matrix) %in% colnames(training.matrix)]] ### hago que solo queden las columnas que estaban en el training, porque sino tira error cuando quiero predecir

class(test.matrix.limpia) <- "numeric" ### no se porque en el paso anterior se me transforman los datos a caracter, entonces los vuelvo numeric otra vez en este paso


##############  "mixOmics"  - PLSDA en si #############################


num.comp <- 50 # el numero de componentes maximo a agregar en el modelo, luego me fijo cual es el valor optimo

PLS.DA <- plsda(X=training.matrix , Y = clase , scale = TRUE, ncomp = num.comp , near.zero.var = TRUE) ## hago PLS-DA, pero no me elije el mejor por cross-validation, tengo que decirle la cantidad de componentes a incluir

PLS.DA.performance <- perf( object = PLS.DA , validation = "Mfold" , folds = 10 , progressBar = TRUE , nrepeat = 5 , auc = TRUE ) ### hago que me estime la performance de mi PLS-DA, por cross-validation con k = 10

plot(PLS.DA.performance , dist = "max.dist") ## me grafica la performance en base a el error calculado como la maxima distancia, que se obtuvo por el crossvalidation en el paso anterior

plot(PLS.DA.performance , dist = "centroids.dist") ## me grafica la performance en base a el error calculado como la distancia de los centroides, que se obtuvo por el crossvalidation en el paso anterior

plot(PLS.DA.performance , dist = "mahalanobis.dist")# me grafica la performance en base a el error calculado como mahalanobis, que se obtuvo por el crossvalidation en el paso anterior




#### **************************************************************####


### CON LOS GRAFICOS ANTERIORES DECIDO CUAL ES EL MEJOR NUMERO DE COMPONENTES 

## Y ESE LO COLOCO EN LA SIGUIENTE VARIABLE






num.comp.optimo <- 11  ## aqui coloco el numero de componentes optimo

PLS.DA.optimo <- plsda(X=training.matrix , Y = clase , scale = TRUE, ncomp = num.comp.optimo ) ## hago PLS-DA, pero no me elije el mejor por cross-validation, tengo que decirle la cantidad de componentes a incluir

plotVar(PLS.DA.optimo) ## me grafica la importancia de las variables, o algo asi

plotIndiv(PLS.DA.optimo) ## me grafica las muestras en el espacio de los 2 componentes que mejor variabilidad tienen

plotIndiv(object = PLS.DA.optimo ,  ind.names = FALSE ,pch = 21,  ellipse = TRUE , ellipse.level = 0.95 , centroid = TRUE , star = TRUE , style = "ggplot2" , title = "Gráfico de Individuos en 2D - PLSDA" , col.per.group =c("blue" , "green") ) ## me grafica las muestras en el espacio de los 2 componentes que mejor variabilidad tienen pero con elipses y unidas al centroide

plotIndiv(object = PLS.DA.optimo ,  ind.names = FALSE ,pch = "tetra" ,  ellipse = TRUE , ellipse.level = 0.95 , centroid = TRUE , star = TRUE , style = "3d" , col.per.group =c("blue" , "green")) ## me grafica las muestras en el espacio de los 3 componentes que mejor variabilidad tienen

predicciones.train <-  predict(object = PLS.DA.optimo , newdata = training.matrix , dist = "max.dist") ### predicciones en el training, como type es response el resultado me da como probabilidad

auc.training <- auc(roc(predictor = predicciones.train,response = clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <-  predict( object = PLS.DA.optimo , newdata = test.matrix.limpia)  ### predicciones en el test, como type es response el resultado me da como probabilidad

auc.test <- auc(roc(predictor= predicciones.test, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por PLS-DA", PLS.DA.optimo , "Nº de Componentes Óptimo", num.comp.optimo , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado


##################  PERFOMANCE EN EL TEST SET          ######################



predicciones.test <- predict( object = PLS.DA.optimo , newdata = test.matrix.limpia) ## hago la prediccion en si


