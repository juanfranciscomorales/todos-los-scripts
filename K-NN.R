
is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(caret)

library(data.table)

library(pROC)

training.set  <- "training set curado.csv"  ### nombre del archivo con el training set

test.set <- "test set curado.csv"  ### nombre del archivo con el test set

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

clase <- training$clase ## guardo los valores de clase para despues

training$clase <- as.factor(make.names(training$clase)) ## hago que la columna clase sea de tipo factor. Tambien hago que los nombres de los factores sean validos para usar en la funcion train

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

test[is.na(test)] <- 0 ### con esto lo que hago es reemplazar los NA por ceros para poder hacer las predicciones, porque sino me tira error



## seteo para la cross validation

ctrl <- trainControl(method="repeatedcv",# aca armo el elemento para optimizar el valor de K. El metodo es cross-validation
                     
                     number = 10 , # el numero de k-fold lo seteo en 10, dado que en el curso nos dijieron que era el mejor para optimizar
                     
                     repeats = 5  # el numero de veces que se repite el cross validation para que el resultado no sea sesgado

                   # , classProbs=TRUE, # le digo que me devuelva la probabilidad para cada clase 
                     
                  # summaryFunction = twoClassSummary ##  con esto hago que la seleccion del mejor modelo sea por curva ROC
                     
                        )

# PUEDO EN LA FUNCION trainControl anular los argumentos classProbs y summaryFunction
# y ver cual es el valor optimo de k pero en vez de curva ROC sino por accuracy




## con esto seteo la busqueda para seleccionar los parametros optimos

knnGrid <-  expand.grid(  ## con esto lo que voy a hacer es decir el barrido que va a hacer la funcion para optimizar los siguientes parámetros de gbm
        
                k = 1:50 ) # que pruebe k vecinos desde 1 a 50, para luego ver cual es el optimo




### Entreno el modelo por knn y optimizo los valores

knnFit <- train(clase ~ .,## uso la funcion train del paquete caret para hacer knn. en esta linea especifico cual es el valor a predecir y cuales son las variables independientes. 
                
                data = training, ## le digo cuales son mis datos para armar el modelo
                
                method = "knn", ## aca le digo que use knn para armar el modelo
                
                trControl = ctrl,  ## le digo que use el elemento ctrl para optimizar el modelo
                
                preProcess = c("center","scale"), ## aca le digo que me centre y me escale los datos antes de armar el modelo
                 
                tuneGrid = knnGrid )  ## con esto le digo que pruebe desde k = 1 hasta k = 50, y con el cross validation que puse en trcontrol va a elegir el optimo k 

knnFit

plot(knnFit)

library(pROC)

predicciones.train <- predict(knnFit, newdata = training , type = "prob") ## hago la prediccion en el training set obteniendo los resultados como probabilidad

names(predicciones.train) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.training <- auc(roc(predictor = predicciones.train$Activo,response = clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(knnFit, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad



#  ME TIRA ERROR, PORQUE HAY NA EN EL TEST SET

# SI PONGO na.omit  ME PREDICE PERO MENOR CANT DE FILAS, ENTONCES CUANDO QUIERO HACER LA CURVA ROC DEL TEST SET ME TIRA ERROR



names(predicciones.test) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.test <- auc(roc(predictor= predicciones.test$Activo, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado.knn <- list("Modelo armado por knn", knnFit ,"Numero de k Óptimo",  knnFit$finalModel , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.knn


