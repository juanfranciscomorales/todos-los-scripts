training.set  <- "Dtrainingmiristoil.csv"  ### nombre del archivo con el training set

test.set <- "Dtestmiristoil.csv"  ### nombre del archivo con el test set

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table) # cargo data.table

library(caret) # cargo el paquete random forest

library(pROC) ## cargo el paquete pROC

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

training$clase <- as.factor(training$clase) # hago que la clase sea factor 

names(training) <- make.names(names(training))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(training))) > 0 ) {  ## hago un if para que me elimine en caso de que haya algun nombre de columna repetida
        
        training <- training[, -which(names(training) == names(training)[duplicated(names(training))])]  ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

names(test) <- make.names(names(test))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(test))) > 0 ) { ## hago un if para eliminar las columnas que tienen nombre repetido
        
        test <- test[, -which(names(test) == names(test)[duplicated(names(test))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}



## seteo para la cross validation



ctrl <- trainControl(method="repeatedcv",# aca armo el elemento para optimizar el valor de K. El metodo es cross-validation
                     
                     number = 10 , # el numero de k-fold lo seteo en 10, dado que en el curso nos dijieron que era el mejor para optimizar
                     
                     repeats = 3  # el numero de veces que se repite el cross validation para que el resultado no sea sesgado
                     
                  #  , classProbs=TRUE , # le digo que me devuelva la probabilidad para cada clase 
                     
                   #  summaryFunction = twoClassSummary ##  con esto hago que la seleccion del mejor modelo sea por curva ROC
                     
)


## con esto seteo la busqueda para seleccionar los parametros optimos



sqtmtry <- round(sqrt(ncol(training) - 1)) ## raiz cuadrada como opcion de mtry

rfGrid <- expand.grid(mtry = c(round(sqtmtry / 2), sqtmtry, 2 * sqtmtry)) ## planteo cuales son los mtry que voy a probar

cant.arboles <- 1000



### Entreno el modelo por gbm y optimizo los valores

set.seed(1)

ptm <- proc.time()

rffit <- train(clase ~ .,## uso la funcion train del paquete caret para hacer knn. en esta linea especifico cual es el valor a predecir y cuales son las variables independientes. 
                
                data = training, ## le digo cuales son mis datos para armar el modelo
                
                method = "rf", ## aca le digo que use knn para armar el modelo
                
                trControl = ctrl,  ## le digo que use el elemento ctrl para optimizar el modelo
                
                importance = TRUE , ## ## le digo que me calcule la importancia de las variables
               
               ntree = cant.arboles , ## le digo el numero de arboles a armar 
               
               do.trace = TRUE, ### con esto le digo que me vaya diciendo como va la corrida

               proximity = TRUE , ## con esto le digo que me calcule la proximidad. La proximidad es la cantidad de veces que caen en la misma hoja 2 compuestos
               
               tuneGrid = rfGrid ) ## con esto le digo que me pruebe los mtry planteados
               
               proc.time() - ptm

rffit

plot(rffit$finalModel, main = "Gráfico OOB error Random Forest") ## hago que grafique el error versus el numero de arboles en el training set

legend("top", colnames(rffit$finalModel$err.rate),col=1:4,cex=0.8,fill=1:4) ## con esto hago que aparezcan las referencias en el grafico anterior

varImpPlot(rffit$finalModel, n.var=10, main= "Importancia de variables en RF") ## grafico la importancia de las variables segun Random Forest

MDSplot(rf = rffit$finalModel,fac = training$clase , k =3) # Plot the scaling coordinates of the proximity matrix from randomForest. Hace escalado multidimensional de la matrix de proximidad. es una forma de ver si me separa las clases

