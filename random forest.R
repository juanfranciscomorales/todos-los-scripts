###### RANDOM FOREST CLASIFICATORIO #######

random.forest <- function(training ="Descriptores Training Set Sofi.csv", test="Descriptorres DUDE2 Sofi.csv",cant.arboles = 500 ) {
        
is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no
        
if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("randomForest") == FALSE) {install.packages("randomForest")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table) # cargo data.table

library(randomForest) # cargo el paquete random forest

library(pROC) ## cargo el paquete pROC

set.seed(125) ## seteo la semilla para que sea reproducible el random forest

training <- as.data.frame(fread(input = training, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

training$clase <- as.factor(training$clase) # hago que la clase sea factor 

names(training) <- make.names(names(training))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

training <- training[, -which(names(training) == names(training)[duplicated(names(training))])]  ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

names(test) <- make.names(names(test))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

test <- test[, -which(names(test) == names(test)[duplicated(names(test))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

rf <- randomForest(clase ~ .,data=training,importance=TRUE, do.trace = FALSE, ntree = cant.arboles)## lo que hacemos aca es random forest

plot(rf, main = "Gráfico OOB error Random Forest") ## hago que grafique el error versus el numero de arboles en el training set

legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf, n.var=10, main= "Importancia de variables en RF")

predicciones.train <- predict(object = rf, newdata = training, type="prob") ## predicciones en el training set expresadas como probabilidad

auc.training <- auc(roc(predictor = predicciones.train[,2],response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = rf, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad

auc.test <- auc(roc(predictor= predicciones.test[,2], response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por Random Forest", rf , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test)

resultado

}
