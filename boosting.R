is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("gbm") == FALSE) {install.packages("gbm")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table)

library(gbm) # cargo el paquete gbm que tiene la funcion para hacer boosting con trees

library(pROC) ## cargo el paquete pROC

training <- as.data.frame(fread(input = "Dtrainingmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

test <- as.data.frame(fread(input = "Dtestmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del test set

set.seed(2) ## seteo la semilla

boosting.cv <- gbm(formula = clase~ ., distribution = "bernoulli" , data = training,  n.trees = 5000 , cv.folds = 10 , shrinkage = 0.01 , interaction.depth = 1 , class.stratify.cv =TRUE , verbose = TRUE) ### aca hago el boosting en si. A su vez hace cross-validation con un k = 10. Esta cross-validation despues es la usada por la funcion gbm.perf para encontrar el numero optimo de arboles

num.arboles.optimo.cv <- gbm.perf(object = boosting.cv , plot.it = TRUE , oobag.curve = FALSE , method = "cv") ## me dice cual es el numero de arboles optimos segun el cross-validation que plantie hacer en la funcion gbm

predicciones.train <-  predict.gbm(object = boosting.cv , newdata = training , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action = NULL ) ### predicciones en el training, como type es response el resultado me da como probabilidad

auc.training <- auc(roc(predictor = predicciones.train,response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <-  predict.gbm(object = boosting.cv , newdata = test , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action =NULL) ### predicciones en el test, como type es response el resultado me da como probabilidad

auc.test <- auc(roc(predictor= predicciones.test, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por Boosting", boosting.cv , "Nº de arboles Òptimo",num.arboles.optimo.cv , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman


