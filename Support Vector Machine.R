# Support Vector Classifier

library(data.table)

training.set <- "Training set sara.csv"

test.set <- "Test set sara.csv"

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

training$clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

library(e1071) ## cargo el paquete que tiene la funcion svm

set.seed(1) ## seteo la semilla

tune.out <- tune( svm , clase~., data = training , kernel = "radial" , type = "C-classification",  scale=TRUE , probability =TRUE , ranges= list( gamma=2^(-15:3) , cost=2^(-5:15) ) , tunecontrol = tune.control(sampling = "cross", cross= 10))## tuneo los parametros para encontrar el que da menor error

summary(tune.out) ## e imprime un sumario con los resultados del tuneo

bestmod <- tune.out$best.model### me tira cual es el mejor modelo segun la funcion de tuneo

summary(bestmod) ## un sumario del mejor modelo segun la funcion tune

svmfit <- svm(clase~. , data = training , kernel ="radial" , type="C-classification" ,  scale =TRUE , probability =TRUE ,na.action = na.omit, gamma= bestmod$gamma , cost = bestmod$cost , cross = 10) ### armo el modelo con los mejores parametros segun el tuneo y le hago un Leave One Out Cross-Validation

svmfit

summary(svmfit)

table(true= training$clase ,  pred=predict(svmfit , training)) ## tabla de confusion en el training set

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

predict.test <- predict(object = svmfit, newdata = test, probability = TRUE , na.action = na.omit) ### predigo en el test set

## ME TIRA ERROR PORQUE TENGO NA EN COLUMNAS QUE DEBEN CONTENER DATOS #####

table(true=test$clase, pred= predict.test)


