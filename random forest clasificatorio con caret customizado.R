###########################################


## RANDOM FOREST CLASIFICATORIO  ##


###########################################








is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no
        
if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("randomForest") == FALSE) {install.packages("randomForest")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("parallel") == FALSE) {install.packages("parallel")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("doParallel") == FALSE) {install.packages("doParallel")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(parallel) ##  paquete para hacer paralelizacion 

library(doParallel) ##  paquete para hacer paralelizacion 

library(data.table) # cargo data.table

library(randomForest) # cargo el paquete random forest

library(pROC) ## cargo el paquete pROC

library(caret) ## cargo el paquete caret


training.set  <- "Training Sofia.csv"  ### nombre del archivo con el training set

test.set <- "Test Sofia.csv"  ### nombre del archivo con el test set

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

training$clase <- as.factor(make.names(training$clase)) ## hago que la columna clase sea como factor y con nombres validos para poder hacer que el boosting sea clasificatorio

#names(training) <- make.names(names(training))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

#if (sum(duplicated(names(training))) > 0 ) {  ## hago un if para que me elimine en caso de que haya algun nombre de columna repetida

#training <- training[, -which(names(training) == names(training)[duplicated(names(training))])]  ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

#}



test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

#names(test) <- make.names(names(test))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

#if (sum(duplicated(names(test))) > 0 ) { ## hago un if para eliminar las columnas que tienen nombre repetido

#test <- test[, -which(names(test) == names(test)[duplicated(names(test))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

#}



#########################################

## CONFIGURACION PARA HACER PARALELIZACION ##

#########################################


#set.seed(1)

## seteo para poder hacer calculos en paralelo

#cores <- detectCores() ## con esta funcion obtengo el numero de nucleos de la compu

#cls = makeCluster(cores) # Creates a set of copies of R running in parallel and communicating over sockets.

#registerDoParallel(cls) ## The registerDoParallel function is used to register the parallel backend with the foreach package.


## genero una lista de seeds para poder hacer reproducible el ejemplo
## lista tiene 1000 elementos con un vector de 1000 dentro de cada elemento
## lo hago enorme por las dudas, ademas si sobran seeds no hay drama


#seeds <- vector(mode = "list", length = 1000) ## creo una lista de largo 1000

#for(i in 1:length(seeds)) seeds[[i]] <- sample.int(1000, 1000) ## hago que cada elemento de la lista este compuesto por un vector con 1000 enteros.






###################################

# create a "new" algorithm for caret to support.

# This is the same random forest algorithm you are using, only modified so that it supports multiple tuning of multiple parameters.

#####################################





customRF <- list(type = "Classification", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        
        randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
                        predict(modelFit, newdata)

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) 
                        predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]

customRF$levels <- function(x) x$classes




###############################################################################



#       BUSQUEDA DE PARAMETROS OPTIMOS 


# ##########################################################################



set.seed(1)


## seteo para la cross validation

rf_ctrl <- trainControl(method="adaptive_cv",# aca armo el elemento para optimizar el valor de K. El metodo es cross-validation
                         
                         allowParallel = TRUE, # con esto le digo que si puede hacer calculos en paralelo lo haga
                         
                         #seeds = seeds, # con esto seteo las seeds para que cuando se use paralelizacion sea reproducible
                         
                         verboseIter = TRUE , ### con esto le digo que me imprima la evolucion de la busqueda de los parametros optimos
                         
                         number = 10 , # el numero de k-fold lo seteo en 10, dado que en el curso nos dijieron que era el mejor para optimizar
                         
                         repeats = 5 , # el numero de veces que se repite el cross validation para que el resultado no sea sesgado
                         
                         classProbs=TRUE , # le digo que me devuelva la probabilidad para cada clase 
                         
                         adaptive = list(min = 5, ## is the minimum number of resamples that will be used for each tuning parameter. 
                                         
                                         alpha = 0.05, ## is a confidence level that is used to remove parameter settings. To date, this value has not shown much of an effect.
                                         
                                         method = "gls", ## is either "gls" for a linear model or "BT" for a Bradley-Terry model. The latter may be more useful when you expect the model to do very well (e.g. an area under the ROC curve near 1) or when there are a large number of tuning parameter settings.
                                         
                                         complete = TRUE)  ##is a logical value that specifies whether train should generate the full resampling set if it finds an optimal solution before the end of resampling. If you want to know the optimal parameter settings and don't care much for the estimated performance value, a value of FALSE would be appropriate here.
                         
                         # , summaryFunction = twoClassSummary ##  con esto hago que la seleccion del mejor modelo sea por curva ROC
                         
)



## con esto seteo la busqueda para seleccionar los parametros optimos


ncol <- ncol(training) - 1 ## numero de columnas


rf_grid <-  expand.grid(  ## con esto lo que voy a hacer es decir el barrido que va a hacer la funcion para optimizar los siguientes parámetros
        
        .ntree= seq(from = 50 , to = 3500 , by = 50), ## este parametro es para ver el numero optimo de arboles

        .mtry = c( seq( from = max( 1 , floor(sqrt(ncol)) - 5) , to = floor(sqrt(ncol)) + 5 , by = 1)  , floor(ncol/3) ) # Number of variables randomly sampled as candidates at each split.
        
        ) 




### Entreno el modelo por Random Forest y optimizo los valores


ptm <- proc.time()


rf <- train(clase ~ .,## uso la funcion train del paquete caret para hacer knn. en esta linea especifico cual es el valor a predecir y cuales son las variables independientes. 
                    
                data = training, ## le digo cuales son mis datos para armar el modelo
                    
                metric = "Accuracy" , # A string that specifies what summary metric will be used to select the optimal model. By default, possible values are "RMSE" and "Rsquared" for regression and "Accuracy" and "Kappa" for classification. If custom performance metrics are used (via the summaryFunction argument in trainControl, the value of metric should match one of the arguments. If it does not, a warning is issued and the first metric given by the summaryFunction is used. (NOTE: If given, this argument must be named.)    
            
                method = customRF, ## aca le digo que use knn para armar el modelo
                    
                trControl = rf_ctrl,  ## le digo que use el elemento ctrl para optimizar el modelo
                    
                tuneGrid = rf_grid , ## hago pasar el elemento rf_grid para probar y encontrar cuales son los valores optimos 
                   
                importance = TRUE , # Should importance of predictors be assessed?
            
                proximity = TRUE) ## le digo que calcule la proximidad de las medidas para poder luego graficar el MDSplot 



proc.time() - ptm



#stopCluster(cls) ## cierro el cluster con el que hice la paralelizacion asi no tengo problemas de configuracion







plot(rf) ## hago que grafique el error versus el numero de arboles en el training set

varImpPlot( rf$finalModel , n.var=10, main= "Importancia de variables en RF") ## grafico la importancia de las variables segun Random Forest

MDSplot(rf = rf$finalModel ,fac = training$clase , k =3) # Plot the scaling coordinates of the proximity matrix from randomForest. Hace escalado multidimensional de la matrix de proximidad. es una forma de ver si me separa las clases

predicciones.train <- predict(object = rf, newdata = training, type="prob") ## predicciones en el training set expresadas como probabilidad

auc.training <- auc(roc(predictor = predicciones.train[,2],response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = rf, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad

auc.test <- auc(roc(predictor= predicciones.test[,2], response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado.rf <- list("Modelo armado por Random Forest", rf , "Resultado K-fold CV" , getTrainPerf(rf) , "AUC ROC Training" , auc.training , "AUC ROC Test" , auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.rf









######### GRAFICOS PARA ANALIZAR LOS RESULTADOS - TRAINING SET ############




predicciones.train <- as.data.frame(predict(rf, newdata = training, type="prob" , na.action = na.pass))  ## predicciones en el train set expresadas como probabilidad

names(predicciones.train) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.train$Activo, labels = training$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Training set") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff - Training set") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff - Training set") ## grafico de PPV versus punto de corte

plot(performance(predicciones , measure = "acc" , x.measure = "cutoff"), main ="Accuracy vs cutoff - Training set") ## grafico de accuracy versus punto de corte



library(ggplot2)

library(plotly)


performance.mat.cutoff <- performance(predicciones , measure = "mat" , x.measure = "cutoff")

df.mat.cutoff <- data.frame(performance.mat.cutoff@x.values , performance.mat.cutoff@y.values)

colnames(df.mat.cutoff) <- c( "cutoff" , "mat")



performance.ppv.cutoff <- performance(predicciones , measure = "ppv" , x.measure = "cutoff")

df.ppv.cutoff <- data.frame(performance.ppv.cutoff@x.values , performance.ppv.cutoff@y.values)

colnames(df.ppv.cutoff) <- c( "cutoff" , "ppv")



performance.acc.cutoff <- performance(predicciones , measure = "acc" , x.measure = "cutoff")

df.acc.cutoff <- data.frame(performance.acc.cutoff@x.values , performance.acc.cutoff@y.values)

colnames(df.acc.cutoff) <- c( "cutoff" , "acc")



performance.sens.cutoff <- performance(predicciones , measure = "sens" , x.measure = "cutoff")

df.sens.cutoff <- data.frame(performance.sens.cutoff@x.values , performance.sens.cutoff@y.values)

colnames(df.sens.cutoff) <- c( "cutoff" , "sens")



performance.spec.cutoff <- performance(predicciones , measure = "spec" , x.measure = "cutoff")

df.spec.cutoff <- data.frame(performance.spec.cutoff@x.values , performance.spec.cutoff@y.values)

colnames(df.spec.cutoff) <- c( "cutoff" , "spec")



performance.npv.cutoff <- performance(predicciones , measure = "npv" , x.measure = "cutoff")

df.npv.cutoff <- data.frame(performance.npv.cutoff@x.values , performance.npv.cutoff@y.values)

colnames(df.npv.cutoff) <- c( "cutoff" , "npv")



grafico <- ggplot () + 
        
        geom_line(data = df.mat.cutoff , aes(x = cutoff, y = mat , color = "mat" ) , size = 1) + 
        
        geom_line(data = df.ppv.cutoff , aes(x = cutoff, y = ppv , color = "ppv" ) , size = 1) + 
        
        geom_line(data = df.acc.cutoff , aes(x = cutoff, y = acc , color = "acc" ) , size = 1) +
        
        geom_line(data = df.sens.cutoff , aes(x = cutoff, y = sens , color = "sens" ) , size = 1) +
        
        geom_line(data = df.spec.cutoff , aes(x = cutoff, y = spec , color = "spec" ) , size = 1) +
        
        geom_line(data = df.npv.cutoff , aes(x = cutoff, y = npv , color = "npv" ) , size = 1) +
        
        scale_colour_manual(values = c("red", "blue", "green" , "violet" , "yellow" , "orange")) +
        
        ggtitle("Training set") +
        
        labs(colour = "Variable", y = NULL) 


ggplotly(grafico)











######### GRAFICOS PARA ANALIZAR LOS RESULTADOS - TEST SET ############











test <- "Test Sofia.csv" ### nombre del test set

test <- as.data.frame(fread(input = test , check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

predicciones.test <- as.data.frame(predict(rf, newdata = test, type="prob"))  ## predicciones en el test set expresadas como probabilidad

######################
##### lo siguiente lo hago porque se me reduce el numero de predicciones. en vez de ponerme NA en el tet set predicho, me disminuye el numero de valores predichos
##### por lo tanto lo que hago es agregar NA a las filas que son eliminadas para poder luego seguir trabajando

un1 <- union(rownames(test), rownames(predicciones.test)) ## creo un vector que me dice que filas estan en ambos dataframes

d1  <- as.data.frame(matrix(NA, ncol = ncol(predicciones.test), nrow = length(un1), dimnames = list(un1, names(predicciones.test)))) # creo un dataframe de solos NA con los nombres y las dimensiones como el dataframe predicciones.test

d1[rownames(d1) %in% rownames(predicciones.test),] <- predicciones.test ## reemplazo los NA por los valores que si tengo en predicciones.test

predicciones.test <- d1 ## cambio el nombre para poder seguir trabajando

###### esta seccion de agregar NA termina aca, sigue normalmente ahora
###################
names(predicciones.test) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test$Activo, labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte

plot(performance(predicciones , measure = "acc" , x.measure = "cutoff"), main ="Accuracy vs cutoff") ## grafico de accuracy versus punto de corte



library(ggplot2)

library(plotly)


performance.mat.cutoff <- performance(predicciones , measure = "mat" , x.measure = "cutoff")

df.mat.cutoff <- data.frame(performance.mat.cutoff@x.values , performance.mat.cutoff@y.values)

colnames(df.mat.cutoff) <- c( "cutoff" , "mat")



performance.ppv.cutoff <- performance(predicciones , measure = "ppv" , x.measure = "cutoff")

df.ppv.cutoff <- data.frame(performance.ppv.cutoff@x.values , performance.ppv.cutoff@y.values)

colnames(df.ppv.cutoff) <- c( "cutoff" , "ppv")



performance.acc.cutoff <- performance(predicciones , measure = "acc" , x.measure = "cutoff")

df.acc.cutoff <- data.frame(performance.acc.cutoff@x.values , performance.acc.cutoff@y.values)

colnames(df.acc.cutoff) <- c( "cutoff" , "acc")



performance.sens.cutoff <- performance(predicciones , measure = "sens" , x.measure = "cutoff")

df.sens.cutoff <- data.frame(performance.sens.cutoff@x.values , performance.sens.cutoff@y.values)

colnames(df.sens.cutoff) <- c( "cutoff" , "sens")



performance.spec.cutoff <- performance(predicciones , measure = "spec" , x.measure = "cutoff")

df.spec.cutoff <- data.frame(performance.spec.cutoff@x.values , performance.spec.cutoff@y.values)

colnames(df.spec.cutoff) <- c( "cutoff" , "spec")



performance.npv.cutoff <- performance(predicciones , measure = "npv" , x.measure = "cutoff")

df.npv.cutoff <- data.frame(performance.npv.cutoff@x.values , performance.npv.cutoff@y.values)

colnames(df.npv.cutoff) <- c( "cutoff" , "npv")



grafico <- ggplot () + 
        
        geom_line(data = df.mat.cutoff , aes(x = cutoff, y = mat , color = "mat" ) , size = 1) + 
        
        geom_line(data = df.ppv.cutoff , aes(x = cutoff, y = ppv , color = "ppv" ) , size = 1) + 
        
        geom_line(data = df.acc.cutoff , aes(x = cutoff, y = acc , color = "acc" ) , size = 1) +
        
        geom_line(data = df.sens.cutoff , aes(x = cutoff, y = sens , color = "sens" ) , size = 1) +
        
        geom_line(data = df.spec.cutoff , aes(x = cutoff, y = spec , color = "spec" ) , size = 1) +
        
        geom_line(data = df.npv.cutoff , aes(x = cutoff, y = npv , color = "npv" ) , size = 1) +
        
        scale_colour_manual(values = c("red", "blue", "green" , "violet" , "yellow" , "orange")) +
        
        ggtitle("Test set") +
        
        labs(colour = "Variable", y = NULL) 


ggplotly(grafico)







##############################################################

##############################################################

## Para guardar el modelo y despues volver a cargarlo 
## cuando lo necesito asi no lo calculo otra vez.

#############################################################

#############################################################





saveRDS(rf, "rf.rds") ## guardo el modelo 


rf <- readRDS("rf.rds") ## vuelvo a cargar el modelo












################### GRAFICO DE SUPERFICIE 3D - PPV  #######################


dude <- "Dudepoliaminas2016.csv"

library(data.table)

dude <- as.data.frame(fread(input = dude, check.names = TRUE)) #leo el archivo con mis descriptores del dude set

dude$clase <- as.factor(dude$clase) # hago que la clase sea factor 

names(dude) <- make.names(names(dude))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

predicciones.dude <- predict(object = rf, newdata = dude, type="prob")  ## predicciones en el dude set expresadas como probabilidad

######################
##### lo siguiente lo hago porque se me reduce el numero de predicciones. en vez de ponerme NA en el tet set predicho, me disminuye el numero de valores predichos
##### por lo tanto lo que hago es agregar NA a las filas que son eliminadas para poder luego seguir trabajando

un1 <- union(rownames(dude), rownames(predicciones.dude)) ## creo un vector que me dice que filas estan en ambos dataframes

d1  <- as.data.frame(matrix(NA, ncol = ncol(predicciones.dude), nrow = length(un1), dimnames = list(un1, names(predicciones.dude)))) # creo un dataframe de solos NA con los nombres y las dimensiones como el dataframe predicciones.dude

d1[rownames(d1) %in% rownames(predicciones.dude),] <- predicciones.dude ## reemplazo los NA por los valores que si tengo en predicciones.dude

predicciones.dude <- d1 ## cambio el nombre para poder seguir trabajando


library(pROC)

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude[,2], direction = "<" , plot = TRUE) ## calculo de la curva ROC para los resultados de la base dude

auc(curva.ROC.dude) ### imprimo el AUC de la curva ROC para ver si coincide con los resultados anteriores

tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de Sensitivity y Specificity, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold Sensitivity y specificity

tabla.puntos.curva.roc$se.sp <- tabla.puntos.curva.roc$sensitivity/tabla.puntos.curva.roc$specificity ## creo una columna donde estan los valores de sensiblidad/especificidad, o sea la division de estos valores

tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc[ tabla.puntos.curva.roc$se.sp <= 2, ]  ### elimino los valores de sensiblidad/especificidad mayores a 2 porque no me dan informacion para el grafico

tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc.limpia[ !tabla.puntos.curva.roc.limpia$se.sp == 0, ] ## elimino los valores de sensibilidad/espeficidad que son iguales a cero porque tampoco me sirven para graficar

library(openxlsx) ## abro el paquete openxlsx

write.xlsx(x= tabla.puntos.curva.roc.limpia, file= "tabla.puntos.curva.roc.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

Sensitivity <- tabla.puntos.curva.roc.limpia$sensitivity ##extraigo los valores de Sensitivity

Specificity <- tabla.puntos.curva.roc.limpia$specificity##extraigo los valores de Specificity

Prevalence <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de Prevalences donde voy a calcular el PPV

list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados

for (i in 1:length(Prevalence)){ ##loop donde para cada Prevalence hago un barrido para los diferentes valores de Sensitivity/Specificity, asi calculo el PPV
        
        list.PPV[[i]] <- (Sensitivity*Prevalence[i])/(Sensitivity*Prevalence[i] + (1- Specificity)*(1- Prevalence[i]))##calculo del PPV
        
}

PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes Prevalences y las filas son las diferentes relaciones Sensitivity/Specificity, y los valores de cada celda es la PPV correspondiente para esos valores

Prevalence <- as.list(Prevalence)

library(plotly)

f1 <- list( size = 18) ## esto es si quiero cambiar algo de la fuente del titulo de los ejes

f2 <- list( size = 14) ## esto es si quiero cambiar algo de la fuente de las marcas de los ejes

axis.x <- list(title="Ya", ## opciones para el eje x
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T, ## si se muestra la cuadricula
               gridwidth = 10, ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje

axis.y <- list(title="Sensitivity/Specificity", ## opciones para el eje y
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 10,  ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje

axis.z <- list(title="PPV",  ## opciones para el eje z
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 10,  ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje

scene <- list(              ## resumo las info de los ejes en esta variable llamada "scene"
        xaxis = axis.x,
        yaxis = axis.y,
        zaxis = axis.z)

####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

p<-plot_ly(x= ~Prevalence, y = ~Sensitivity/Specificity, z = ~PPV, type = "surface") %>% layout( title ="3D Surface PPV" , scene = scene)  # hago el grafico de superficie 3D, aca especifico el nombre del grafico y luego en scene pongo la variable scene que tiene los formatos deseados

p

htmlwidgets::saveWidget(as.widget(p), "PPV - Random Forest.html") ### GUARDO EL GRÁFICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB









###############  PREDICCIONES EN BASES DE DATOS  #################




setwd("D:/Dropbox/R/descriptores drugbank") ### seteo la carpeta de drugbank. Si lo hago con sweatlead tengo que setear otra carpeta

## la carpeta de arriba es la de la facu

setwd("C:/Users/Francisco/Dropbox/R/descriptores drugbank") ## esta carpeta es la de la notebook





base.datos <- "base drugbank 24-10-16.csv" ### nombre del archivo con la base de datos

df.base.datos <- as.data.frame(fread(input=base.datos, check.names = TRUE)) #leo el archivo con la base de datos

names(df.base.datos) <- make.names(names(df.base.datos))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(df.base.datos))) > 0) { ## hago un if para eliminar las columnas que tienen nombre repetido
        
        df.base.datos <- df.base.datos[, -which(names(df.base.datos) == names(df.base.datos)[duplicated(names(df.base.datos))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}

predicciones.base.datos <- as.data.frame(predict(object = rf, newdata = df.base.datos, type="prob"))  ## predicciones en la base de datos expresadas como probabilidad

colnames(predicciones.base.datos) <- c("Prob. Inactivo" , "Prob. Activo")

######################
##### lo siguiente lo hago porque se me reduce el numero de predicciones. en vez de ponerme NA en el tet set predicho, me disminuye el numero de valores predichos
##### por lo tanto lo que hago es agregar NA a las filas que son eliminadas para poder luego seguir trabajando

un1 <- union(rownames(df.base.datos), rownames(predicciones.base.datos)) ## creo un vector que me dice que filas estan en ambos dataframes

d1  <- as.data.frame(matrix(NA, ncol = ncol(predicciones.base.datos), nrow = length(un1), dimnames = list(un1, names(predicciones.base.datos)))) # creo un dataframe de solos NA con los nombres y las dimensiones como el dataframe predicciones.base.datos

d1[rownames(d1) %in% rownames(predicciones.base.datos),] <- predicciones.base.datos ## reemplazo los NA por los valores que si tengo en predicciones.base.datos

predicciones.base.datos <- d1 ## cambio el nombre para poder seguir trabajando


predicciones.base.datos$NOMBRE <- df.base.datos$NAME

library(openxlsx)

write.xlsx(x= predicciones.base.datos, file= "Screening por Random Forest.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos





######################## SUMO DOMINIO DE APLICABILIDAD A PREDICCIONES DE BASE DE DATOS ####################





base.datos.solo.descr.train <- df.base.datos[,colnames(training)[-1]] ## con esto hago que mi base de datos tenga las mismas columnas que mi training set. Pongo -1 para eliminar la columna clase del training. Hago esto para en el paso siguiente poder unir ambas tablas porque si tienen diferentes num de columnas no las puedo unir

base.datos.training <- rbind(base.datos.solo.descr.train, training[,-1]) ### junto el training y la base de datos en un solo data frame. las filas de arriba son las de la base de datos. Lo hago para poder luego extraer la info de proximidad de la base de datos contra el training

base.datos.training <- na.roughfix(base.datos.training) ## con esto hago que se imputen los NA por valores que son estimados por la media/moda. esto lo hago para poder calcular el dominio de aplicabilidad. Si no hago esto hay menos filas y columnas y no se que fila es cada compuesto. Los valores que son NA como predije su actividad antes, si es NA, lo va a seguir siendo

predicciones.base.datos.training <- predict(object = rf, newdata = base.datos.training, type="prob", proximity = TRUE)  ## predigo para luego ver la proximidad de la base de datos con respecto al training 

proximidad.base.datos <- as.data.frame(predicciones.base.datos.training$proximity[1:nrow(df.base.datos),(nrow(df.base.datos)+1):ncol(predicciones.base.datos.training$proximity)]) ## aca extraigo las filas y columnas que solo necesito. Son las que comparan la base de datos con respecto al training

nombres.col <- sprintf("training %d",seq(1:nrow(training))) ## creo el vector con los nombres para las columnas (training set)

nombres.filas <- df.base.datos$NAME ## creo el vector para el nombre de las filas (base de datos)

colnames(proximidad.base.datos) <- nombres.col ## nombro las columnas

row.names(proximidad.base.datos) <- nombres.filas ## nombro las filas

punto.corte.1 <- c(0.5,0.6,0.7,0.8,0.9) ## creo la secuencia de puntos de corte de proximidad que voy a utilizar

matrix.suma <- matrix(nrow = nrow(proximidad.base.datos) , ncol = length(punto.corte.1) ) ## creo una matrix vacia donde voy a guardar los resultados de aplicar los distintos puntos de corte de proximidad

for ( i in 1:length(punto.corte.1)) { ## hago un loop a traves de la tabla de proximidad, para saber cuantos compuestos cumplen con el corte de proximidad
        
        resultados.punto.corte.1 <- proximidad.base.datos > punto.corte.1[i] ## aplico el punto de corte de proximidad a mi tabla de proximidad de la base de datos contra el training
        
        matrix.suma[,i] <- rowSums(x = resultados.punto.corte.1 , na.rm = FALSE) ## realizo la suma de la cantidad de compuestos que cumplen con el punto de corte y lo guardo en la matrix vacia que habia armado
        
}

colnames(matrix.suma) <- sprintf("DA - punto corte proximidad %f",punto.corte.1) ## creo el vector con los nombres para las columnas (training set)

predicciones.base.datos.DA <- cbind(predicciones.base.datos, matrix.suma) ## esta es la tabla que contiene las predicciones y los resultados del dominio de aplicabilidad hecho por proximidad

library(openxlsx) ## abro el paquete openxlsx para poder guardar el data frame con los resultados en un excel

write.xlsx(x= predicciones.base.datos.DA, file= "Screening por Random Forest con Dominio Aplicabilidad.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos





