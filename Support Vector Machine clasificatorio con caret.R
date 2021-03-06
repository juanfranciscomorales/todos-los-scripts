###########################################


## # Support Vector Classifier ##


###########################################



is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("parallel") == FALSE) {install.packages("parallel")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("doParallel") == FALSE) {install.packages("doParallel")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(parallel) ##  paquete para hacer paralelizacion 

library(doParallel) ##  paquete para hacer paralelizacion 

library(caret)

library(data.table)

library(pROC)



training.set  <- "Trainingpoliaminas2016.csv"  ### nombre del archivo con el training set

test.set <- "Testpoliaminas2016.csv"  ### nombre del archivo con el test set

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

training$clase <- as.factor(make.names(training$clase)) ## hago que la columna clase sea como factor y con nombres validos para poder hacer que el SVM sea clasificatorio

clase <- training$clase ## guardo los valores de clase para despues

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test[is.na(test)] <- 0 ### con esto lo que hago es reemplazar los NA por ceros para poder hacer las predicciones, porque sino me tira error





#########################################

## CONFIGURACION PARA HACER PARALELIZACION ##

#########################################


set.seed(1)

## seteo para poder hacer calculos en paralelo

cores <- detectCores() ## con esta funcion obtengo el numero de nucleos de la compu

cls = makeCluster(cores) # Creates a set of copies of R running in parallel and communicating over sockets.

registerDoParallel(cls) ## The registerDoParallel function is used to register the parallel backend with the foreach package.


## genero una lista de seeds para poder hacer reproducible el ejemplo
## lista tiene 1000 elementos con un vector de 1000 dentro de cada elemento
## lo hago enorme por las dudas, ademas si sobran seeds no hay drama


seeds <- vector(mode = "list", length = 1000) ## creo una lista de largo 1000

for(i in 1:length(seeds)) seeds[[i]] <- sample.int(1000, 1000) ## hago que cada elemento de la lista este compuesto por un vector con 1000 enteros.


###############################################################################



#       BUSQUEDA DE PARAMETROS OPTIMOS 



##############################################################################





## seteo para la cross validation

ctrl <- trainControl(method="adaptive_cv",# aca armo el elemento para optimizar el valor de K. El metodo es cross-validation
                     
                     allowParallel = TRUE, # con esto le digo que si puede hacer calculos en paralelo lo haga
                     
                     seeds = seeds, # con esto seteo las seeds para que cuando se use paralelizacion sea reproducible
                     
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

svmGrid <-  expand.grid(  ## con esto lo que voy a hacer es decir el barrido que va a hacer la funcion para optimizar los siguientes parámetros de gbm
        
        sigma = 2^(-15:3), ## este parametro es para ver la profundidad del arbol optima
        
        C = 2^(-5:15)) ## este parametro es para ver el numero optimo de arboles
        


### Entreno el modelo por svmRadial y optimizo los valores

ptm <- proc.time() 
 
svmfit <- train(clase ~ .,## uso la funcion train del paquete caret para hacer knn. en esta linea especifico cual es el valor a predecir y cuales son las variables independientes. 
                
                data = training, ## le digo cuales son mis datos para armar el modelo
                
                method = "svmRadial", ## aca le digo que use Support vector machine para armar el modelo
                
                trControl = ctrl,  ## le digo que use el elemento ctrl para optimizar el modelo
                
                tuneGrid = svmGrid , ## hago pasar el elemento svmGrid para probar y encontrar cuales son los valores optimos 
                
                preProcess = c("center","scale") , ## aca le digo que me centre y me escale los datos antes de armar el modelo
                
                prob.model = TRUE) ## le digo con esto que tambien calcule la probabilidad de ser de una clase u otra

proc.time() - ptm

svmfit ## imprimo el resultado

plot(svmfit) ## grafico los resultados del cross validation




predicciones.train <- predict(object = svmfit, newdata = training, type ="prob") ## predicciones en el training set expresadas como probabilidad

names(predicciones.train) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.training <- auc(roc(predictor = predicciones.train$Activo,response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = svmfit, newdata = test, type = "prob") ### predigo en el test set

names(predicciones.test) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.test <- auc(roc(predictor= predicciones.test$Activo, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 



#  ME TIRA ERROR, PORQUE HAY NA EN EL TEST SET

# SI PONGO na.omit  ME PREDICE PERO MENOR CANT DE FILAS, ENTONCES CUANDO QUIERO HACER LA CURVA ROC DEL TEST SET ME TIRA ERROR




resultado.svm <- list("Modelo armado por Support Vector Machine con caret", svmfit , "AUC ROC Training" , auc.training,"Resultado K-fold CV" , getTrainPerf(svmfit) ,  "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.svm



stopCluster(cls) ## cierro el cluster con el que hice la paralelizacion asi no tengo problemas de configuracion




######### GRAFICOS PARA ANALIZAR LOS RESULTADOS - TRAINING SET ############




predicciones.train <- predict(svmfit, newdata = training, type="prob" , na.action = na.pass)  ## predicciones en el train set expresadas como probabilidad

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






test <- "Testpoliaminas2016.csv"  ### nombre del test set

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test[is.na(test)] <- 0 ### con esto lo que hago es reemplazar los NA por ceros para poder hacer las predicciones, porque sino me tira error

predicciones.test <- predict(svmfit, newdata = test, type="prob" , na.action = na.pass)  ## predicciones en el test set expresadas como probabilidad

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





saveRDS(svmfit, "svmfit.rds") ## guardo el modelo 


svmfit <- readRDS("svmfit.rds") ## vuelvo a cargar el modelo










################### GRAFICO DE SUPERFICIE 3D - PPV  #######################






dude <- "test set curado 2.csv"

dude <- as.data.frame(fread(input = dude, check.names = TRUE)) #leo el archivo con mis descriptores del dude set

dude[is.na(dude)] <- 0 ### con esto lo que hago es reemplazar los NA por ceros para poder hacer las predicciones, porque sino me tira error

predicciones.dude <- predict(object = svmfit, newdata = dude, type = "prob") ### predigo en el test set

names(predicciones.dude) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude$Activo, direction = "<" , plot = TRUE) ## calculo de la curva ROC para los resultados de la base dude

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

htmlwidgets::saveWidget(as.widget(p), "PPV.html") ### GUARDO EL GR?FICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB









###############  PREDICCIONES EN BASES DE DATOS  #################







setwd("D:/Dropbox/R/descriptores drugbank") ### seteo la carpeta de drugbank. Si lo hago con sweatlead tengo que setear otra carpeta

base.datos <- "base drugbank 24-10-16.csv" ### nombre del archivo con la base de datos

df.base.datos <- as.data.frame(fread(input=base.datos, check.names = TRUE)) #leo el archivo con la base de datos

df.base.datos[is.na(df.base.datos)] <- 0 ### con esto lo que hago es reemplazar los NA por ceros para poder hacer las predicciones, porque sino me tira error

predicciones.base.datos <- predict(object = svmfit, newdata = df.base.datos, type = "prob" ) ### predigo en el test set

colnames(predicciones.base.datos) <- c("Prob. Activo" , "Prob. Inactivo")

predicciones.base.datos$NOMBRE <- df.base.datos$NAME

library(openxlsx)

write.xlsx(x= predicciones.base.datos, file= "Screening por Support Vector Machine con caret.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos



