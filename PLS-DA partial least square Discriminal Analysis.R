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






## armo el modelo de plsda que luego voy a optimizar

PLS.DA <- plsda(X=training.matrix , ## le digo cuales son las variables, tengo que darselas como matrix
                
                Y = clase , ## aca tengo que poner un vector que indique la clase de cada fila
                
                scale = TRUE, ## con esto le pido que me escale las variables
                
                ncomp = num.comp , ## con esto le digo hasta que numero de componentes desarrolle el modelo
                
                near.zero.var = FALSE) ## le digo que no me elimine las variables con varianza cercanas a cero porque ya lo hice yo anteriormente





## Voy a medir la performance de mi modelo por cross-validation

PLS.DA.performance <- perf( object = PLS.DA ,  ## modelo al cual le voy a estimar la performance
                            
                            validation = "Mfold" , ## le digo que voy a hacer la validacion por cross-validation
                            
                            folds = 10 ,  ## le digo la cantidad de veces que voy a particionar al training set. En este caso seleccione  k fold = 10 porque en el curso vimos que es el valor optimo
                            
                            progressBar = TRUE , ### le digo que si a la progress bar asi veo por donde anda el proceso
                            
                            nrepeat = 5 ,  ## numero de repeticiones para hacer el cross-validation
                            
                            auc = TRUE ) ### le pido que me calcule el AUC ROC de la performance del modelo



PLS.DA.performance ## imprimo el objeto con los resultados

plot(PLS.DA.performance  , sd= FALSE) ## hago el grafico general para entender la performance del cross-validation

plot(PLS.DA.performance , dist = "max.dist") ## me grafica la performance en base a el error calculado como la maxima distancia, que se obtuvo por el crossvalidation en el paso anterior

plot(PLS.DA.performance , dist = "centroids.dist") ## me grafica la performance en base a el error calculado como la distancia de los centroides, que se obtuvo por el crossvalidation en el paso anterior

plot(PLS.DA.performance , dist = "mahalanobis.dist")# me grafica la performance en base a el error calculado como mahalanobis, que se obtuvo por el crossvalidation en el paso anterior




#### **************************************************************####


### CON LOS GRAFICOS ANTERIORES DECIDO CUAL ES EL MEJOR NUMERO DE COMPONENTES 

## Y ESE LO COLOCO EN LA SIGUIENTE VARIABLE






num.comp.optimo <- 4  ## aqui coloco el numero de componentes optimo


## hago PLS-DA, pero ahora con el numero optimo de componentes obtenidos en el paso anterior

PLS.DA.optimo <- plsda(X=training.matrix , ## ## le digo cuales son las variables, tengo que darselas como matrix
                       
                       Y = clase , ## aca tengo que poner un vector que indique la clase de cada fila
                       
                       scale = TRUE, ## con esto le pido que me escale las variables
                       
                       ncomp = num.comp.optimo )  ## con esto le digo el numero optimo de componentes a utilizar para desarrollar el modelo

plotVar(PLS.DA.optimo) ## me grafica la importancia de las variables, o algo asi

plotIndiv(PLS.DA.optimo) ## me grafica las muestras en el espacio de los 2 componentes que mejor variabilidad tienen

plotIndiv(object = PLS.DA.optimo ,  ind.names = FALSE ,pch = 21,  ellipse = TRUE , ellipse.level = 0.95 , centroid = TRUE , star = TRUE , style = "ggplot2" , title = "Gráfico de Individuos en 2D - PLSDA" , col.per.group =c("blue" , "green") ) ## me grafica las muestras en el espacio de los 2 componentes que mejor variabilidad tienen pero con elipses y unidas al centroide

plotIndiv(object = PLS.DA.optimo ,  ind.names = FALSE ,pch = "tetra" ,  ellipse = TRUE , ellipse.level = 0.95 , centroid = TRUE , star = TRUE , style = "3d" , col.per.group =c("blue" , "green")) ## me grafica las muestras en el espacio de los 3 componentes que mejor variabilidad tienen

predicciones.train <-  predict(object = PLS.DA.optimo , newdata = training.matrix , dist = "max.dist") ### predicciones en el training. Uso max.dist porque supuestamente para este caso de clasificacion es la mejor medida

predicciones.train2 <- predicciones.train$predict[,2,num.comp.optimo] ## con esto lo que hago es extraer los valores predichos. El valor de 2 es para extraer la columna de predicciones para ser de clase 1 y el num.comp.opt para decir cual de todas las matrices quiero del array

auc.training <- auc(roc(predictor = predicciones.train2,response = clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <-  predict( object = PLS.DA.optimo , newdata = test.matrix.limpia)  ### predicciones en el test, como type es response el resultado me da como probabilidad

predicciones.test2 <- predicciones.test$predict[,2,num.comp.optimo] ## con esto lo que hago es extraer los valores predichos. El valor de 2 es para extraer la columna de predicciones para ser de clase 1 y el num.comp.opt para decir cual de todas las matrices quiero del array

auc.test <- auc(roc(predictor= predicciones.test2, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado.plsda <- list("Modelo armado por PLS-DA", PLS.DA.optimo , "Nº de Componentes Óptimo", num.comp.optimo , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.plsda






######### GRAFICOS PARA ANALIZAR LOS RESULTADOS ############










test <- "Ddudesmiristoiltodos.csv"  ### nombre del test set

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test.matrix <- as.matrix(test) ## lo transformo en matrix

test.matrix.limpia <- test.matrix[,colnames(test.matrix)[colnames(test.matrix) %in% colnames(training.matrix)]] ### hago que solo queden las columnas que estaban en el training, porque sino tira error cuando quiero predecir

class(test.matrix.limpia) <- "numeric" ### no se porque en el paso anterior se me transforman los datos a caracter, entonces los vuelvo numeric otra vez en este paso

predicciones.test <-  predict( object = PLS.DA.optimo , newdata = test.matrix.limpia)  ### predicciones en el test, como type es response el resultado me da como probabilidad

predicciones.test2 <- predicciones.test$predict[,2,num.comp.optimo] ## con esto lo que hago es extraer los valores predichos. El valor de 2 es para extraer la columna de predicciones para ser de clase 1 y el num.comp.opt para decir cual de todas las matrices quiero del array

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test2, labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte






################### GRAFICO DE SUPERFICIE 3D - PPV  #######################






dude <- "Ddudesmiristoiltodos.csv"  ### nombre del test set

dude <- as.data.frame(fread(input = dude, check.names = TRUE)) #leo el archivo con mis descriptores del test set

dude.matrix <- as.matrix(test) ## lo transformo en matrix

dude.matrix.limpia <- dude.matrix[,colnames(dude.matrix)[colnames(dude.matrix) %in% colnames(training.matrix)]] ### hago que solo queden las columnas que estaban en el training, porque sino tira error cuando quiero predecir

class(dude.matrix.limpia) <- "numeric" ### no se porque en el paso anterior se me transforman los datos a caracter, entonces los vuelvo numeric otra vez en este paso

predicciones.dude <-  predict( object = PLS.DA.optimo , newdata = dude.matrix.limpia)  ### predicciones en el test, como type es response el resultado me da como probabilidad

predicciones.dude2 <- predicciones.dude$predict[,2,num.comp.optimo] ## con esto lo que hago es extraer los valores predichos. El valor de 2 es para extraer la columna de predicciones para ser de clase 1 y el num.comp.opt para decir cual de todas las matrices quiero del array

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude2, direction = "<" , plot = TRUE) ## calculo de la curva ROC para los resultados de la base dude

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

df.base.datos <- as.data.frame(fread(input = base.datos, check.names = TRUE)) #leo el archivo con mis descriptores del test set

base.datos.matrix <- as.matrix(df.base.datos) ## lo transformo en matrix

base.datos.matrix.limpia <- base.datos.matrix[,colnames(base.datos.matrix)[colnames(base.datos.matrix) %in% colnames(training.matrix)]] ### hago que solo queden las columnas que estaban en el training, porque sino tira error cuando quiero predecir

class(base.datos.matrix.limpia) <- "numeric" ### no se porque en el paso anterior se me transforman los datos a caracter, entonces los vuelvo numeric otra vez en este paso

predicciones.base.datos <-  predict( object = PLS.DA.optimo , newdata = base.datos.matrix.limpia)  ### predicciones en el test, como type es response el resultado me da como probabilidad

predicciones.base.datos2 <- as.data.frame(predicciones.base.datos$predict[,2,num.comp.optimo]) ## con esto lo que hago es extraer los valores predichos. El valor de 2 es para extraer la columna de predicciones para ser de clase 1 y el num.comp.opt para decir cual de todas las matrices quiero del array

colnames(predicciones.base.datos2) <- "SCORE max.dist"

predicciones.base.datos2$NOMBRE <- df.base.datos$NAME ## le agrego la columna nombres

library(openxlsx)

write.xlsx(x= predicciones.base.datos2, file= "Screening por PLSDA paquete mixOmics.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos




