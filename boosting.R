###########################################


## # BOOSTING - PARA 2 CLASES ##


###########################################




is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("gbm") == FALSE) {install.packages("gbm")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table)

library(gbm) # cargo el paquete gbm que tiene la funcion para hacer boosting con trees

library(pROC) ## cargo el paquete pROC

library(ggplot2)

training.set <- "Dtrainingmiristoil.csv"

test.set <-  "Dtestmiristoil.csv"

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

set.seed(2) ## seteo la semilla

boosting.cv <- gbm(formula = clase~ ., distribution = "bernoulli" , data = training,  n.trees = 5000 , cv.folds = 10 , shrinkage = 0.01 , interaction.depth = 1 , class.stratify.cv =TRUE , verbose = TRUE) ### aca hago el boosting en si. A su vez hace cross-validation con un k = 10. Esta cross-validation despues es la usada por la funcion gbm.perf para encontrar el numero optimo de arboles

variables.importantes <- head(summary(boosting.cv), n = 20) ## extraigo un data frame que contiene los valores de influencia y el nombre de las variables. con n decido cuantas variables mas importantes extraigo

variables.importantes <- transform(variables.importantes, var = reorder(var, rel.inf)) ## este paso lo hago asi en el siguiente grafico me pone ordenadas por la influencia relativa y no por orden alfabetico

ggplot(data = variables.importantes , aes(x = var, y = rel.inf)) + geom_bar(stat="identity", fill="steelblue")  + coord_flip() + theme_minimal() + labs(title = "20 most influence variables in Boosting", y = "Relative Influence" , x = "Variable") + theme(plot.title = element_text(hjust = 0.5))  ## con esto grafico las 20 variables que mas influyen en el armado del modelo

num.arboles.optimo.cv <- gbm.perf(object = boosting.cv , plot.it = TRUE , oobag.curve = FALSE , method = "cv") ## me dice cual es el numero de arboles optimos segun el cross-validation que plantie hacer en la funcion gbm

predicciones.train <-  predict.gbm(object = boosting.cv , newdata = training , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action = NULL ) ### predicciones en el training, como type es response el resultado me da como probabilidad

auc.training <- auc(roc(predictor = predicciones.train,response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <-  predict.gbm(object = boosting.cv , newdata = test , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action =NULL) ### predicciones en el test, como type es response el resultado me da como probabilidad

auc.test <- auc(roc(predictor= predicciones.test, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por Boosting", boosting.cv , "Nº de arboles Òptimo",num.arboles.optimo.cv , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado






######### GRAFICOS PARA ANALIZAR LOS RESULTADOS ############






test <- "Ddudes2miristoil.csv"  ### nombre del test set

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

predicciones.test <- predict.gbm(object = boosting.cv , newdata = test , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action =NULL) ### predicciones en el test, como type es response el resultado me da como probabilidad  ## predicciones en el test set expresadas como probabilidad

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test, labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte









################### GRAFICO DE SUPERFICIE 3D - PPV  #######################







dude.set <- "Ddudes2miristoil.csv"

dude <- as.data.frame(fread(input = dude.set, check.names = TRUE)) #leo el archivo con mis descriptores del dude set

predicciones.dude <- predict.gbm(object = boosting.cv , newdata = dude , n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action =NULL) ### predicciones en el test, como type es response el resultado me da como probabilidad  ## predicciones en el test set expresadas como probabilidad

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude, direction = "<", plot = TRUE) ## calculo de la curva ROC para los resultados de la base dude

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

axis.x <- list(title="Prevalence", ## opciones para el eje x
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

htmlwidgets::saveWidget(as.widget(p), "PPV.html") ### GUARDO EL GRÁFICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB









###############  PREDICCIONES EN BASES DE DATOS  #################







setwd("D:/Dropbox/R/descriptores drugbank") ### seteo la carpeta de drugbank. Si lo hago con sweatlead tengo que setear otra carpeta

base.datos <- "base drugbank 24-10-16.csv" ### nombre del archivo con la base de datos

df.base.datos <- as.data.frame(fread(input=base.datos, check.names = TRUE)) #leo el archivo con la base de datos

predicciones.base.datos <- as.data.frame(predict.gbm(object = boosting.cv , newdata = df.base.datos, n.trees = num.arboles.optimo.cv , type = "response" , single.tree = FALSE , na.action =NULL)) ### predicciones en el test, como type es response el resultado me da como probabilidad  ## predicciones en el test set expresadas como probabilidad  ## predicciones en la base de datos expresadas como probabilidad

colnames(predicciones.base.datos) <- c("Prob. Activo")

predicciones.base.datos$NOMBRE <- df.base.datos$NAME

library(openxlsx)

write.xlsx(x= predicciones.base.datos, file= "Screening por Boosting.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos





