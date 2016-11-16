

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

if (sum(duplicated(names(training))) > 0 ) {  ## hago un if para que me elimine en caso de que haya algun nombre de columna repetida

training <- training[, -which(names(training) == names(training)[duplicated(names(training))])]  ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

}

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

names(test) <- make.names(names(test))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(test))) > 0 ) { ## hago un if para eliminar las columnas que tienen nombre repetido

test <- test[, -which(names(test) == names(test)[duplicated(names(test))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados

}

rf <- randomForest(clase ~., data=training ,importance=TRUE, do.trace = TRUE, ntree = cant.arboles)## lo que hacemos aca es random forest

plot(rf, main = "Gráfico OOB error Random Forest") ## hago que grafique el error versus el numero de arboles en el training set

legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4) ## con esto hago que aparezcan las referencias en el grafico anterior

varImpPlot(rf, n.var=10, main= "Importancia de variables en RF") ## grafico la importancia de las variables segun Random Forest

predicciones.train <- predict(object = rf, newdata = training, type="prob") ## predicciones en el training set expresadas como probabilidad

auc.training <- auc(roc(predictor = predicciones.train[,2],response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = rf, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad

auc.test <- auc(roc(predictor= predicciones.test[,2], response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por Random Forest", rf , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado

}





####### HASTA ACA ES LA FUNCION, LUEGO HAGO CORRER EL COMANDO DE ABAJO #####





resultado.rf <- random.forest(training ="Dtrainingmiristoil.csv", test="Ddudes2miristoil.csv",cant.arboles = 1000 )  ### con esto hago correr para obtener resultados

resultado.rf ## imprimo los resultados obtenidos. A su vez obtengo 4 graficos que se imprimen automaticamente






######### GRAFICOS PARA ANALIZAR LOS RESULTADOS ############




rf <- resultado.rf[[2]] ### es la funcion obtenida de Random Forest

test <- "Descriptorres DUDE2 Sofi.csv"  ### nombre del test set

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test$clase <- as.factor(test$clase) # hago que la clase sea factor 

names(test) <- make.names(names(test))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(test)))) { ## hago un if para eliminar las columnas que tienen nombre repetido
        
        test <- test[, -which(names(test) == names(test)[duplicated(names(test))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}

predicciones.test <- predict(object = rf, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test[,2], labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte






################### GRAFICO DE SUPERFICIE 3D - PPV  #######################






dude <- "Descriptorres DUDE2 Sofi.csv"

dude <- as.data.frame(fread(input = dude, check.names = TRUE)) #leo el archivo con mis descriptores del dude set

dude$clase <- as.factor(dude$clase) # hago que la clase sea factor 

names(dude) <- make.names(names(dude))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(dude)))) { ## hago un if para eliminar las columnas que tienen nombre repetido
        
        dude <- dude[, -which(names(dude) == names(dude)[duplicated(names(dude))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}

predicciones.dude <- predict(object = rf, newdata = dude, type="prob")  ## predicciones en el dude set expresadas como probabilidad

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude[,2], direction = "<") ## calculo de la curva ROC para los resultados de la base dude

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
               titlefont = f1,
               tickfont = f2,
               showgrid = T)

axis.y <- list(title="Sensitivity/Specificity", ## opciones para el eje y
               titlefont = f1,
               tickfont = f2,
               showgrid = T)

axis.z <- list(title="PPV",  ## opciones para el eje z
               titlefont = f1,
               tickfont = f2,
               showgrid = T)

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

names(df.base.datos) <- make.names(names(df.base.datos))# con esto hago que los nombres sean "legales", porque sino el random forest no corre

if (sum(duplicated(names(df.base.datos))) > 0) { ## hago un if para eliminar las columnas que tienen nombre repetido
        
        df.base.datos <- df.base.datos[, -which(names(df.base.datos) == names(df.base.datos)[duplicated(names(df.base.datos))])] ## elimino las columnas que tienen nombre igual ya que en el paso anterior, distintos simbolos se vuelven puntos y hacen que haya nombres de columnas duplicados
        
}

predicciones.base.datos <- as.data.frame(predict(object = rf, newdata = df.base.datos, type="prob"))  ## predicciones en la base de datos expresadas como probabilidad

colnames(predicciones.base.datos) <- c("Prob. Inactivo" , "Prob. Activo")

predicciones.base.datos$NOMBRE <- df.base.datos$NAME

library(openxlsx)

write.xlsx(x= predicciones.base.datos, file= "Screening por Random Forest.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos

