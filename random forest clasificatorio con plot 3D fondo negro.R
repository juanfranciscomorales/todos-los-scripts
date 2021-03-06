###########################################


## RANDOM FOREST CLASIFICATORIO  ##


###########################################






training.set  <- "Dtrainingmiristoil.csv"  ### nombre del archivo con el training set

test.set <- "Dtestmiristoil.csv"  ### nombre del archivo con el test set

cant.arboles <- 3000  ### cant de arboles que pongo en el RF

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no
        
if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("randomForest") == FALSE) {install.packages("randomForest")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table) # cargo data.table

library(randomForest) # cargo el paquete random forest

library(pROC) ## cargo el paquete pROC

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

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

set.seed(125) ## seteo la semilla para que sea reproducible el random forest




## lo que hacemos aca es random forest
rf <- randomForest(clase ~.,  ## aca pongo la formula para decir cual es mi variable y y cual es mi x. en este caso la y = clase y x = al resto de las columnas
                   
                   data=training , ## explicito de donde saco los datos
                   
                   importance=TRUE, ## le digo que me calcule la importancia de las variables
                   
                   do.trace = TRUE, ### con esto le digo que me vaya diciendo como va la corrida
                   
                   ntree = cant.arboles , ## le digo el numero de arboles a armar
                   
                   proximity = TRUE) ## con esto le digo que me calcule la proximidad. La proximidad es la cantidad de veces que caen en la misma hoja 2 compuestos



plot(rf, main = "Gr�fico OOB error Random Forest") ## hago que grafique el error versus el numero de arboles en el training set

legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4) ## con esto hago que aparezcan las referencias en el grafico anterior

varImpPlot(rf, n.var=10, main= "Importancia de variables en RF") ## grafico la importancia de las variables segun Random Forest

MDSplot(rf = rf,fac = training$clase , k =3) # Plot the scaling coordinates of the proximity matrix from randomForest. Hace escalado multidimensional de la matrix de proximidad. es una forma de ver si me separa las clases


############################

# MIRO LOS GRAFICOS Y VEO CUAL ES EL NUMERO DE ARBOLES OPTIMO

############################


cant.arboles.optimo <- 500  #### ESTO LO SACO DE LOS GRAFICOS QUE HICE ANTES DE OOB



## vuelvo a correr el random forest pero esta vez con el numero optimo de arboles

rf <- randomForest(clase ~., ## aca pongo la formula para decir cual es mi variable y y cual es mi x. en este caso la y = clase y x = al resto de las columnas
                   
                   data=training , ## explicito de donde saco los datos
                   
                   importance=TRUE, ## le digo que me calcule la importancia de las variables
                   
                   do.trace = TRUE, ### con esto le digo que me vaya diciendo como va la corrida
                   
                   ntree = cant.arboles.optimo , ## le digo el numero de arboles a armar
                   
                   keep.forest= TRUE , ## le digo que guarde el bosque para cuando uso la funcion predict tambien pueda calcular la proximidad
                   
                   proximity = TRUE) ## con esto le digo que me calcule la proximidad. La proximidad es la cantidad de veces que caen en la misma hoja 2 compuestos



plot(rf, main = "Gr�fico OOB error Random Forest") ## hago que grafique el error versus el numero de arboles en el training set

legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4) ## con esto hago que aparezcan las referencias en el grafico anterior

varImpPlot(rf, n.var=10, main= "Importancia de variables en RF") ## grafico la importancia de las variables segun Random Forest

MDSplot(rf = rf,fac = training$clase , k =3) # Plot the scaling coordinates of the proximity matrix from randomForest. Hace escalado multidimensional de la matrix de proximidad. es una forma de ver si me separa las clases

predicciones.train <- predict(object = rf, newdata = training, type="prob") ## predicciones en el training set expresadas como probabilidad

auc.training <- auc(roc(predictor = predicciones.train[,2],response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = rf, newdata = test, type="prob")  ## predicciones en el test set expresadas como probabilidad

auc.test <- auc(roc(predictor= predicciones.test[,2], response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 

resultado.rf <- list("Modelo armado por Random Forest", rf ,"Numero de �rboles �ptimo", cant.arboles.optimo , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.rf









######### GRAFICOS PARA ANALIZAR LOS RESULTADOS ############










test <-"Ddudesmiristoiltodos.csv"  ### nombre del test set

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

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo. Es una medida de calidad de clasificacion binaria

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte











################### GRAFICO DE SUPERFICIE 3D - PPV  #######################






dude <- "Ddudesmiristoiltodos.csv"

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

f1 <- list( size = 12) ## esto es si quiero cambiar algo de la fuente del titulo de los ejes

f2 <- list( size = 12) ## esto es si quiero cambiar algo de la fuente de las marcas de los ejes

axis.x <- list(title="Ya", ## opciones para el eje x
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T, ## si se muestra la cuadricula
               gridwidth = 3, ## el ancho de la linea de la cuadricula
               linewidth = 3, ## el ancho de la linea del eje
                color="white", ## color del titulo del eje
               gridcolor ="white", ### color de la linea de la cuadricula
               spikecolor = "white", ## color de la linea que se proyecta cuando estoy sobre el grafico
               spikesides = TRUE,
                zerolinecolor ="white") ## color de la linea del eje zero

axis.y <- list(title="Sensitivity/Specificity", ## opciones para el eje y
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 3,  ## el ancho de la linea de la cuadricula
               linewidth = 3, ## el ancho de la linea del eje
                color ="white", ## color del titulo del eje
               gridcolor="white",### color de la linea de la cuadricula
               spikecolor = "white",## color de la linea que se proyecta cuando estoy sobre el grafico
               spikesides =TRUE , 
               zerolinecolor ="white") ## color de la linea del eje zero

axis.z <- list(title="PPV",  ## opciones para el eje z
               titlefont = f1, ## para cambiar la fuente del titulo
               tickfont = f2, ## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 3,  ## el ancho de la linea de la cuadricula
               linewidth = 3, ## el ancho de la linea del eje
                color="white", ## color del titulo del eje
               gridcolor="white",### color de la linea de la cuadricula
               spikecolor = "white", ## color de la linea que se proyecta cuando estoy sobre el grafico
              spikesides = TRUE ,
                zerolinecolor ="white") ## color de la linea del eje zero

scene <- list(              ## resumo las info de los ejes en esta variable llamada "scene"
        xaxis = axis.x,
        yaxis = axis.y,
        zaxis = axis.z)



m <- list(                 ### esto es para ajustar los margenes
        l = 0,    ## margen left
        r = 0,    ## margen right
        b = 0,   ## margen bottom
        t = 60,   ## margen top
        pad = 0    ## margen del grafico a los ejes 
)

lighting <- list(  ### con esto seteo como son las sombras y las luces
        fresnel = 0.2,
        specular = 0.09,
        ambient = 0.88,
        diffuse = 0.99,
        roughness = 0.49
)













####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

p <- plot_ly(x= ~Prevalence, y = ~Sensitivity/Specificity, z = ~PPV, type = "surface", colors = "Spectral" ) %>% 
        
        colorbar(titlefont = list(color ="white"), tickfont = list(color = "rgb(255,255,255)") , lighting = lighting , contours = list(x = list(highlight = TRUE),y = list(highlight = TRUE),z = list(highlight = TRUE))) %>% 
        
        layout( paper_bgcolor="black" , plot_bgcolor= "black", title ="3D Surface PPV" , titlefont =list(color="white") , scene = scene , margin = m)  # hago el grafico de superficie 3D, aca especifico el nombre del grafico y luego en scene pongo la variable scene que tiene los formatos deseados

p

htmlwidgets::saveWidget(p, "PPV.html" , background = "black") ### GUARDO EL GR�FICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB









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





