
###########################################


   ## PRINCIPAL COMPONENT REGRESSION ##


###########################################



training.set <- "Dtrainingmiristoil.csv"

test.set <- "Dtestmiristoil.csv"

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pls") == FALSE) {install.packages("pls")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table)

library(pls)

library(caret)

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna con los nombres, dejo solo la columna clase con los descriptores

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set






##### HAGO PRINCIPAL COMPONENT REGRESSION HACIENDO CROSS-VALIDATION con  K = 10 ( que es el optimo para hacer cross validation), CON LOS GRAFICOS QUE OBTENGO VER CUANTOS COMPONENTES SERIAN LOS OPTIMOS SEGUN ESTE TIPO DE VALIDACION ######




# IMPORTANTE!!!!!!!!!!!!!!!!!!!!  ######


# si hay poca variabilidad en las columnas, 

#hace que el cross-validation tire error, 

#asi que por eso elimino las columnas con varianza cercana a cero








set.seed(2) ## seteo la semilla

pcr.fit <- pcr(clase ~ ., data = training  ,  scale= TRUE , validation ="CV" , segments = 10,  na.action = na.omit) ## hago Principal Component Regression, hago que los datos se escalen porque sino puede variar el resultado. A su vez hace crossvalidation con k = 10 para luego saber cual es la cant de componentes principales optima

summary(pcr.fit) ## sumario de los resultados obtenidos durante el fitteo

validationplot(pcr.fit , val.type="MSEP" , main = "MSEP Training set Cross-Validation - k = 10") ## me grafica el MSEP del LOO a medida que agrego componentes principales

validationplot( pcr.fit , val.type="R2" , main = "R2 Training set Cross-Validation - k = 10") ## me grafica el R2 del LOO a medida que agrego componentes principales

# no se porque el grafico de la linea de arriba tira error

validationplot( pcr.fit , val.type="RMSEP" , main = "RMSEP Training set Cross-Validation - k = 10") ## me grafica RMSEP del LOO a medida que agrego componentes  principales

validationplot(pcr.fit, ncomp = 1:pcr.fit$ncomp , val.type="MSEP", newdata = test , main = "MSEP Test set validation") ### me grafica el error en el test set a medida que agrego componentes principales

validationplot(pcr.fit, ncomp = 1:pcr.fit$ncomp , val.type="RMSEP", newdata = test , main = "RMSEP Test set validation") ### me grafica el error en el test set a medida que agrego componentes principales

validationplot(pcr.fit, ncomp = 1:pcr.fit$ncomp , val.type="R2", newdata = test , main = "R2 Test set validation") ### me grafica el error en el test set a medida que agrego componentes principales







###################################################################################################################################

### AHORA TENGO QUE ELEGIR CUAL ES EL NUMERO DE COMPONENTES PRINCIPALES OPTIMO, SEGUN TODOS LOS GRAFICOS ANTERIORES ################

####################################################################################################################################






N_comp_optimo <- 50 ### A PARTIR DE LOS GRAFICOS ANTERIORES MODIFICAR ESTE VALOR

predplot(pcr.fit, ncomp = N_comp_optimo  , main = "Predicted vs Measured - Training set") ### veo el comportamiento del training set segun la cant de componentes optimos que yo decidi por los graficos anteriores

plot(y = predict(object = pcr.fit,newdata =  test , ncomp = N_comp_optimo  , type ="response"), x =test$clase , main = "Predicted vs Measured - Test set" , xlab ="measured" , ylab ="predicted") ## veo el comportamiento del test set segun la cant de componentes optimos que yo decidi con los graficos anteriores

predicciones.train <- predict(object = pcr.fit,newdata =  training , ncomp = N_comp_optimo  , type ="response") ## predicciones en el training set

library(pROC)

auc.training <- auc(roc(predictor = predicciones.train ,response = training$clase, direction = "<", plot = TRUE, main ="ROC Training set - PCR")) ## calculo la curva ROC para el training set

predicciones.test <- predict(object = pcr.fit,newdata =  test , ncomp = N_comp_optimo  , type ="response") ## predicciones en el Test set

auc.test <- auc(roc(predictor= predicciones.test, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set - PCR")) ## calculo de curva ROC para el test set 

resultado <- list("Modelo armado por Principal Component Regression", pcr.fit ,"Numero de Componentes Principales �ptimo", N_comp_optimo , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado





######### GRAFICOS PARA VER LA PERFORMANCE EN EL TEST SET O LA DUDE  #######




test.set <- "Dtestmiristoil.csv"

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

predicciones.test <- as.data.frame(predict(object = pcr.fit,newdata =  test , ncomp = N_comp_optimo  , type ="response")) ## predicciones en el Test set

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test, labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte




################### GRAFICO DE SUPERFICIE 3D - PPV  #######################




dude <- "Ddudes1miristoil.csv"

dude <- as.data.frame(fread(input = dude, check.names = TRUE)) #leo el archivo con mis descriptores del dude set

predicciones.dude <- as.vector(predict(object = pcr.fit,newdata =  dude , ncomp = N_comp_optimo  , type ="response")) ## predicciones en el Test set

curva.ROC.dude <- roc(response = dude$clase, predictor = predicciones.dude, direction = "<") ## calculo de la curva ROC para los resultados de la base dude

auc(curva.ROC.dude) ### imprimo el AUC de la curva ROC para ver si coincide con los resultados anteriores

tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de sensibilidad y especificidad, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold sensitivity y specificity

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

htmlwidgets::saveWidget(as.widget(p), "PPV.html") ### GUARDO EL GR�FICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB









###############  PREDICCIONES EN BASES DE DATOS  #################









setwd("D:/Dropbox/R/descriptores drugbank") ### seteo la carpeta de drugbank. Si lo hago con sweatlead tengo que setear otra carpeta

base.datos <- "base drugbank 24-10-16.csv" ### nombre del archivo con la base de datos

df.base.datos <- as.data.frame(fread(input=base.datos, check.names = TRUE)) #leo el archivo con la base de datos

predicciones.base.datos <- as.data.frame(predict(object = pcr.fit,newdata =  df.base.datos , ncomp = N_comp_optimo  , type ="response")) ## predicciones en el Test set

colnames(predicciones.base.datos) <- c("SCORE - PCR")

predicciones.base.datos$NOMBRE <- df.base.datos$NAME

library(openxlsx)

write.xlsx(x= predicciones.base.datos, file= "Screening por Principal Component Regression.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos




