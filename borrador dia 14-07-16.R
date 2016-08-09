setwd("C:/Users/Administrador/Dropbox/R/lucas alberca/Base de datos - Myristoyl/Dude Myristoyl")

library(openxlsx) ## cargo el paquete openxlsx

df <- read.xlsx(xlsxFile="Training Set Myristoyl.xlsx", check.names = TRUE) ## leo los descriptores. pase a los clase de inactivos como -1 y de activos como 1

lm1 <- lm(clase ~ ATSC3i + F04.N.O. + Eig05_EA.bo. + F10.C.N.+ SpMAD_Dz.p., data = df) ##armo modelo 1

lm11 <- lm(clase ~ ATSC4v + B06.N.O. + CATS2D_07_DA + F08.N.S. , data = df)##  armo modelo 11

lm27 <- lm(clase ~ ATSC3p + B07.N.N. + F10.C.N. + Eig05_AEA.bo.	+ T.O..O. , data = df)##  armo modelo 27

lm31 <- lm(clase ~ ON1V + F03.N.S. + CATS2D_07_DA + NssO + P_VSA_i_2 , data = df)##  armo modelo 31

lm41 <- lm(clase ~ ATSC3p + N.067 + CATS2D_02_DL + T.O..S. , data = df)##  armo modelo 41

lista.modelos <- list(lm1,lm11,lm27,lm31,lm41) ##pongo a los modelos en una lista

###########################################

AUC.curvas.ROC <- function ( x = lista.modelos ) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente
        
        library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion
        
        lista.predicciones <- lapply( x , predict)#calculos los valores predecidos por el modelo para cada compuesto
        
        lista.roc<-list() #creo una lista vacia donde voy a poner los calculos de las curvas ROC
        
        for (i in 1:length(lista.predicciones)) { #loop para el calculo de las curvas ROC de todos los modelos
                q<-factor(ifelse(df$clase==1,1,0)) # creo el factor que va a usar la funcion roc asi hace las curvas
                lista.roc[[i]]<- roc(predictor = lista.predicciones[[i]],response = q) #calculo la curva ROC para cada conjunto y la guardo en una lista
                lista.roc # lista con las curvas ROC
        }
        
        lista.auc.roc<-list() #creo una lista vacia donde guardar los valorse de AUC de la curvas ROC
        
        for (i in 1:length(lista.roc)) { # loop para el calculo de AUC de las curvas ROC para todos los modelos
                lista.auc.roc[[i]]<- auc(lista.roc[[i]]) #calculo de la AUC para cada modelo y lo guardo en una lista
                lista.auc.roc #lista con las AUC de las curvas ROC
        }
        
        lista.auc.roc.numeric<-list()  # loop para extraer los valores de AUC como numeros
        for (i in 1:length(lista.auc.roc)){
                lista.auc.roc.numeric[[i]]<- summary(lista.auc.roc[[i]])[["Median"]]
                lista.auc.roc.numeric #lista con los valores absolutos de las AUC
        }
        
        models.ranking.by.auc.roc<-order(unlist(lista.auc.roc.numeric), decreasing = TRUE) # ordenar de forma decreciente los valores de AUC de todos los modelos
        
        matrix.ranking <- cbind(modelo = models.ranking.by.auc.roc,AUC = lista.auc.roc.numeric[models.ranking.by.auc.roc]) #tabla donde obtengo el ranking de modelos segun AUC de la curva ROC, es una matrix
        tabla.ranking <-as.data.frame(matrix.ranking)#hago que la matrix se vuelva data frame
        tabla.ranking<-data.frame(apply(X=tabla.ranking, MARGIN = 2, FUN = unlist))# hago de manera que mis columnas que son listas se vuelvan numeros solamente, para poder luego guardarlos con la funcion write.xlsx
        tabla.ranking #para que me tire el data frame ordenado
}
###### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

tabla.AUC.ordenadas <- AUC.curvas.ROC( x= lista.modelos) # aplico mi funcion a la lista de modelos armada con la funcion anterior, como resultado obtengo una tabla con los valores de AUC ordenadas por mejores modelos

###########################

clasificaciones.test.set.ensemble.minimo.lm <- function (test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        minimo<-apply(tabla.valores.prediccion.test,1,min)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(minimo,clase)
        
        colnames(resultado)<- c("minimo", "clase")
        
        resultado
        
}

lista.dude <- c("Test + Dudes 1.xlsx","Test + Dudes 2.xlsx","Test + Dudes 3.xlsx","Test + Dudes 4.xlsx","Test + Dudes 5.xlsx","Test + Dudes 6.xlsx","Test + Dudes 7.xlsx","Test + Dudes 8.xlsx","Test + Dudes 9.xlsx","Test + Dudes 10.xlsx","Test + Dudes 11.xlsx","Test + Dudes 12.xlsx","Test + Dudes 13.xlsx","Test + Dudes 14.xlsx","Test + Dudes 15.xlsx","Test + Dudes 16.xlsx", "Test + Dudes 17.xlsx")## lista con los nombres de la base dude miristoil

lista.predicciones.dude <- list() ## creo lista vacia donde voy a guardar las predicciones

for ( i in 1:17) { ## hago loop para tener resultados de todas las dude
        
        lista.predicciones.dude[[i]] <- clasificaciones.test.set.ensemble.minimo.lm(test.set   = lista.dude[i],cant.modelos = 5, x = tabla.AUC.ordenadas)  
}

library(plyr)

df <- ldply(lista.predicciones.dude, data.frame) ## combino las tablas de de las dudes para hacer una sola

df2<- df[!is.na(df$minimo),]

write.xlsx(x= df2, file= "scores minimo lucas.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla con los score minimos calculados

tabla.puntos.curva.roc.limpia<- read.xlsx(xlsxFile="tabla sp se dude test lucas medcalc.xlsx", check.names = TRUE) 

### usar todos los datos de la tabla tabla.puntos.curva.roc hacer que no se pueda visualizar bien. Conviene acotar la tabla a los datos donde sensibilidad/especificidad es de 2.5 a 0.01 aprox

sensibilidad <- tabla.puntos.curva.roc.limpia$sensitivity ##extraigo los valores de sensibilidad

especificidad <- tabla.puntos.curva.roc.limpia$specificity##extraigo los valores de especificidad

prevalencia <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de prevalencias donde voy a calcular el PPV

list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados

for (i in 1:length(prevalencia)){ ##loop donde para cada prevalencia hago un barrido para los diferentes valores de sensibilidad/especificidad, asi calculo el PPV
        
        list.PPV[[i]] <- (sensibilidad*prevalencia[i])/(sensibilidad*prevalencia[i] + (1- especificidad)*(1- prevalencia[i]))##calculo del PPV
        
}

PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes prevalencias y las filas son las diferentes relaciones sensibilidad/especificidad, y los valores de cada celda es la PPV correspondiente para esos valores

library(plotly)

p<-plot_ly(x= prevalencia, y = sensibilidad/especificidad, z = PPV , type = "surface") ##grafico de superficie 3D

####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

htmlwidgets::saveWidget(as.widget(p), "index.html") ### GUARDO EL GRÁFICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB

