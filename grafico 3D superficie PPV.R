clasificaciones.test.set.ensemble.minimo.lm <- function (test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        minimo<-apply(tabla.valores.prediccion.test,1,min, na.rm=TRUE)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(minimo,clase)
        
        colnames(resultado)<- c("minimo", "clase")
        
        resultado
        
}

######## la anterior funcion hace que como resultado de mi test set me de una tabla con el score para minimo y clase

setwd("D:/Dropbox/R/lucas alberca/Base de datos - Myristoyl/Dude Myristoyl")

lista.dude <- c("Test + Dudes 1.xlsx","Test + Dudes 2.xlsx","Test + Dudes 3.xlsx","Test + Dudes 4.xlsx","Test + Dudes 5.xlsx","Test + Dudes 6.xlsx","Test + Dudes 7.xlsx","Test + Dudes 8.xlsx","Test + Dudes 9.xlsx","Test + Dudes 10.xlsx","Test + Dudes 11.xlsx","Test + Dudes 12.xlsx","Test + Dudes 13.xlsx","Test + Dudes 14.xlsx","Test + Dudes 15.xlsx","Test + Dudes 16.xlsx", "Test + Dudes 17.xlsx")## lista con los nombres de la base dude miristoil

lista.predicciones.dude <- list() ## creo lista vacia donde voy a guardar las predicciones

for ( i in 1:17) { ## hago loop para tener resultados de todas las dude
        
        lista.predicciones.dude[[i]] <- clasificaciones.test.set.ensemble.minimo.lm(test.set   = lista.dude[i],cant.modelos = 5, x = tabla.AUC.ordenadas.dude)  
}

library(plyr)

df <- ldply(lista.predicciones.dude, data.frame) ## combino las tablas de de las dudes para hacer una sola

library(pROC)

curva.ROC.dude <- roc(response = df$clase, predictor = df$minimo) ## calculo de la curva ROC para los resultados de la base dude

tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de sensibilidad y especificidad, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold sensitivity y specificity

write.xlsx(x= tabla.puntos.curva.roc, file= "tabla.puntos.curva.roc.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de la dude si quiero

tabla.puntos.curva.roc.limpia<- read.xlsx(xlsxFile="tabla.puntos.curva.roc.xlsx", check.names = TRUE) 

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

Sys.setenv("plotly_username" ="juanfranciscomorales") ##seteo mi usuario de plotly

Sys.setenv("plotly_api_key"="9lb4lh4kcy") ##seteo mi api key que figura en la siguiente pagina https://plot.ly/settings/api

plotly_IMAGE(x=p, width = 1920, height = 1080, format = "pdf",  out_file = "lucas.pdf")

