###### VOY A ARMAR UNA FUNCION PARA VER LA PREDICCION POR EL ENSEMBLE VOTING EN LOS COMPUESTOS CUANDO HAGO EL SCREENING EN UNA BASE DE DATOS

clasificaciones.test.set.ensemble.voting.glm <- function (test.set = "Dtest.xlsx",cant.modelos = 10,fraccion.votos = 0.5, x = tabla.AUC.ordenadas.glm){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con la base de datos
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test.set <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
        
        secuencia.puntos.corte<-puntos.corte.ROC.glm[ , "cutoff"] #  extraigo los puntos de corte
        
        secuencia.puntos.corte2 <- secuencia.puntos.corte[mejores.segun.AUC] # solo los puntos de corte de los mejores modelos
        
        predicciones.redondeadas.test.set<-list() #lista vacia donde poner las predicciones redondeadas
        
        for( i in 1: length(lista.predicciones.test.set)) { # for loop donde lo que hago es que me redondee las predicciones
                predicciones.redondeadas.test.set[[i]] <- ifelse( lista.predicciones.test.set[[i]] > secuencia.puntos.corte2[i], yes = 1,no = 0)
                predicciones.redondeadas.test.set
        }
        
        matriz.predicciones.redondeadas.test.set <- matrix(unlist(predicciones.redondeadas.test.set), nrow= length(predicciones.redondeadas.test.set[[1]]), byrow=FALSE) #armo una matriz donde esten todas las predicciones redondeadas
        
        votos <- apply(X = matriz.predicciones.redondeadas.test.set, MARGIN = 1, sum, na.rm =TRUE) ## funcion donde hago es contar la cantidad de votos de si es activo
        
        predicciones.ensemble.voto.mayoria.test.set <- ifelse(votos > cant.modelos*fraccion.votos, yes=1,no=0 ) ## lo que hago es que me diga que si mas de la mitad me lo clasifica como +1 entonces es +1, sino 0
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(predicciones.ensemble.voto.mayoria.test.set,clase)
        
        colnames(resultado)<- c("voting", "clase")
        
        resultado
        
}

######## la anterior funcion hace que como resultado de mi test set me de una tabla con el score para voting y clase

lista.dude <- c("Dudes Myristoyl 1.xlsx","Dudes Myristoyl 2.xlsx","Dudes Myristoyl 3.xlsx","Dudes Myristoyl 4.xlsx","Dudes Myristoyl 5.xlsx","Dudes Myristoyl 6.xlsx","Dudes Myristoyl 7.xlsx","Dudes Myristoyl 8.xlsx","Dudes Myristoyl 9.xlsx","Dudes Myristoyl 10.xlsx","Dudes Myristoyl 11.xlsx","Dudes Myristoyl 12.xlsx","Dudes Myristoyl 13.xlsx","Dudes Myristoyl 1.xlsx","Dudes Myristoyl 15.xlsx","Dudes Myristoyl 16.xlsx", "Dudes Myristoyl 17.xlsx")## lista con los nombres de la base dude miristoil

lista.predicciones.dude <- list() ## creo lista vacia donde voy a guardar las predicciones

for ( i in 1:17) { ## hago loop para tener resultados de todas las dude
        
        lista.predicciones.dude[[i]] <- clasificaciones.test.set.ensemble.voting.glm(test.set   = lista.dude[i],cant.modelos = 100, fraccion.votos = 0.95, x = tabla.sensibilidad.ordenadas)  
}

library(plyr)

df <- ldply(lista.predicciones.dude, data.frame) ## combino las tablas de de las dudes para hacer una sola

library(pROC)

curva.ROC.dude <- roc(response = df$clase, predictor = df$voting) ## calculo de la curva ROC para los resultados de la base dude

       