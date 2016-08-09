#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE VOTING

clasificaciones.test.set.ensemble.voting.glm <- function (test.set = "Dtest.xlsx",cant.modelos = 10,fraccion.votos = 0.5, x = tabla.sensibilidad.ordenadas){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        mejores.segun.sensibilidad <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.sensibilidad]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        secuencia.puntos.corte<-puntos.corte.ROC.glm[ , "cutoff"] # puntos de corte
        
        secuencia.puntos.corte2 <- secuencia.puntos.corte[mejores.segun.sensibilidad] # solo los puntos de corte de los mejores modelos
        
        predicciones.redondeadas.test<-list()
        
        for( i in 1: length(lista.predicciones.test)) {
                predicciones.redondeadas.test[[i]] <- ifelse( lista.predicciones.test[[i]] > secuencia.puntos.corte2[i], yes = 1,no = 0)
                predicciones.redondeadas.test
        }
        
        matriz.predicciones.redondeadas.test <- matrix(unlist(predicciones.redondeadas.test), nrow= length(predicciones.redondeadas.test[[1]]), byrow=FALSE) #armo una matriz don
        
        votos <- apply(X = matriz.predicciones.redondeadas.test, MARGIN = 1, sum, na.rm =TRUE)
        
        predicciones.ensemble.voto.mayoria.test <- ifelse(votos > cant.modelos*fraccion.votos, yes=1,no=0 )
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        bien.clasificados <- predicciones.ensemble.voto.mayoria.test == clase ## veo si el voto me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
        
        tabla.bien.mal.clasificados <- table(predicciones.ensemble.voto.mayoria.test ,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") #tabla para mostrar los bien y mal clasificados
        
        predichos.activos <- tabla.bien.mal.clasificados[2,2]
        
        predichos.inactivos <- tabla.bien.mal.clasificados[1,1]
        
        reales.activos <- sum(tabla.bien.mal.clasificados[,2])
        
        reales.inactivos <- sum(tabla.bien.mal.clasificados[,1])
        
        tabla.resultado <-cbind(predichos.activos,predichos.inactivos,reales.activos,reales.inactivos)
        
        tabla.resultado
        }

######## la anterior funcion hace que como resultado de mi test set me de una tabla con el score para voting y clase

lista.dude <- c("Dudes Myristoyl 1.xlsx","Dudes Myristoyl 2.xlsx","Dudes Myristoyl 3.xlsx","Dudes Myristoyl 4.xlsx","Dudes Myristoyl 5.xlsx","Dudes Myristoyl 6.xlsx","Dudes Myristoyl 7.xlsx","Dudes Myristoyl 8.xlsx","Dudes Myristoyl 9.xlsx","Dudes Myristoyl 10.xlsx","Dudes Myristoyl 11.xlsx","Dudes Myristoyl 12.xlsx","Dudes Myristoyl 13.xlsx","Dudes Myristoyl 1.xlsx","Dudes Myristoyl 15.xlsx","Dudes Myristoyl 16.xlsx", "Dudes Myristoyl 17.xlsx")## lista con los nombres de la base dude miristoil

lista.por.fraccion.votos <- list() ## creo lista vacia donde voy a guardar las predicciones



for (i in seq(from =0, to =1, by =0.01)) {

lista.por.fraccion.votos [[i]]<-lapply(lista.dude, clasificaciones.test.set.ensemble.voting.glm, cant.modelos = 100, fraccion.votos = i , x = tabla.sensibilidad.ordenadas)

}

### me tira el siguiente error

### Error in `[.default`(tabla.bien.mal.clasificados, 2, 2) : subscript out of bounds 
library(plyr)

df <- ldply(lista.predicciones.dude, data.frame) ## combino las tablas de de las dudes para hacer una sola

library(pROC)

curva.ROC.dude <- roc(response = df$clase, predictor = df$voting) ## calculo de la curva ROC para los resultados de la base dude

