prediccion.test.set.lm <- function (test.set = "Dtest.xlsx", modelo) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        prediccion <- predict(object = modelo, newdata = df.test.set, type="response")
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(prediccion,clase)
        
        colnames(resultado)<- c("prediccion", "clase")
        
        resultado
        
}

lista.dude <- c("Test + Dudes 1.xlsx","Test + Dudes 2.xlsx","Test + Dudes 3.xlsx","Test + Dudes 4.xlsx","Test + Dudes 5.xlsx","Test + Dudes 6.xlsx","Test + Dudes 7.xlsx","Test + Dudes 8.xlsx","Test + Dudes 9.xlsx","Test + Dudes 10.xlsx","Test + Dudes 11.xlsx","Test + Dudes 12.xlsx","Test + Dudes 13.xlsx","Test + Dudes 14.xlsx","Test + Dudes 15.xlsx","Test + Dudes 16.xlsx", "Test + Dudes 17.xlsx")## lista con los nombres de la base dude miristoil

lista.predicciones.dude <- list() ## creo lista vacia donde voy a guardar las predicciones

for ( i in 1:17) { ## hago loop para tener resultados de todas las dude
        
        lista.predicciones.dude[[i]] <- prediccion.test.set.lm(test.set   = lista.dude[i], modelo = lista.modelos[[534]])  
}

library(plyr)

df <- ldply(lista.predicciones.dude, data.frame) ## combino las tablas de de las dudes para hacer una sola

df$clase[df$clase == -1] <- 0

curva.roc.dude <- roc(predictor = df$prediccion ,response = df$clase)

AUC.dude <- auc(curva.roc.dude)

predicciones <- ifelse( df$prediccion > puntos.corte.ROC.lm$cutoff[534], yes = 1,no = 0) ## predicciones aplicando el ensemble de operador minimo y usando el punto de corte que obtuve con el training

clase <-df$clase #extraigo los valores de la columna clase

tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany")  ##armo la tabla clasificatoria 

bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien

porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el test set

resultado.final <- list("AUC de la curva ROC", AUC.dude, "punto de corte", resultados.ensemble.minimo[[4]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion

resultado.final ## pongo el resultado final

