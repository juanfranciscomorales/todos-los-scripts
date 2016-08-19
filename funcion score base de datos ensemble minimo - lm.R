clasificaciones.base.datos.ensemble.minimo.lm <- function (base.datos = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.base.datos <- read.xlsx(xlsxFile=base.datos, check.names = TRUE) #leo el archivo con la base de datos
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con los mejores modelos
        
        lista.predicciones.base.datos <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
        
        tabla.valores.prediccion.base.datos <- data.frame(matrix(unlist(lista.predicciones.base.datos), nrow= length(lista.predicciones.base.datos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        minimo <- apply(tabla.valores.prediccion.base.datos,1,min)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto
        
        tabla.predicciones.ensemble <- data.frame(minimo) ## lo transformo en data frame porque es una matrix
        
        colnames(tabla.predicciones.ensemble)<- "Score Ensemble Operador Minimo" ## le cambio el nombre a la columna
        
        tabla.predicciones.ensemble$NOMBRE <- df.base.datos[, "GENERIC_NAME"] #agrego la columna de los nombres de cada compuesto
        
        tabla.predicciones.ensemble ##  este es el resultado final
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.minimo.lm(base.datos  = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas) ## si quiero que sea por AUC

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.minimo.lm(base.datos  = "Dtest.xlsx",cant.modelos = 10, x = tabla.sensibilidad.ordenadas) ## si quiero que sea por modelos con mayor sensibilidad
