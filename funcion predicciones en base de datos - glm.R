###### VOY A ARMAR UNA FUNCION PARA VER LA PREDICCION EN LOS COMPUESTOS CUANDO HAGO EL SCREENING EN UNA BASE DE DATOS

clasificaciones.base.datos.glm <- function (base.datos = "Dtest.xlsx", lista.de.modelos = lista.modelos){
  
  is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
  
  if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
  
  library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
  
  df.base.datos <- read.xlsx(xlsxFile=base.datos, check.names = TRUE) #leo el archivo con el test set
  
  lista.predicciones.base.datos <- lapply(X = lista.de.modelos , FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
  
  secuencia.puntos.corte<-puntos.corte.ROC.glm[ , "cutoff"] # selecciono los puntos de corte
  
  predicciones.redondeadas.base.datos<-list() ## lista vacia donde voy a poner las predicciones
  
  for( i in 1: length(lista.predicciones.base.datos)) { ## for loop donde lo que hago es que me redondee la prediccion a 1 o 0 en base al punto de corte
    predicciones.redondeadas.base.datos[[i]] <- ifelse( lista.predicciones.test[[i]] > secuencia.puntos.corte[i], yes = 1,no = 0) 
    predicciones.redondeadas.base.datos
  }
  tabla.predicciones.redondeadas.base.datos <- data.frame(matrix(unlist(predicciones.redondeadas.test), nrow= length(predicciones.redondeadas.test[[1]]), byrow=FALSE)) #armo una tabla donde pongo los resultados segun el redondeo a si es 1 o 0.
  
  tabla.predicciones <- as.data.frame(ifelse(tabla.predicciones.redondeadas.base.datos == 1, yes = "activo", no = "inactivo")) # se hace una tabla donde digo que si el valor es 1 es activo y si es 0 es inactivo
  
  tabla.predicciones ## resultado final
 
  
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.base.datos <- clasificaciones.base.datos.glm(test.set = "Dtest.xlsx", lista.de.modelos = lista.modelos)
