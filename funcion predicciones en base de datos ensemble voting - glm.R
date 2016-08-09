###### VOY A ARMAR UNA FUNCION PARA VER LA PREDICCION POR EL ENSEMBLE VOTING EN LOS COMPUESTOS CUANDO HAGO EL SCREENING EN UNA BASE DE DATOS

clasificaciones.base.datos.ensemble.voting.glm <- function (base.datos = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas.glm){
  
  is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
  
  if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
  
  library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
  
  df.base.datos <- read.xlsx(xlsxFile=base.datos, check.names = TRUE) #leo el archivo con la base de datos
  
  mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
  
  lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores modelos
  
  lista.predicciones.base.datos <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
  
  secuencia.puntos.corte<-puntos.corte.ROC.glm[ , "cutoff"] #  extraigo los puntos de corte
  
  secuencia.puntos.corte2 <- secuencia.puntos.corte[mejores.segun.AUC] # solo los puntos de corte de los mejores modelos
  
  predicciones.redondeadas.base.datos<-list() #lista vacia donde poner las predicciones redondeadas
  
  for( i in 1: length(lista.predicciones.base.datos)) { # for loop donde lo que hago es que me redondee las predicciones
    predicciones.redondeadas.base.datos[[i]] <- ifelse( lista.predicciones.base.datos[[i]] > secuencia.puntos.corte2[i], yes = 1,no = 0)
    predicciones.redondeadas.base.datos
  }
  
  matriz.predicciones.redondeadas.base.datos <- matrix(unlist(predicciones.redondeadas.base.datos), nrow= length(predicciones.redondeadas.base.datos[[1]]), byrow=FALSE) #armo una matriz donde esten todas las predicciones redondeadas
  
  votos <- apply(X = matriz.predicciones.redondeadas.base.datos, MARGIN = 1, sum, na.rm =TRUE) ## funcion donde hago es contar la cantidad de votos de si es activo
  
  predicciones.ensemble.voto.mayoria.base.datos <- ifelse(votos > cant.modelos/2, yes=1,no=0 ) ## lo que hago es que me diga que si mas de la mitad me lo clasifica como +1 entonces es +1, sino 0
  
  predicciones.ensemble <- ifelse(predicciones.ensemble.voto.mayoria.base.datos == 1, yes = "activo", no = "inactivo") ## hago que si es +1 lo reemplace por activo, sino por inactivo
  
  tabla.predicciones.ensemble <- data.frame(predicciones.ensemble) ## lo transformo en data frame porque es una matrix
  
  colnames(tabla.predicciones.ensemble)<- "Predicción por Ensemble Voting" ## le cambio el nombre a la columna

  tabla.predicciones.ensemble ##  este es el resultado final
  
  }

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.voting.glm(base.datos  = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas.glm) ## si quiero que sea por AUC

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.voting.glm(base.datos  = "Dtest.xlsx",cant.modelos = 10, x = tabla.sensibilidad.ordenadas) ## si quiero que sea por modelos con mayor sensibilidad
 