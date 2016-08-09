#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE VOTING

clasificaciones.test.set.glm <- function (test.set = "Dtest.xlsx", lista.de.modelos = lista.modelos){
  
  is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
  
  if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
  
  library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
  
  df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
  
  lista.predicciones.test <- lapply(X = lista.de.modelos , FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
  
  secuencia.puntos.corte<-puntos.corte.ROC.glm[ , "cutoff"] # puntos de corte
  
  predicciones.redondeadas.test<-list()
  
  for( i in 1: length(lista.predicciones.test)) {
    predicciones.redondeadas.test[[i]] <- ifelse( lista.predicciones.test[[i]] > secuencia.puntos.corte[i], yes = 1,no = 0)
    predicciones.redondeadas.test
  }
  tabla.predicciones.redondeadas.test <- data.frame(matrix(unlist(predicciones.redondeadas.test), nrow= length(predicciones.redondeadas.test[[1]]), byrow=FALSE)) #armo una matriz don
  
  clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
  
  lista.bien.clasificados <- list() # creo una lista vacia donde guardar los bien clasificados
  
  for (i in 1:ncol(tabla.predicciones.redondeadas.test)) {
    lista.bien.clasificados[[i]] <- tabla.predicciones.redondeadas.test[,i] == clase ##me fijo cuales valores predichos estan bien clasificados en el test set
  lista.bien.clasificados
    }
  
  lista.porcentaje.bien.clasificados <- lapply(lista.bien.clasificados, function (x) 100*sum(x)/length(x)) #porcentaje de buenas clasificaciones en el test set
  
 df.resultados<-data.frame(unlist(lista.porcentaje.bien.clasificados))
 
 colnames(df.resultados)<- c("% buenas clasificaciones test set")
 
 df.resultados
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.glm(test.set = "Dtest.xlsx", lista.de.modelos = lista.modelos)
