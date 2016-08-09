#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET

clasificaciones.test.set.glm <- function (test.set = "Dtest.xlsx", lista.de.modelos = lista.modelos){

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente

library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx

df.test.set <- read.xlsx(xlsxFile=test.set) #leo el archivo con el test set

lista.predicciones.test <- lapply(X = lista.de.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set

lista.resultados.test.inactivos <- list()
for ( i in 1:length(lista.predicciones.test)) {
  lista.resultados.test.inactivos[[i]]<-   lista.predicciones.test[[i]] < puntos.corte.ROC.glm$cutoff[[i]]    
      lista.resultados.test.inactivos  
}
lista.cant.inactivos<-lapply(lista.resultados.test.inactivos,sum)

porcentaje <- function (x) {
        j<-100*as.numeric(x)/nrow(df.test.set)
        j}
lista.porcentaje.inactivos <-lapply(lista.cant.inactivos,porcentaje)
df.resultados<-data.frame(unlist(lista.porcentaje.inactivos))
colnames(df.resultados)<- c("% buenas clasificaciones")
df.resultados
}
######## ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

tabla.BC.test.set<-clasificaciones.test.set.glm(test.set = "Dtest.xlsx", lista.de.modelos = lista.modelos)