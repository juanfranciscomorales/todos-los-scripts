
setwd("D:/Dropbox/R/descriptores drugbank")

mejores.compuestos.drug.bank <- read.xlsx(xlsxFile = "tabla scores drugbank.xlsx", check.names = TRUE)$NOMBRE[1:500] ## extraigo los nombres de los primeros 500 compuestos predichos como activos. Al archivo "tabla scores drugbank.xlsx" primero le elimino los compuestos sin scores y luego los ordeno de mayor a menor, esto lo hago antes de leer el archivo

lista.drugbank <- c("drug bank 1.xlsx","drug bank 2.xlsx","drug bank 3.xlsx","drug bank 4.xlsx","drug bank 5.xlsx","drug bank 6.xlsx","drug bank 7.xlsx","drug bank 8.xlsx","drug bank 9.xlsx","drug bank 10.xlsx","drug bank 11.xlsx","drug bank 12.xlsx","drug bank 13.xlsx","drug bank 14.xlsx") ##creo una lista con los nombres de los archivos de drugbank

lista.df.mejores.compuestos.drugbank <- list() ##creo una lista vacia donde voy a poner las filas que extraigo

for (i in 1:length(lista.drugbank)) {
        
        df.drugbank <- read.xlsx(xlsxFile= lista.drugbank[i], check.names = TRUE) ## leo el excel de cada drugbank
        
        lista.df.mejores.compuestos.drugbank[[i]] <-  df.drugbank[df.drugbank$GENERIC_NAME %in% mejores.compuestos.drug.bank ,  ] ## extraigo los compuestos que estan dentro de los 500 compuestos mejor clasificados
        
}

library(plyr) ##cargo el paquete plyr

tabla.descriptores.drugbank <- ldply(lista.df.mejores.compuestos.drugbank, data.frame) ### hago que pase de una lista de data frames a un solo data frame que contiene los descriptores de los 500 compuestos mejor clasificados en la base de drug bank



score.base.datos.mejores.modelos.lm <- function (base.datos = tabla.descriptores.drugbank ,cant.modelos = 10, x = tabla.AUC.ordenadas.dude){
        
        df.base.datos <- as.data.frame(base.datos) #leo el archivo con la base de datos
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con los mejores modelos
        
        lista.predicciones.base.datos <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
        
        tabla.valores.prediccion.base.datos <- data.frame(matrix(unlist(lista.predicciones.base.datos), nrow= length(lista.predicciones.base.datos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        colnames(tabla.valores.prediccion.base.datos)<- paste(rep("modelo" , length(mejores.segun.AUC)),paste(mejores.segun.AUC) ) ## le cambio el nombre a la columna
        
        rownames(tabla.valores.prediccion.base.datos) <- df.base.datos$GENERIC_NAME #agrego la columna de los nombres de cada compuesto
        
        tabla.valores.prediccion.base.datos ##  este es el resultado final
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.mejores.500.base.datos <- score.base.datos.mejores.modelos.lm(base.datos = tabla.descriptores.drugbank ,cant.modelos = 25, x = tabla.AUC.ordenadas.dude) ## si quiero que sea por AUC

write.xlsx(x= tabla.predicciones.mejores.500.base.datos, file= "tabla scores mejores 500.xlsx" , colNames= TRUE, rowNames = TRUE,  keepNA=TRUE) 


