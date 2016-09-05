

mejores.segun.AUC <- as.numeric(tabla.AUC.ordenadas.dude[c(1:5),"modelo"]) ### me fijo cuales son los mejores modelos

lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]## armo una lista con los mejores modelos

lista.nombre.descriptores <- lapply (X = lista.mejores.modelos , FUN = variable.names) ## extraigo los nombres de los descriptores elegidos por los mejores modelos

lista.nombre.descriptores2 <- lapply( lista.nombre.descriptores, function (x) x[!x %in% c("(Intercept)") ]) ### elimino la variable intercept asi me quedan solo los nombres de los descriptores

setwd("D:/Dropbox/R/lucas alberca/Base de datos - Myristoyl/Nuevas base de datos Myristoyl")## seteo la carpeta de trabajo a donde esta el training set

lista.df <- list() ## creo una lista vacia donde voy a guardar los data frames de cada modelo que contiene los valrose de los descriptores

for (i in 1:length(lista.nombre.descriptores2)) {   ### hago un for loop para extraer las columnas de los descriptores de cada modelo
        
        library(openxlsx)
        
        df <-  read.xlsx(xlsxFile = "TrainingSet.xlsx" , check.names = TRUE ) ## armo un data frame con el training set
        
        lista.df[[i]] <- df[ , lista.nombre.descriptores2[[i]] ]
}

lista.matrix <- lapply(lista.df, as.matrix) ### hago que mi lista de data frames se vuelva una lista de matrices

multiplicado <- lapply( lista.matrix , function (M) t(M) %*% M) ## multiplico la matriz con los descriptores del modelo del traning por su transpuesta

inversa <- lapply(multiplicado , solve) ## me calcula la inversa de la matriz

########## hasta aca trabaje con el training, ahora voy a comenzar con sweatlead

setwd("C:/Users/Francisco/Dropbox/R/descriptores sweatlead")

mejores.compuestos.sweatlead <- read.xlsx(xlsxFile = "tabla scores sweatlead.xlsx", check.names = TRUE)$NOMBRE[1:500] ## extraigo los nombres de los primeros 500 compuestos predichos como activos. Al archivo "tabla scores sweatlead.xlsx" primero le elimino los compuestos sin scores y luego los ordeno de mayor a menor, esto lo hago antes de leer el archivo

lista.sweatlead <- c("SW1.xlsx", "SW2.xlsx" , "SW3.xlsx" , "SW4.xlsx", "SW5.xlsx" , "SW6.xlsx" , "SW7.xlsx" , "SW8.xlsx", "SW9.xlsx")

lista.df.mejores.compuestos.sweatlead <- list() ##creo una lista vacia donde voy a poner las filas que extraigo

for (i in 1:length(lista.sweatlead)) {
        
        df.sweatlead <- read.xlsx(xlsxFile= lista.sweatlead[i], check.names = TRUE) ## leo el excel de cada sweatlead
        
        lista.df.mejores.compuestos.sweatlead[[i]] <-  df.sweatlead[df.sweatlead$NAME %in% mejores.compuestos.sweatlead ,  ] ## extraigo los compuestos que estan dentro de los 500 compuestos mejor clasificados
        
}

library(plyr) ##cargo el paquete plyr

tabla.descriptores.sweatlead <- ldply(lista.df.mejores.compuestos.sweatlead, data.frame) ### hago que pase de una lista de data frames a un solo data frame que contiene los descriptores de los 500 compuestos mejor clasificados en la base de drug bank

lista.df.sweatlead <- list() ## creo una lista vacia donde voy a guardar los descriptores de cada modelo

for (i in 1:length(lista.nombre.descriptores2)) {  #### hago un for loop para extraer de la tabla con todos los descriptores de los 500 mejores de sweatlead, los descriptores que se corresponden a cada modelo
        
        lista.df.sweatlead[[i]] <- tabla.descriptores.sweatlead[ , lista.nombre.descriptores2[[i]] ]
}

lista.matrix.sweatlead <- lapply(lista.df.sweatlead, as.matrix) ### hago que mi lista de data frames se vuelva una lista de matrices

lista.matrix.sweatlead.traspuesta <- lapply(lista.matrix.sweatlead, t) ### obtengo las matrices traspuestas de las matrices de sweatlead

multiplicado2 <- list() ## lista vacia

for (i in 1: length(lista.matrix.sweatlead)) {   ### hago un for loop para hacer las multiplicaciones de matrices
        
        multiplicado2[[i]] <- lista.matrix.sweatlead[[i]] %*% inversa[[i]]
        
}

multiplicado3 <- list()   ### lista vacia

for (i in 1: length(multiplicado2)) { ### hago la multiplicacion de las matrices
        
        multiplicado3[[i]] <- multiplicado2[[i]] %*% lista.matrix.sweatlead.traspuesta[[i]]
        
}

diagonales <- lapply (multiplicado3 , diag) ### extraigo las diagonales de la matriz resultado de la ultima multiplicacion

lista.h <- list() ## creo lista vacia

for ( i in 1 :length(diagonales)) {  ### hago un for loop para calcular los valores de corte h*
        
        lista.h[[i]] <- 3*length(lista.nombre.descriptores2[[i]])/nrow(df)  ## la cuenta es 3 por el num de descriptores del modelo dividido el numero de compuestos en el training set
        
}


lista.resultados <- list() ### creo lista vacia

for ( i in 1:length(diagonales)) { 
        
        lista.resultados[[i]] <-  diagonales[[i]]< lista.h[[i]]
}

tabla.resultados <- data.frame(matrix(unlist(lista.resultados), ncol= length(lista.resultados), byrow=FALSE))### a la lista de resultados la vuelvo data frame haciendo que cada columna sea un modelo y cada fila un compuesto

row.names(tabla.resultados) <- tabla.descriptores.sweatlead$NAME

colnames(tabla.resultados) <- paste(rep("modelo" , length(mejores.segun.AUC)),paste(mejores.segun.AUC) )

write.xlsx(x= tabla.resultados, file= "tabla resultados leverage sweatlead.xlsx" , colNames= TRUE, rowNames = TRUE,  keepNA=TRUE) # funcion para guardar la tabla de la dude si quiero

