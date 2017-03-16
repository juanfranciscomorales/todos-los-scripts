


setwd("D:/Dropbox/R/descriptores drugbank")## carpeta de trabajo





### fijarme de tener todos los archivos en la carpeta de trabajo, estos archivos son :

# training set

#base de datos

#tabla con los scores del screening ordenada, con los de mayor score arriba en la tabla
#Al archivo "tabla scores drugbank.xlsx" primero le elimino los compuestos sin scores y luego los ordeno de mayor a menor, esto lo hago antes de leer el archivo




regla.palanca <- function ( training.set ="trainingset.csv", base.datos = "drugbank.csv", tabla.scores  = "tabla scores drugbank.xlsx" , cant.modelos = 10, x = tabla.AUC.ordenadas.test.set,  cant.compuestos = 500 ) {

mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"]) ### me fijo cuales son los mejores modelos

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no est? instalado hago que me lo instale automaticamente

if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(openxlsx) ## cargo el paquete openxlsx

library(data.table) # cargo el paquete que tiene la funcion fread

lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]## armo una lista con los mejores modelos

lista.nombre.descriptores <- lapply (X = lista.mejores.modelos , FUN = variable.names) ## extraigo los nombres de los descriptores elegidos por los mejores modelos



lista.nombre.descriptores2 <- lapply( lista.nombre.descriptores, function (x) x[!x %in% c("(Intercept)") ]) ### elimino la variable intercept asi me quedan solo los nombres de los descriptores

lista.df <- list() ## creo una lista vacia donde voy a guardar los data frames de cada modelo que contiene los valrose de los descriptores

for (i in 1:length(lista.nombre.descriptores2)) {   ### hago un for loop para extraer las columnas de los descriptores de cada modelo
        
        df <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores
        
        lista.df[[i]] <- df[ , gsub("`", '', lista.nombre.descriptores2[[i]], fixed = TRUE) ] ## uso la funcion gsub() para poder eliminar las comillas simples que hacen que no pueda elegir las columnas sino
}

lista.matrix <- lapply(lista.df, as.matrix) ### hago que mi lista de data frames se vuelva una lista de matrices

multiplicado <- lapply( lista.matrix , function (M) t(M) %*% M) ## multiplico la matriz con los descriptores del modelo del traning por su transpuesta

inversa <- lapply(multiplicado , solve) ## me calcula la inversa de la matriz

########## hasta aca trabaje con el training, ahora voy a comenzar con la base de datos donde hago las predicciones, como drugbank o sweatlead

mejores.compuestos.base.datos <- read.xlsx(xlsxFile =tabla.scores , check.names = TRUE)$NOMBRE[1:cant.compuestos] ## extraigo los nombres de los mejores compuestos predichos como activos. Al archivo "tabla scores drugbank.xlsx" primero le elimino los compuestos sin scores y luego los ordeno de mayor a menor, esto lo hago antes de leer el archivo

df.base.datos <- as.data.frame(fread(input = base.datos, check.names = TRUE)) ## leo el archivo con la base de datos de los compuestos a predecir

df.descriptores.mejores.compuestos.base.datos <-  df.base.datos[df.base.datos$NAME %in% mejores.compuestos.base.datos ,  ] ## extraigo los compuestos que estan dentro de los 500 compuestos mejor clasificados
        
lista.df.base.datos <- list() ## creo una lista vacia donde voy a guardar los descriptores de cada modelo

for (i in 1:length(lista.nombre.descriptores2)) {  #### hago un for loop para extraer de la tabla con todos los descriptores de los mejores compuestos predichos en la base de datos, los descriptores que se corresponden a cada modelo
        
        lista.df.base.datos[[i]] <- df.descriptores.mejores.compuestos.base.datos[ , gsub("`", '', lista.nombre.descriptores2[[i]], fixed = TRUE) ]## uso la funcion gsub() para poder eliminar las comillas simples que hacen que no pueda elegir las columnas sino
}

lista.matrix.base.datos <- lapply(lista.df.base.datos, as.matrix) ### hago que mi lista de data frames se vuelva una lista de matrices

lista.matrix.base.datos.traspuesta <- lapply(lista.matrix.base.datos, t) ### obtengo las matrices traspuestas de las matrices de drugbank

multiplicado2 <- list() ## lista vacia

for (i in 1: length(lista.matrix.base.datos)) {   ### hago un for loop para hacer las multiplicaciones de matrices
        
        multiplicado2[[i]] <- lista.matrix.base.datos[[i]] %*% inversa[[i]]
        
}

multiplicado3 <- list()   ### lista vacia

for (i in 1: length(multiplicado2)) { ### hago la multiplicacion de las matrices
        
        multiplicado3[[i]] <- multiplicado2[[i]] %*% lista.matrix.base.datos.traspuesta[[i]]
        
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

row.names(tabla.resultados) <- df.descriptores.mejores.compuestos.base.datos$NAME

colnames(tabla.resultados) <- paste(rep("modelo" , length(mejores.segun.AUC)),paste(mejores.segun.AUC) )

write.xlsx(x= tabla.resultados, file= "tabla resultados leverage.xlsx" , colNames= TRUE, rowNames = TRUE,  keepNA=TRUE) # funcion para guardar la tabla de la dude si quiero

print("se guardo los resultados con nombre: tabla resultados leverage.xlsx")

}

####### ACA TERMINA LA FUNCION, LA CARGO Y HAGO CORRER LO DE ABAJO

regla.palanca ( training.set ="Training set sara.csv", base.datos = "Sweatlead-Drugbank 14-12-16.csv", tabla.scores  = "Screening por Ensemble Minimo.xlsx" , cant.modelos = 25, x = tabla.AUC.ordenadas.test.set,  cant.compuestos = 500 )
