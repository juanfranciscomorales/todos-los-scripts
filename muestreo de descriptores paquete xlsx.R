library(xlsx) # cargo el paquete que tiene la funcion read.xlsx

set.seed(125) # uso esta funcion para que el sampleo sea reproducible, y no varie cada vez q lo hago

df <- read.xlsx(file="Dtraining.xlsx", sheetName = "Dtraining") #leo el archivo con mis descriptores

n <- 1000 # la veces que quiero que se repita el muestreo

j <- c(1:ncol(df))#armo un vector de 1 a la cantidad de columnas que haya(num de descriptores)

k <- j[-c(1,2,3)]#elimino los primeros 3 valores de mi vector, xq no quiero samplearlos

dflist <-list() #creo una lista vacia donde guardar los data frames

for(i in 1:n) { 
  dflist[[i]] <- df[ , c(sample(k,200)) ]#hago que repita el sampleo de mi dataframe 1000 veces, tomando 200 descriptores por vez, no tiene columna de actividad.dflist es una lista con 1000 dataframes de 200 descriptores cada uno
}


