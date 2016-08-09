conjunto.descriptores.sin <- function (archivo="Dtraining.xlsx", conjuntos=1000,descrip.conjunto=200, semilla=125){# funcion para armar conjuntos aleatorios de descriptores. conjuntos= Num de conjuntos a armar. descrip.conjunto= cant de descriptores por conjunto

  library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx

set.seed(semilla) # uso esta funcion para que el sampleo sea reproducible, y no varie cada vez q lo hago

df <- read.xlsx(xlsxFile=archivo) #leo el archivo con mis descriptores

n <- conjuntos # la veces que quiero que se repita el muestreo

j <- c(1:ncol(df))#armo un vector de 1 a la cantidad de columnas que haya(num de descriptores)

k <- j[-c(1,2,3)]#elimino los primeros 3 valores de mi vector, xq no quiero samplearlos

dflist <-list() #creo una lista vacia donde guardar los data frames

for(i in 1:n) { 
  dflist[[i]] <- df[ , c(sample(k,descrip.conjunto)) ]#hago que repita el sampleo de mi dataframe "conjunto" de veces, tomando "descrip.conjunto" descriptores por vez,no tiene columna de actividad. dflist es una lista con todos los data frames q son lso conjuntos
              }
dflist # dflist es una lista que contiene los data frames de todos lso conjuntos de descriptores
  }

lista.conjuntos.sin <- conjunto.descriptores.sin(archivo= , conjuntos= , descrip.conjunto = ,semilla = )#aplico la funcion para obtener mi lista de conjuntos de datos

lista.conjuntos2.sin <- lapply(lista.conjuntos.sin, data.frame)#a la lista le aplico la funcion data.frame para que me la pueda leer luego la funcino
