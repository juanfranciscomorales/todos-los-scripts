conjuntos.descriptores <- function (archivo="Dtraining.xlsx", conjuntos=1000,descrip.conjunto=200, semilla=125) { # funcion para armar conjuntos aleatorios de descriptores. conjuntos= Num de conjuntos a armar. descrip.conjunto= cant de descriptores por conjunto. archivo = archivo xlsx donde estan los descriptores. semilla = sirve para poder reproducir la misma aleatorizacion
  
  is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
 
  if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
   
  library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
  
  set.seed(semilla) # uso esta funcion para que el sampleo sea reproducible, y no varie cada vez q lo hago
  
  df <- read.xlsx(xlsxFile=archivo, check.names = TRUE) #leo el archivo con mis descriptores
  
  n <- conjuntos # la veces que quiero que se repita el muestreo
  
  j <- c(1:ncol(df))#armo un vector de 1 a la cantidad de columnas que haya(num de descriptores)
  
  k <- j[-c(1,2,3)]#elimino los primeros 3 valores de mi vector, xq no quiero samplearlos
  
  dflist <-list() #creo una lista vacia donde guardar los data frames
  
  for(i in 1:n) { 
    dflist[[i]] <- df[ , c(2,sample(k,descrip.conjunto)) ]#hago que repita el sampleo de mi dataframe "conjuntos" de veces, tomando "descrip.conjunto" descriptores por vez,tiene columna de actividad. dflist es una lista con todos los data frames q son los conjuntos
  }
  dflist # dflist es una lista que contiene los data frames de todos lso conjuntos de descriptores
  } 

#### ACA TERMINA LA FUNCION. PRIMERO CARGO LA FUNCION Y LUEGO EJECUTO LO DE ABAJO

lista.conjuntos2 <- conjuntos.descriptores(archivo= , conjuntos= , descrip.conjunto = ,semilla = )#aplico la funcion para obtener mi lista de conjuntos de datos, yo decido los valores. lo guardo en una lista llamada lista.conjuntos


