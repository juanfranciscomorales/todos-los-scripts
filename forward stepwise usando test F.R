forward.stepwise.testF <- function(archivo,punto.corte=0.001,steps=6) {
        library(openxlsx)#abro el paquete openxlsx para poder usar la funcion read.xlsx()
        datos<-read.xlsx(archivo)#leo el archivo de excel donde está el conjunto de descriptores
        df<-datos[,-c(1,3,4)]#saco las columnas que no sirven, dejo la columna clase y los descriptores
        nombre.columnas<-colnames(df)#extraigo los nombres de todas las columnas
        nombre.descriptores<-nombre.columnas[-1]#lista con los nombres de los descriptores
        null<-lm(clase ~1,data = df)#modelo inicial para hacer el forward
        full<-lm(clase~.,data = df)#este es el modelo con todos los descriptores, lo hago para poder armar el scope de la funcion add1
        
        repeat {
                paso<-add1(null,scope=formula(full),test="F")#hago que pruebe como da cuando agrego cada una de las variables individualmente
                menor.probabilidad<-which.min(paso$`Pr(>F)`)#elijo cual es el que posee menor p-valor
                elegido <- noquote(row.names(paso)[menor.probabilidad])#el descriptor elegido
                formulacion <- paste(".~. +", elegido)
                 if ( paso$`Pr(>F)`[menor.probabilidad] >= punto.corte ) { #me fijo si es menor a mi punto de corte, si lo es se repite el loop, sino termina aca
                        break
                }
                null<-update(null, formula. = formulacion)#agrego el descriptor seleccionado, a partir de este modelo tengo que agregar las otras variables y ver si el agregado es significativo
               
                if (length(null$coefficients) == steps + 1){ # le digo que corte cuando cumplo con el numero de pasos máx, sumo 1 porque los coeficientes son los descriptores + ordenada al origen
                        break
                }
        }
        result <-null
        result
}
