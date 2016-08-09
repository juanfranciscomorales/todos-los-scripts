forward.stepwise.testF.lm <- function(tabla.conjunto,punto.corte=0.00001,steps=6) {
        
        df<- as.data.frame(tabla.conjunto)#hago por las dudas que la tabla del conjunto me la lleve a clase data frame
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

lista.modelos<- lapply(lista.conjuntos2.con, forward.stepwise.testF, punto.corte=0.00001,steps=6)# aplico a cada conjunto la función para obtener los modelos llamada forward.stepwise.testF