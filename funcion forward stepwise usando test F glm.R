forward.stepwise.testF.glm <- function(tabla.conjunto,punto.corte=0.05,steps=6) { ## funcion para realizar los modelos por glm
        
        df<- as.data.frame(tabla.conjunto)#hago por las dudas que la tabla del conjunto me la lleve a clase data frame
        null<-glm(clase ~1,data = df)#modelo inicial para hacer el forward
        full<-glm(clase~.,data = df)#este es el modelo con todos los descriptores, lo hago para poder armar el scope de la funcion add1
        
        repeat {      ## repeat es un loop donde todo se repite de manera infinita hasta que se cumpla alguna de las condiciones if para darle finalizacion
                paso<-add1(null,scope=formula(full),test="F")#hago que pruebe como da cuando agrego cada una de las variables individualmente
                menor.probabilidad<-which.min(paso$`Pr(>F)`)#elijo cual es el que posee menor p-valor
                elegido <- noquote(row.names(paso)[menor.probabilidad])#el descriptor elegido
                formulacion <- paste(".~. +", elegido) # esta sería la nueva formula para comenzar el modelo
                if ( paso$`Pr(>F)`[menor.probabilidad] >= punto.corte ) { #me fijo si el p-valor es menor a mi punto de corte, si lo es se repite el loop, sino termina aca
                        break
                }
                null<-update(null, formula. = formulacion)#agrego el descriptor seleccionado, a partir de este modelo tengo que agregar las otras variables y ver si el agregado es significativo
                
                if (length(null$coefficients) == steps + 1){ # le digo que corte cuando cumplo con el numero de steps máximo, sumo 1 porque los coeficientes son los descriptores + ordenada al origen
                        break
                }
        }
        result <-null
        result   # es el modelo que me da como resultado
}
###### HASTA ACA ES LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

lista.modelos<- lapply(lista.conjuntos2, forward.stepwise.testF.glm, punto.corte=0.05,steps=6)# aplico a cada conjunto la función llamada forward.stepwise.testF.glm para obtener los modelos. si quiero modifico punto de corte y numero de steps

