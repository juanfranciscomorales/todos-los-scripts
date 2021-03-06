forward.stepwise.testLRT.glm.vif <- function(tabla.conjunto,punto.corte.Chi=0.01,steps=6, punto.corte.vif = 2) { # funcion para realizar los modelos por glm
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est� instalado o no
        
        if ( is.installed("car") == FALSE) {install.packages("car")} #si car no est� instalado hago que me lo instale automaticamente
        
        library(car) #cargo el paquete car
        
        df<- as.data.frame(tabla.conjunto)#hago por las dudas que la tabla del conjunto me la lleve a clase data frame
        
        null<-glm(clase ~1,data = df,family = binomial)#modelo inicial para hacer el forward
        
        full<-glm(clase~.,data = df,family = binomial)#este es el modelo con todos los descriptores, lo hago para poder armar el scope de la funcion add1
        
        repeat {      ## repeat es un loop donde todo se repite de manera infinita hasta que se cumpla alguna de las condiciones if para darle finalizacion
                
                paso<-add1(null,scope=formula(full),test="LRT")#hago que pruebe como da cuando agrego cada una de las variables individualmente
                
                menor.probabilidad<-which.min(paso$`Pr(>Chi)`)#elijo cual es el que posee menor p-valor
                
                elegido <- noquote(row.names(paso)[menor.probabilidad])#el descriptor elegido
                
                formulacion <- paste(".~. +", elegido) # esta ser�a la nueva formula para comenzar el modelo
                
                if ( paso$`Pr(>Chi)`[menor.probabilidad] >= punto.corte.Chi ) { #me fijo si el p-valor es menor a mi punto de corte, si lo es se repite el loop, sino termina aca
                        
                        break
                }
                
                null<-update(null, formula. = formulacion)#agrego el descriptor seleccionado por tener menor p-valor
                
                if (length(null$coefficients) > 2){ # le digo que si tengo mas de 2 coeficientes, que significa que tiene mas de dos variables haga el calculo del vif
                        
                        test.vif<- data.frame(vif(null)) # calculo el vif de cada termino. quiero que sea menor a punto.corte.vif = 2, es bastante exigente esto, porque con menor a 5 esta bien.
                        
                        if (any( test.vif[,1]>= punto.corte.vif, na.rm = TRUE)) { #si algun termino no cumple con el valor de vif lo elimino
                                
                                formulacion2<- paste(".~. -",paste(noquote(rownames(test.vif)[which(test.vif[,1]>= punto.corte.vif)]) ,collapse = " - ")) # selecciono los valores que no cumplen con mi p-valor
                                
                                null<-update(null, formula. = formulacion2)#elimino los terminos que en el paso anterior seleccione por no cumplir con el vif luego del agregado del nuevo termino
                        }
                }
                
                test <- anova(null, test = "LRT") #hago un anova al modelo con el nuevo descriptor para obtener los p-valores de todos los terminos, asi elimino los que ahora no cumplen con el p-valor ahora que agregu� un termino nuevo
                
                if (any(test$`Pr(>Chi)`>= punto.corte.Chi, na.rm = TRUE)) { #si algun p-valor de algun termino del modelo es mayor al punto de corte lo elimino
                        
                        formulacion2<- paste(".~. -",paste(noquote(rownames(test)[which(test$`Pr(>Chi)`>= punto.corte.Chi)]) ,collapse = " - ")) # selecciono los valores que no cumplen con mi p-valor
                        
                        null<-update(null, formula. = formulacion2)#elimino los terminos que en el paso anterior seleccione por no cumplir con el p-valor luego del agregado del nuevo termino
                }
                
                if (length(null$coefficients) >= steps + 1){ # le digo que corte cuando cumplo con el numero de steps m�ximo, sumo 1 porque los coeficientes son los descriptores + ordenada al origen
                        break
                }
        }
        result <-null
        result   # es el modelo que me da como resultado
}
###### HASTA ACA ES LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

lista.modelos<- lapply(lista.conjuntos2, forward.stepwise.testLRT.glm.vif, punto.corte.Chi=0.01,steps=6, punto.corte.vif=2)# aplico a cada conjunto la funci�n llamada forward.stepwise.testF.glm para obtener los modelos. si quiero modifico punto de corte y numero de steps

