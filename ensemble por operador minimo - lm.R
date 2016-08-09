##### ENSEMBLE OPERADOR MINIMO CON LOS MEJORES MODELOS

combinacion.modelos.minimo <- function (x = tabla.AUC.ordenadas, cant.modelos = 10) { #funcion donde x es el resultado de la funcion del script de curvas ROC.R y cant.modelos es la cant de modelos que quiero combinar
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("OptimalCutpoints") == FALSE) {install.packages("OptimalCutpoints")} #si OptimalCutpoints no está instalado hago que me lo instale automaticamente
        
        if( is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(OptimalCutpoints) #cargo el paquete OptimalCutpoints que tiene las funciones para esta funcion
        
        library(data.table) #cargo el paquete data.table que tiene las funciones para esta funcion
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores 20 modelos
        
        lista.predicciones.mejores.modelos <- lapply( lista.mejores.modelos , predict, type = "response")#armo una lista donde guardo las predicciones de cada uno de los mejroes 20 modelos
        
        tabla.valores.prediccion <- data.frame(matrix(unlist(lista.predicciones.mejores.modelos), nrow= length(lista.predicciones.mejores.modelos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        minimo<-apply(tabla.valores.prediccion,1,min)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto
        
        p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
                
        clase <-p[,"clase"]  ## extraigo la columna clase
                
        q <- factor(clase) ##hago que la columna clase se exprese como factor
        
        ROC.ensemble.minimo <- roc(predictor = minimo, response = q)#creo lista donde voy a guardar las curvas ROC
       
        AUC.ROC.ensemble.minimo <- summary(auc(ROC.ensemble.minimo))[["Median"]] ## valor del AUC de la curva ROC
        
        df <- data.frame(cbind(clase,minimo)) ## creo un data frame donde tengo la clase y el valor del operador minimo para cada compuesto
        
       punto.corte <- OptimalCutpoints::optimal.cutpoints (X = "minimo",status="clase",tag.healthy=0, data=df,methods="MaxSp") ## optimizo el punto de corte usando la funcion de maxima especificidad
        
       punto.corte2 <- data.frame(OptimalCutpoints::summary.optimal.cutpoints(punto.corte)$p.table$Global$MaxSp)["cutoff",] ## extraigo el valor de punto de corte
       
       predicciones <- ifelse( minimo > punto.corte2, yes = 1,no = 0) ## veo cual es la prediccion usando el punto de corte optimizado
        
       tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real")) ## hago la tabla para ver las clasificaciones      
       
       bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
       
       porcentaje.bien.clasificados <- 100*sum(bien.clasificados)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
       
       resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.minimo,"punto de corte", punto.corte2, "% bien clasificados training set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
       
       resultado.final ## pongo el resultado final
       
       }

############# ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

resultados.ensemble.minimo <- combinacion.modelos.minimo(x = tabla.AUC.ordenadas, cant.modelos = 10)

resultados.ensemble.minimo <- combinacion.modelos.minimo(x = tabla.sensibilidad.ordenadas, cant.modelos = 10)