#VOY A INTENTAR ARMAR FUNCION PARA ENSEMBLES, PARA VER SI MEJORA AUC DE CURVAS ROC
combinacion.modelos <- function (x = tabla.AUC.ordenadas, cant.modelos = 10) { #funcion donde x es el resultado de la funcion del script de curvas ROC.R y cant.modelos es la cant de modelos que quiero combinar

        mejores.segun.AUC <- as.numeric(tabla.AUC.ordenadas[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos

        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores 20 modelos

        lista.predicciones.mejores.modelos <- lapply( lista.mejores.modelos , predict, type = "response")#armo una lista donde guardo las predicciones de cada uno de los mejroes 20 modelos

        tabla.valores.prediccion <- data.frame(matrix(unlist(lista.predicciones.mejores.modelos), nrow= length(lista.predicciones.mejores.modelos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame

        minimo<-apply(tabla.valores.prediccion,1,min)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto

        maximo<-apply(tabla.valores.prediccion,1,max)#aplico operador maximo en los valores predichos de los mejores modelos para cada compuesto

        promedio<-apply(tabla.valores.prediccion,1,mean)#aplico operador promedio en los valores predichos de los mejores modelos para cada compuesto

        tabla.operadores.by.row<-cbind(minimo,maximo,promedio)#armo una tabla donde estan para cada compuesto el resultado de los operadores minimo, maximo y promedio

        lista.ROC.ensembles <- list()#creo lista donde voy a guardar las curvas ROC

for (i in 1:ncol(tabla.operadores.by.row)) { #loop para obtener las curvas ROC para cada operador
        
        p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
        
        q<-factor(p[,"clase"]) # creo el factor que va a usar la funcion roc asi hace las curvas
        
        lista.ROC.ensembles[[i]]<- roc(predictor= tabla.operadores.by.row[,i],response = q ) #aplico la funcion roc para calcular las curvas ROC
        
        lista.ROC.ensembles #lista con las curvas ROC
}
 
        names(lista.ROC.ensembles)<- c("minimo","maximo","promedio") #le pongo nombre a cada elemento de la lista, segun lo q le corresponde

        lista.AUC.ROC.ensembles<-lapply(lista.ROC.ensembles,auc)#calculo de las AUC para las curvas ROC antes armadas

        lista.AUC.ROC.ensembles
}

##############

combinacion.modelos(x = tabla.AUC.ordenadas, cant.modelos = 10)