
 

        library(car)
     

        list.regresion.steps.1 <- list()  # creo la lista donde voy a guardar la info de cada regresion con steps=1

for (i in 1:length(lista.conjuntos2.con)) { #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
                null<- lm(formula=clase ~ 1, data = lista.conjuntos2.con[[i]]) #creo el modelo de inicio para poner en la funcition step
                full<- lm(formula= clase ~., data=lista.conjuntos2.con[[i]]) #creo el modelo final para poner en la funcion step
                list.regresion.steps.1[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 1, trace=FALSE)#le digo que haga forward stepwise con steps=1
                list.regresion.steps.1 # este es el resultado obtenido, una lista con todas las regresiones realizadas
}
        
        list.regresion.steps.2 <- list()  # creo la lista donde voy a guardar la info de cada regresion
        lista.provisoria.2 <-list()
        
        for (i in 1:length(list.regresion.steps.1)) {  
                lista.provisoria.2[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 2, trace=FALSE)#le digo que haga forward stepwise para encontrar los mejores descriptores            #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        if (all(vif(lista.provisoria.2[[i]])<2)) {
               list.regresion.steps.2[[i]]<- lista.provisoria.2[[i]]
        }else {
                list.regresion.steps.2[[i]]<-list.regresion.steps.1[[i]]
}
       list.regresion.steps.2 # este es el resultado obtenido, una lista con todas las regresiones realizadas
}

        list.regresion.steps.3<-list()
        lista.provisoria.3 <-list()
        
for (i in 1:length(list.regresion.steps.2)) {
        lista.provisoria.3[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 3, trace=FALSE)#le digo que haga forward stepwise para encontrar los mejores descriptores        
        if (all(vif(lista.provisoria.3[[i]])<2)) {
                list.regresion.steps.3[[i]]<- lista.provisoria.3[[i]] 
       
        
}else {
    list.regresion.steps.3[[i]]<-list.regresion.steps.2[[i]]
}
list.regresion.steps.3
        }     

        list.regresion.steps.4<-list()
        lista.provisoria.4 <-list()
        
for (i in 1:length(list.regresion.steps.3)) {
        
        lista.provisoria.4[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 4, trace=FALSE)#le digo que haga forward stepwise para encontrar los mejores descriptores
        if (all(vif(lista.provisoria.4[[i]])<2)) {
                
                list.regresion.steps.4[[i]]<- lista.provisoria.4[[i]]          
                
        }else {
                list.regresion.steps.4[[i]]<-list.regresion.steps.3[[i]]
        }
list.regresion.steps.4
}     

        list.regresion.steps.5<-list()
        lista.provisoria.5 <-list()
        
        for (i in 1:length(list.regresion.steps.4)) { 
                lista.provisoria.5[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 5, trace=FALSE)#le digo que haga forward stepwise para encontrar los mejores descriptores        
        if (all(vif(lista.provisoria.5[[i]])<2)) {
                
                list.regresion.steps.5[[i]]<- lista.provisoria.5[[i]] 
                
        }else {
                list.regresion.steps.5[[i]]<-list.regresion.steps.4[[i]]
        }
list.regresion.steps.5
}     

        list.regresion.steps.6<-list()
        lista.provisoria.6 <-list()
for (i in 1:length(list.regresion.steps.5)) {
        lista.provisoria.6[[i]] <- step(object = null,scope = list(lower=null,upper=full),direction="forward",steps = 6, trace=FALSE)#le digo que haga forward stepwise para encontrar los mejores descriptores        
        if (all(vif(lista.provisoria.6[[i]])<2)) {
                
                list.regresion.steps.6[[i]]<- lista.provisoria.6[[i]] 
                
        }else {
                list.regresion.steps.6[[i]]<-list.regresion.steps.5[[i]]
        }
list.regresion.steps.6
}   


        lista.predicciones <- lapply(list.regresion.steps.6, predict)#calculos los valores predecidos por el modelo para cada compuesto
        
        library(pROC)
        
        lista.roc<-list() #creo una lista vacia donde voy a poner los calculos de las curvas ROC
        
        for (i in 1:length(lista.predicciones)) { #loop para el calculo de las curvas ROC de todos los modelos
                p <-lista.conjuntos2.con[[1]]
                q<-factor(p[,"clase"])
                lista.roc[[i]]<- roc(predictor = lista.predicciones[[i]],response = q)
                lista.roc
        }
        
        lista.auc.roc<-list() #creo una lista vacia donde guardar los valorse de AUC de la curvas ROC
        
        for (i in 1:length(lista.roc)) { # loop para el calculo de AUC de las curvas ROC para todos los modelos
                lista.auc.roc[[i]]<- auc(lista.roc[[i]])
                lista.auc.roc
        }
        
        lista.auc.roc.numeric<-list()  # loop para extraer los valores de AUC como numeros
        for (i in 1:length(lista.auc.roc)){
                lista.auc.roc.numeric[[i]]<- summary(lista.auc.roc[[i]])[["Median"]]
                lista.auc.roc.numeric
        }
        
        models.ranking.by.auc.roc<-order(unlist(lista.auc.roc.numeric), decreasing = TRUE) # ordenar de forma decreciente los valores de AUC de todos los modelos
        
        tabla.ranking <- cbind(modelo = models.ranking.by.auc.roc,AUC = lista.auc.roc.numeric[models.ranking.by.auc.roc]) #tabla donde obtengo el ranking de modelos segun AUC de la curva ROC



