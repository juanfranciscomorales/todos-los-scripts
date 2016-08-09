library(car)


list.regresion.steps.1 <- list()  # creo la lista donde voy a guardar la info de cada regresion con steps=1
null<-list()
full<-list()
for (i in 1:length(lista.conjuntos2.con)) { #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        null[[i]]<- lm(formula=clase ~ 1, data = lista.conjuntos2.con[[i]]) #creo el modelo de inicio para poner en la funcition step
        full[[i]] <- lm(formula= clase ~., data=lista.conjuntos2.con[[i]]) #creo el modelo final para poner en la funcion step
        list.regresion.steps.1[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 1, trace=FALSE)#le digo que haga forward stepwise con steps=1
        list.regresion.steps.1 # este es el resultado obtenido, una lista con todas las regresiones realizadas
}

list.regresion.steps.2 <- list()  # creo la lista donde voy a guardar la info de cada regresion

for (i in 1:length(lista.conjuntos2.con)) {   #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        list.regresion.steps.2[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 2, trace=FALSE)#le digo que haga forward stepwise con steps=2          
      
        list.regresion.steps.2 # este es el resultado obtenido, una lista con todas las regresiones realizadas
}

list.regresion.steps.3<-list()  # creo la lista donde voy a guardar la info de cada regresion


for (i in 1:length(lista.conjuntos2.con)) {  #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        list.regresion.steps.3[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 3, trace=FALSE)#le digo que haga forward stepwise con steps=3
       
        list.regresion.steps.3
}     

list.regresion.steps.4<-list()  # creo la lista donde voy a guardar la info de cada regresion


for (i in 1:length(lista.conjuntos2.con)) {  #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        
        list.regresion.steps.4[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 4, trace=FALSE)#le digo que haga forward stepwise con steps=4
        
        list.regresion.steps.4
}     

list.regresion.steps.5<-list()  # creo la lista donde voy a guardar la info de cada regresion


for (i in 1:length(lista.conjuntos2.con)) {  #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        list.regresion.steps.5[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 5, trace=FALSE)#le digo que haga forward stepwise con steps=5        
        
        list.regresion.steps.5
}     

list.regresion.steps.6<-list()  # creo la lista donde voy a guardar la info de cada regresion

for (i in 1:length(lista.conjuntos2.con)) {  #hago un for loop para hacer que sea iterativo a todos los conjuntos que estan dentro de la lista
        list.regresion.steps.6[[i]] <- step(object = null[[i]],scope = list(lower=null[[i]],upper=full[[i]]),direction="forward",steps = 6, trace=FALSE)#le digo que haga forward stepwise con steps=6        
        
        list.regresion.steps.6
}   

listado.total.regresiones<-list(list.regresion.steps.1,list.regresion.steps.2,list.regresion.steps.3,list.regresion.steps.4,list.regresion.steps.5, list.regresion.steps.6)#meto todas las listas de regresiones de los distintos steps en una sola. resultado tengo una lista con listas

listado.vif.todos<-list()
 for (i in 2:length(listado.total.regresiones)) {
        listado.vif.todos[[i]]<- lapply(listado.total.regresiones[[i]], vif) #le calculo el vif a cada modelo desarrollado de cada paso
 }
      
          listado.prueba.vif<-list()
        listado.prueba.vif2<-list()
for (i in 2:length(listado.vif.todos)) { #loop para ver cuales son los modelos con VIF <2
        listado.prueba.vif[[i]]<- lapply(listado.vif.todos[[i]], function (x) {x<2}) # veo si el vif es menor a 2
       
         listado.prueba.vif2[[i]]<-lapply(listado.prueba.vif[[i]],all) #como me calcula si el vif es <2  para cada descriptor. lo que hago es ver si todos los descriptores cumplen el requisito
        }

      tabla.resultados.vif<-as.data.frame(cbind(rep(TRUE,length(lista.conjuntos2.con)),as.logical(listado.prueba.vif2[[2]]),as.logical(listado.prueba.vif2[[3]]),as.logical(listado.prueba.vif2[[4]]),as.logical(listado.prueba.vif2[[5]]),as.logical(listado.prueba.vif2[[6]]))) #tabla con los valores de si cumple o no cada modelo con el VIF 

      x<-as.data.frame(which(!tabla.resultados.vif,arr.ind = TRUE)) # me tira una tabla diciendome en que col y row estan los que no cumplen con el VIF<2. row=nº de modelo;col=nºde paso
      
      y<- x[ x$row == ave(x$row, x$col, FUN=min), ] #me da una tabla diciendome a partir de cuales no se cumple el VIF, sino esta en la tabla es porque cumple hasta el 6to paso
      
      z<- data.frame(y$row -1) #quiero restarle -1 a solo la columna row, porque asi obtendria 
      
      lista.predicciones <- lapply(list.regresion.steps.6, predict)#calculos los valores predecidos por el modelo para cada compuesto
