AUC.curvas.ROC <- function ( x = lista.modelos ) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente
        
        library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion
        
        lista.predicciones <- lapply( x , predict)#calculos los valores predecidos por el modelo para cada compuesto
        
        lista.roc<-list() #creo una lista vacia donde voy a poner los calculos de las curvas ROC
        
        for (i in 1:length(lista.predicciones)) { #loop para el calculo de las curvas ROC de todos los modelos
                p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
                q<-factor(p[,"clase"]) # creo el factor que va a usar la funcion roc asi hace las curvas
                lista.roc[[i]]<- roc(predictor = lista.predicciones[[i]],response = q, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE ) #calculo la curva ROC para cada conjunto y la guardo en una lista
                lista.roc # lista con las curvas ROC
        }
        
        ###  PUEDO HACERLO POR EL MÉTODO DE BOOSTRAP O POR EL METODO 
        
        ###  DE DELONG tambien se puede calcular. Preguntarle a Alan
        
        lista.roc[[i]]$ci[[1]] ### extraigo el menor valor del intervalo de confianza
        
        lista.roc[[i]]$ci[[2]] ### extraigo el valor estimado de la AUC que es la mediana del intervalo de confianza
        
        lista.roc[[i]]$ci[[3]] ### extraigo el mayor valor del intervalo de confianza
       