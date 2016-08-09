AUC.curvas.ROC.test.set.glm <- function ( test.set= "TEST SET - Poliaminas.xlsx", modelos = lista.modelos ) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion
        
        df.test.set <- read.xlsx(xlsxFile = test.set, check.names = TRUE) #leo el archivo con el test set
        
        lista.predicciones.test <- lapply(X = modelos , FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        lista.roc<-list() #creo una lista vacia donde voy a poner los calculos de las curvas ROC
        
        for (i in 1:length(lista.predicciones.test)) { #loop para el calculo de las curvas ROC de todos los modelos
                q<-factor(df.test.set[,"clase"]) # creo el factor que va a usar la funcion roc asi hace las curvas
                lista.roc[[i]]<- roc(predictor = lista.predicciones.test[[i]],response = q) #calculo la curva ROC para cada conjunto y la guardo en una lista
                lista.roc # lista con las curvas ROC
        }
        
        lista.auc.roc<-list() #creo una lista vacia donde guardar los valorse de AUC de la curvas ROC
        
        for (i in 1:length(lista.roc)) { # loop para el calculo de AUC de las curvas ROC para todos los modelos
                lista.auc.roc[[i]]<- auc(lista.roc[[i]]) #calculo de la AUC para cada modelo y lo guardo en una lista
                lista.auc.roc #lista con las AUC de las curvas ROC
        }
        
        lista.auc.roc.numeric<-list()  # loop para extraer los valores de AUC como numeros
        for (i in 1:length(lista.auc.roc)){
                lista.auc.roc.numeric[[i]]<- summary(lista.auc.roc[[i]])[["Median"]]
                lista.auc.roc.numeric #lista con los valores absolutos de las AUC
        }
        
        models.ranking.by.auc.roc<-order(unlist(lista.auc.roc.numeric), decreasing = TRUE) # ordenar de forma decreciente los valores de AUC de todos los modelos
        
        matrix.ranking <- cbind(modelo = models.ranking.by.auc.roc, AUC = lista.auc.roc.numeric[models.ranking.by.auc.roc]) #tabla donde obtengo el ranking de modelos segun AUC de la curva ROC, es una matrix
        tabla.ranking <-as.data.frame(matrix.ranking)#hago que la matrix se vuelva data frame
        tabla.ranking<-data.frame(apply(X=tabla.ranking, MARGIN = 2, FUN = unlist))# hago de manera que mis columnas que son listas se vuelvan numeros solamente, para poder luego guardarlos con la funcion write.xlsx
        tabla.ranking #para que me tire el data frame ordenado
}
###### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

tabla.AUC.ordenadas.test.set.glm <- AUC.curvas.ROC.test.set.glm( test.set= "TEST SET - Poliaminas.xlsx", modelos = lista.modelos) # aplico mi funcion a la lista de modelos armada con la funcion anterior, como resultado obtengo una tabla con los valores de AUC ordenadas por mejores modelos

