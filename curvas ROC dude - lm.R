

AUC.curvas.ROC.dude.lm <- function (modelos = lista.modelos, dude = lista.dude) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(plyr) # cargo el paquete que tiene la funcion read.xlsx
        
        library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion
        

lista.scores.dude <- list()

for (i in 1:length(dude)) {
        
lista.scores.dude[[i]] <- lapply (X = modelos, FUN = predict , newdata = read.xlsx(xlsxFile = dude[i], check.names = TRUE))

}

lista.clase <- list()

for (i in 1:length(dude)) {
        
        lista.clase[[i]] <- read.xlsx(xlsxFile = dude[i], check.names = TRUE)$clase
        
}

df.clase <- as.data.frame(matrix(unlist(lista.clase), nrow =length(unlist(lista.clase)), byrow = FALSE))

colnames(df.clase) <- c("clase")

df.clase$clase[df.clase$clase == -1] <- 0

lista.tablas.scores.dudes <- list()

for ( i in 1:length(lista.scores.dude)) {
        
        lista.tablas.scores.dudes[[i]]  <-  as.data.frame(matrix(unlist(lista.scores.dude[[i]]), ncol=length(lista.scores.dude[[i]]), byrow = FALSE))
}


df.scores <- ldply(lista.tablas.scores.dudes, data.frame)


lista.curvas.roc.dude <- list()

for (i in 1:ncol(df.scores)) {
        
        lista.curvas.roc.dude[[i]] <- roc( predictor = df.scores[, i],response = df.clase$clase)
        
}

lista.auc.dude <- list()

for (i in 1:length(lista.curvas.roc.dude)) {
        
        lista.auc.dude[[i]] <- auc(lista.curvas.roc.dude[[i]])
}

lista.auc.roc.numeric<-list()  # loop para extraer los valores de AUC como numeros

for (i in 1:length(lista.auc.dude)){
        
        lista.auc.roc.numeric[[i]]<- summary(lista.auc.dude[[i]])[["Median"]]
        
}

models.ranking.by.auc.roc<-order(unlist(lista.auc.roc.numeric), decreasing = TRUE) # ordenar de forma decreciente los valores de AUC de todos los modelos

matrix.ranking <- cbind(modelo = models.ranking.by.auc.roc, AUC = lista.auc.roc.numeric[models.ranking.by.auc.roc]) #tabla donde obtengo el ranking de modelos segun AUC de la curva ROC, es una matrix

tabla.ranking <-as.data.frame(matrix.ranking)#hago que la matrix se vuelva data frame

tabla.ranking<-data.frame(apply(X=tabla.ranking, MARGIN = 2, FUN = unlist))# hago de manera que mis columnas que son listas se vuelvan numeros solamente, para poder luego guardarlos con la funcion write.xlsx

tabla.ranking #para que me tire el data frame ordenado

}


###### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

lista.dude <- c("Test + Dudes 1.xlsx","Test + Dudes 2.xlsx","Test + Dudes 3.xlsx","Test + Dudes 4.xlsx","Test + Dudes 5.xlsx","Test + Dudes 6.xlsx","Test + Dudes 7.xlsx","Test + Dudes 8.xlsx","Test + Dudes 9.xlsx","Test + Dudes 10.xlsx","Test + Dudes 11.xlsx","Test + Dudes 12.xlsx","Test + Dudes 13.xlsx","Test + Dudes 14.xlsx","Test + Dudes 15.xlsx","Test + Dudes 16.xlsx", "Test + Dudes 17.xlsx")## lista con los nombres de la base dude miristoil


tabla.AUC.ordenadas.dude <- AUC.curvas.ROC.dude.lm(modelos = lista.modelos, dude = lista.dude)




