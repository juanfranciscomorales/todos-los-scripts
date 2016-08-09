optimizacion.punto.corte.ROC.test.set.MinValueSpSe.glm <- function (test.set = "BASE SIMULADA - Poliaminas.xlsx" , modelos = lista.modelos, minSp=0.9, minSe=0.9) { #funcion que busca el punto de corte optimo donde la especificidad es maxima y se cumplen con los criterios de la minima especificidad(minSp) y minima sensibilidad(minSe) que yo seteo
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("OptimalCutpoints") == FALSE) {install.packages("OptimalCutpoints")} #si OptimalCutpoints no está instalado hago que me lo instale automaticamente
        
        if( is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(OptimalCutpoints) #cargo el paquete OptimalCutpoints que tiene las funciones para esta funcion
        
        library(data.table) #cargo el paquete data.table que tiene las funciones para esta funcion
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        lista.predicciones.test <- lapply(X = modelos , FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        clase <- df.test.set[,"clase"] #extraigo los valores de la columna clase
        
        predicciones<- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(clase), byrow=FALSE)) #armo un data frame donde están todos los valores de las predicciones
        
        names(predicciones)<-1:length(modelos) #nombro las columnas con numeros para poder hacer loop
        
        df<-cbind(clase,predicciones) #armo un data frame donde estan los valores de las clases y los valores predichos
        
        lista.punto.corte<-list() #armo una lista vacia
        
        resumen.punto.corte<-list()#armo otra lista vacia
        
        controles <- control.cutpoints(maxSp=TRUE, valueSp= minSp , valueSe= minSe) ## funcion para controlar las variables de la funcion optimal.cutpoints
        
        for( i in 1:length(modelos)) { #loop donde calculos los punto de corte optimos
                lista.punto.corte[[i]] <-OptimalCutpoints::optimal.cutpoints (X =paste(i),status="clase",tag.healthy=0, data=df,methods="MinValueSpSe", control = controles) #punto de corte optimo donde pongo que mi sensibilidad y especificidad no pueden ser menores a los valores que especifico en la funcion, y que la especificidad sea maxima
                resumen.punto.corte[[i]]<-OptimalCutpoints::summary.optimal.cutpoints(lista.punto.corte[[i]]) # resumen, donde pone el valor del punto de corte y los valores de sensibilidad y espeficidad para ese punto de corte
                resumen.punto.corte#resultado del loop
        }
        
        list.df<-list() #creo lista vacia
        
        for (i in 1:length(resumen.punto.corte)) { #loop para extraer los datos que me interesan del resultado del anterior loop
                list.df[[i]]<-as.data.frame(resumen.punto.corte[[i]]$p.table$Global$MinValueSpSe) #extraigo los datos que me interesan para cada modelo
                list.df #resultado del loop
        }
        
        list.df.resultados<-lapply(list.df,t)#transpongo los data frames para luego poder armar un data frame solo con toda la info
        
        list.df.resultados2<- lapply(list.df.resultados,matrix,nrow=1, ncol= 9) #hago esto para que se me completen con NA en los casos  que no se pudo encontrar punto de corte
        
        list.df.resultados3<- lapply(list.df.resultados2,data.frame) #vuelvo data frame otra vez porque con la funcion anterior se me volvieron matrix
        
        nombre.columnas<-c("cutoff","Se", "Sp", "PPV", "NPV", "DLR.Positive", "DLR.Negative", "FP", "FN")#creo elemento con el nombre de las columnas
        
        nombre.filas<-c("Estimate") #creo elemento con el nombre de las filas
        
        for (i in seq_along(list.df.resultados3)){
                colnames(list.df.resultados3[[i]]) <- nombre.columnas
                rownames(list.df.resultados3[[i]])<- nombre.filas
        }
        
        df.resultados<-as.data.frame(rbindlist(list.df.resultados3,fill=TRUE)) #armo un data frame unico con todos los datos que necesito
        
        tabla.final <-df.resultados[,c("cutoff","Se","Sp")] #extraigo las columnas del data frame creado que contiene la info que me interesa
        tabla.final
}
######## ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

puntos.corte.ROC.glm <-optimizacion.punto.corte.ROC.test.set.MinValueSpSe.glm(test.set = "BASE SIMULADA - Poliaminas.xlsx", modelos = lista.modelos,minSp=0.9,minSe=0.9 )

