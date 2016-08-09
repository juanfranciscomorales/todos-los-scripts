library(OptimalCutpoints)

optimal.cutpoints( X = ##las curvas ROC## , methods="MaxSp",) #es una funcion que me calcula el punto de corte optimo donde la especificidad es máx
                           
                           
optimal.cutpoints( X = ##las curvas ROC## , methods="MinValueSpSe",maxSp=TRUE, valueSp=0.9 , valueSe=0.9) # opcion 2, punto de corte optimo donde pongo que mi sensibilidad y especificidad no pueden ser menores a 0.9

                         
                           

optimizacion.punto.corte.ROC.max.especificidad <- function (modelos = lista.modelos,minSp=0.9,minSe=0.9) {
lista.predicciones <- lapply( modelos , predict) #armo lista donde cada objeto es un modelo y tiene los valores predichos para cada compuesto
clase <-lista.conjuntos2[[1]][,"clase" ] #extraigo los valores de la columna clase
predicciones<- data.frame(matrix(unlist(lista.predicciones), nrow= length(clase), byrow=FALSE)) #armo un data frame donde están todos los valores de las predicciones
names(predicciones)<-1:length(modelos) #nombro las columnas con numeros para poder hacer loop
df<-cbind(clase,predicciones) #armo un data frame donde estan los valores de las clases y los valores predichos
lista.punto.corte<-list()
resumen.punto.corte<-list()
for( i in 1:length(modelos)) {
lista.punto.corte[[i]] <-OptimalCutpoints::optimal.cutpoints (X =paste(i),status="clase",tag.healthy=0, data=df,methods="MinValueSpSe",maxSp=TRUE, valueSp= minSp , valueSe= minSe) #punto de corte optimo donde pongo que mi sensibilidad y especificidad no pueden ser menores a 0.9, y que la specificidad sea maxima
resumen.punto.corte[[i]]<-OptimalCutpoints::summary.optimal.cutpoints(lista.punto.corte[[i]]) # resumen, donde pone el valor del punto de corte y los valores de sensibilidad y espeficidad para ese punto de corte
resumen.punto.corte#resultado del loop
}
list.df<-list()
for (i in 1:length(resumen.punto.corte)) {
list.df[[i]]<-as.data.frame(resumen.punto.corte[[i]]$p.table$Global$MinValueSpSe)
list.df
}
list.df.resultados<- lapply(lapply(list.df,t),data.frame)
df.resultados<-data.frame()
for(i in seq(along=list.df.resultados)){ 
        for(j in names(list.df.resultados[[i]])){
        df.resultados[i,j] <- list.df.resultados[[i]][j]
        df.resultados}
        }
df.resultados[,c("cutoff","Se","Sp")]
}

optimizacion.punto.corte.ROC.max.especificidad(modelos = lista.modelos,minSp=0.9,minSe=0.9)

