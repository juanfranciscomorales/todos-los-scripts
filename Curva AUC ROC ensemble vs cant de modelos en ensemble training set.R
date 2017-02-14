###### ENSEMBLE PROMEDIO ########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <-combinacion.modelos.promedio(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
        }  


low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%
        
        layout(title = "Ensemble Promedio - Training" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


####### ENSEMBLE MINIMO #########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <-combinacion.modelos.minimo(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
}  


low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%
        
        layout(title = "Ensemble minimo - Training" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


######### ENSEMBLE VOTO ##########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <-combinacion.modelos.voto(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
}  


low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%
        
        layout(title = "Ensemble voto - Training" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


##### ENSEMBLE RANKING #######

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <-combinacion.modelos.ranking(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
}  


low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%
        
        layout(title = "Ensemble ranking - Training" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))




