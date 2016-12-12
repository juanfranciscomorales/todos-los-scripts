###### ENSEMBLE PROMEDIO ########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.promedio.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]

        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        }  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%
        
        layout(title = "Ensemble Promedio" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


####### ENSEMBLE MINIMO #########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.minimo.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
        layout(title = "Ensemble Minimo" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


######### ENSEMBLE VOTO ##########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.voto.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
        layout(title = "Ensemble Voto" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


##### ENSEMBLE RANKING #######

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.ranking.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
        layout(title = "Ensemble Ranking" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))

