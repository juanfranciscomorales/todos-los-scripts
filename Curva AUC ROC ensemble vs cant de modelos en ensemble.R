###### ENSEMBLE PROMEDIO ########

vector.AUC.ensembles <- vector()

for( i in 1:100){
        
        vector.AUC.ensembles[i] <-clasificaciones.test.set.ensemble.promedio.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:100

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Promedio" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


####### ENSEMBLE MINIMO #########

vector.AUC.ensembles <- vector()

for( i in 1:100){
        
        vector.AUC.ensembles[i] <-clasificaciones.test.set.ensemble.minimo.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:100

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Minimo" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


######### ENSEMBLE VOTO ##########

vector.AUC.ensembles <- vector()

for( i in 1:100){
        
        vector.AUC.ensembles[i] <-clasificaciones.test.set.ensemble.voto.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:100

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Voto" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))

##### ENSEMBLE RANKING #######

vector.AUC.ensembles <- vector()

for( i in 1:100){
        
        vector.AUC.ensembles[i] <-clasificaciones.test.set.ensemble.ranking.lm(test.set = "test set.csv",cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:100

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
layout(title = "Ensemble Ranking" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))



