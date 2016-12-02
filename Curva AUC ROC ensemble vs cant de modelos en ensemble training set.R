###### ENSEMBLE PROMEDIO ########

vector.AUC.ensembles <- vector()

for( i in 1:30){
        
        vector.AUC.ensembles[i] <-combinacion.modelos.promedio(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:30

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Promedio - Training set" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


####### ENSEMBLE MINIMO #########

vector.AUC.ensembles <- vector()

for( i in 1:30){
        
        vector.AUC.ensembles[i] <-combinacion.modelos.minimo(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:30

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Minimo - Training set" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))


######### ENSEMBLE VOTO ##########

vector.AUC.ensembles <- vector()

for( i in 1:30){
        
        vector.AUC.ensembles[i] <-combinacion.modelos.voto(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:30

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
        layout(title = "Ensemble Voto - Training set" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))

##### ENSEMBLE RANKING #######

vector.AUC.ensembles <- vector()

for( i in 1:30){
        
        vector.AUC.ensembles[i] <-combinacion.modelos.ranking(cant.modelos = i, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)[[2]]
}  

cant.modelos <- 1:30

library(plotly)

plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers")%>%
layout(title = "Ensemble Ranking - Training set" , xaxis = list(title="cantidad de modelos en Ensemble"), yaxis=list(title="AUC curva ROC del Ensemble"))



