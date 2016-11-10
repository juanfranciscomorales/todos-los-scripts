

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.train[,2], labels = training$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff")) ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte
