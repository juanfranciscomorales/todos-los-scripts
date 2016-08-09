n <- colnames(lista.conjuntos2[[1]])
f <- as.formula(paste("clase ~", paste(n[!n %in% "clase"], collapse = " + ")))
nn<-neuralnet(formula = clase ~. , data= lista.conjuntos2[[1]],hidden = c(16,8,4,2), act.fct= "logistic", err.fct="ce", algorithm = "rprop+", linear.output = FALSE)
comp<-compute(nn, covariate = lista.conjuntos2[[1]][,-2]) ## son las predicciones, tengo que eliminar de mi tabla la columna clase
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('setosa', 'versicolor', 'virginica')[idx]
table(pred, irisvalid$Species)