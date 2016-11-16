
library(data.table)

training <- as.data.frame(fread(input = "Dtrainingmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training <- training[,-c(1,2)] # elimino las columnas clase y nombre, dejo solo las columnas con los descriptores

training <- training[, colSums(training != 0) > 0] ## para eliminar las columnas que solo tienen zeros

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

training.dataframe <- as.data.frame(scale(training)) ## escalo mis datos

training.matrix <- scale(training) ## escalo mis datos

test <- as.data.frame(fread(input = "Dtestmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del test set


########### "DiscriMiner" ##########################3#


install.packages("DiscriMiner")

library(DiscriMiner)

PLS.DA <- plsDA(variables = training.dataframe, group = clase, autosel = TRUE, cv="LOO", retain.models = F) ### me elige el mejor PLS-DA optimizando por Leave One Out

summary(PLS.DA)

print(PLS.DA)

plot(PLS.DA)

PLS.DA$confusion

##############  "mixOmics"  #############################

install.packages("mixOmics")

library(mixOmics)

PLS.DA <- plsda(X=training.matrix , Y = clase , scale = FALSE, ncomp = 2) ## hago PLS-DA, pero no me elije el mejor por Leave One Out, tengo que decirle la cantidad de componentes a incluir

auroc(PLS.DA) ## me muestra la curva ROC del modelo en los datos

plotVar(PLS.DA) ## me grafica la importancia de las variables, o algo asi

plotIndiv(PLS.DA) ## me grafica las muestras en el espacio de los 2 componentes que mejor variabilidad tienen


#################### mdatools ##########################

install.packages("mdatools")

library(mdatools)

PLS.DA <- plsda(x = training.matrix , c = clase , center = FALSE , cv = 1, ncomp = 50) ### hago PLS-DA, y que me elija el mejor segun LOO

summary(PLS.DA)

print(PLS.DA)

par(mar=c(0.5,0.5,0.5,0.5)) ## amplio los margenes para que pueda funcionar la funcion plot, sino no dan los margenes

plot(PLS.DA) ## me muestra 4 graficos de como actua el modelo
