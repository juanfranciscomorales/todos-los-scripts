##########################################


## # Deep Learning con H2O - PARA 2 CLASES ##


###########################################






is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("h2o") == FALSE) {install.packages("h2o")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table)

library(pROC)

library(caret)

library(h2o)

training.set  <- "S-M-H training set.csv"  ### nombre del archivo con el training set

test.set <- "S-M-H test set.csv"  ### nombre del archivo con el test set

training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-c(1,2,4)] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

#sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

#training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

training$clase <- as.factor(make.names(training$clase)) ## hago que la columna clase sea como factor y con nombres validos para poder hacer que el boosting sea clasificatorio

clase <- training$clase ## guardo los valores de clase para despues

training.log <- data.frame(training[, c("clase")], apply(training[2:ncol(training)],2, function(x) log10(x+1)) ) ## aplico la funcion log10 del valor +1 para evitar que haya valores de 1.

colnames(training.log)[1] <- "clase" ## renombro la primer columna

training.log <- training.log[ , apply(training.log, 2, function(x) !any(is.na(x)))] ## elimino las columnas con NA porque sino me va a tirar error despues


test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test <- test[, colnames(training)] ## extraigo en el test set 

test$clase <- as.factor(make.names(test$clase)) ## hago que la columna clase sea como factor y con nombres validos para poder hacer que el boosting sea clasificatorio

test.log <- data.frame(test[, c("clase")], apply(test[2:ncol(test)],2, function(x) log10(x+1)) ) ## aplico la funcion log10 del valor +1 para evitar que haya valores de 1.

colnames(test.log)[1] <- "clase"



h2o.init(nthreads=-1, max_mem_size="2G") ## con esto hago que el servidor sea mi propia compu y le seteo el maximo uso de memoria RAM en mi compu a 2GB

h2o.removeAll() ## clean slate - just in case the cluster was already running

training.h2o <- as.h2o(training.log) ## lo conviento en clase h2o para poder hacerlo correr luego

test.h2o <- as.h2o(test.log) ## lo conviento en clase h2o para poder hacerlo correr luego

variables.independientes <- colnames(training.h2o)[-1] ## armo un vector con el nombre de todas las variables independientes menos la variable dependiente llamada clase



####  HAGO CORRER EL DEEPLEARNING EN SI

# BASANDOME EN EL PAPER "DEEP NEURAL NETS AS A METHOD FOR QSAR" autor = Junshui Ma DOI: 10.1021/ci500747n

# DECIDO UTILIZAR LAS RECOMENDACIONES QUE PLANTEA ESTE PAPER EN COMO ARMAR EL MODELO

# USO LA BASE DE CUANTAS HIDDEN LAYERS HAY QUE TENER Y DE CUANTAS NEURONAS POR HIDDEN LAYER

# dropout ratios los elijo basandome en este paper tambien

# la activation function la elijo basandome en el paper que es ReLU ( rectified linear unit) con dropout, por eso elijo  "RectifierWithDropout"

# el dropout es una forma de armar el modelo y que se evite el overfitting.

# numero de iteraciones (epochs) recomienda que sea el valor mas alto el costo computacional tolere, porque no voy a tener overfitting por el dropout. 

# el resto de los parametros dice que hay e los dejan con los valores de default

# Otro paper llamado "Extreme gradient boosting as a method for quantitative structure-activity relationships"  autor= Robert Sheridan DOI:10.1021/acs.jcim.6b00591

# cita al paper anterior y recomienda el seteo de ciertos argumentos igual que el paper anterior

# excepto en el minibatch size = 125 y el epochs = 300.

# el minibatch size que plantea no puedo usarlo yo porque no puede superar ese valor al numero de datos que tengo. Como maximo puede ser igual al numero de datos que tengo. El si puede usarlo porque tiene datasets de miles de compuestos

# el epochs si lo puedo usar e incluso uno mayor por el numero bajo de compuestos que tengo

set.seed(1)

ptm <- proc.time()

deepmodel <- h2o.deeplearning( x = variables.independientes, ### los nombres de las variables independientes
                              
                              y = "clase" ,## el nombre de la variable a modelar
                              
                              training_frame = training.h2o , ## el nombre del dataframe con el training set
                              
                              missing_values_handling = "Skip", ## How to handle missing values. Yo le digo que los saque
                              
                              shuffle_training_data = TRUE , ##Enable shuffling of training data (recommended if training data is replicated and train_samples_per_iteration is close to #nodes x #rows, of if using balance_classes).
                              
                              standardize = FALSE , # If enabled, automatically standardize the data. If disabled, the user must provide properly scaled input data.
                              
                              model_id = "deep_model" , # Destination id for this model; auto-generated if not specified.
                              
                              activation = "RectifierWithDropout" , # Activation function. elegi "RectifierWithDropout" por ser la recomendada opr el paper
                              
                              mini_batch_size =  floor(nrow(training.log)*0.05) , # Mini-batch size (smaller leads to better fit, larger can speed up and generalize better).
                              
                              epochs = 300 , # How many times the dataset should be iterated. Puse un valor alto porque es lo que recomienda el paper
                              
                              seed = 1 , # Seed for random numbers
                              
                              stopping_rounds = 5 , # Early stopping based on convergence of stopping_metric. Stop if simple moving average of length k of the stopping_metric does not improve for k:=stopping_rounds scoring events. El numero epochs al cual evaluo el early stopping. Early stopping
                              
                              stopping_metric = "AUC", # Metric to use for early stopping. Yo puse qeu sea el AUC de la curva ROC. Early stopping
                              
                              stopping_tolerance = 0.001 , # Relative tolerance for metric-based stopping criterion (stop if relative improvement is not at least this much). Le digo que tiene que mejorar por lo menos un 0.1 % el AUC cada 5 rondas(epochs). Early stopping
                              
                              #nfolds = 5 , #Number of folds for N-fold cross-validation. Hago que sea 5 fold CV. Lo hago para encontrar el numero optimo de epochs. Early stopping
                              
                              reproducible = FALSE ,## Force reproducibility on small data (will be slow - only uses 1 thread). Con esto obligo a que el resultado sea reproducible
                              
                              hidden = c(4000 , 2000 , 1000 , 1000) ,  ## Hidden layer sizes. aca le digo la cantidad de capas ocultas y las neuronas por capa oculta. Decido en base al paper
                              
                              input_dropout_ratio = 0 , # Input layer dropout ratio. le digo que el dropout en la input layer sea 0. Me baso en el paper.
                              
                              hidden_dropout_ratios = c(0.25 ,0.25 , 0.25 , 0.1) , ## Hidden layer dropout ratios (can improve generalization), specify one value per hidden layer. le digo que el dropout en las hidden layers sea de 0.25 para las primeras 3 y de 0.1 para la ultima. me baso en el paper.
                              
                              adaptive_rate = FALSE , #  Adaptive learning rate. Tengo que poner TRUE or FALSE. Si quiero que el learning rate sea adaptivo. En caso de que no, corren los argumentos rate y momentums.
                              
                              rate = 0.005 , ## Learning rate (higher => less stable, lower => slower convergence). es el valor del learning rate.
                              
                              #momentum_start=0.5 , # Initial momentum at the beginning of training (try 0.5).
                              
                              #momentum_ramp=1e6 , #Number of training samples for which momentum increases. 
                              
                              momentum_stable = 0.99 , ## Final momentum after the ramp is over. supongo que es el momentum strength que dice en el paper. 
                              
                              #max_w2 = 10, 
                              
                              l2 = 0.0001, #L2 regularization (can add stability and improve generalization, causes many weights to be small. l2 puede ser el weight cost strength del paper
                              
                              l1 = 0.0001, #L1 regularization (can add stability and improve generalization, causes many weights to become 0). 
                              
                              rate_decay = 0.000001 , ##Learning rate decay factor between layers (N-th layer: rate * rate_decay ^ (n - 1). Defaults to 1. supongo que es weight cost strength del paper
                              
                              variable_importances = TRUE #  Compute variable importances for input features (Gedeon method) - can be slow for large networks.
                              
)

proc.time() - ptm

deepmodel

h2o.varimp_plot(model = deepmodel , num_of_features = 10)

predicciones.train <- as.data.frame(predict(deepmodel, newdata = training.h2o )) ## hago la prediccion en el training set obteniendo los resultados como probabilidad

names(predicciones.train) <- c("clase predicha","Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.training <- auc(roc(predictor = predicciones.train$Activo,response = clase, direction = "<", plot = TRUE, main ="ROC Training set")) ## calculo la curva ROC para el training set

predicciones.test <- as.data.frame(predict(deepmodel, newdata = test.h2o )) ## hago la prediccion en el training set obteniendo los resultados como probabilidad

names(predicciones.test) <- c("clase predicha","Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.test <- auc(roc(predictor= predicciones.test$Activo, response = test$clase, direction = "<", plot = TRUE, main ="ROC Test set")) ## calculo de curva ROC para el test set 


resultado.deep.learning <- list("Modelo armado por Deep Learning", deepmodel  , "AUC ROC Training" , auc.training, "AUC ROC Test", auc.test) ## armo una lista con todos los resultados que quiero que se impriman

resultado.deep.learning



######### GRAFICOS PARA ANALIZAR LOS RESULTADOS - TRAINING SET ############




predicciones.train <- as.data.frame(predict(deepmodel, newdata = training.h2o )) ## hago la prediccion en el training set obteniendo los resultados como probabilidad

names(predicciones.train) <- c("clase predicha","Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.train$Activo, labels = clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Training set") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff - Training set") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff - Training set") ## grafico de PPV versus punto de corte

plot(performance(predicciones , measure = "acc" , x.measure = "cutoff"), main ="Accuracy vs cutoff - Training set") ## grafico de accuracy versus punto de corte



library(ggplot2)

library(plotly)


performance.mat.cutoff <- performance(predicciones , measure = "mat" , x.measure = "cutoff")

df.mat.cutoff <- data.frame(performance.mat.cutoff@x.values , performance.mat.cutoff@y.values)

colnames(df.mat.cutoff) <- c( "cutoff" , "mat")



performance.ppv.cutoff <- performance(predicciones , measure = "ppv" , x.measure = "cutoff")

df.ppv.cutoff <- data.frame(performance.ppv.cutoff@x.values , performance.ppv.cutoff@y.values)

colnames(df.ppv.cutoff) <- c( "cutoff" , "ppv")



performance.acc.cutoff <- performance(predicciones , measure = "acc" , x.measure = "cutoff")

df.acc.cutoff <- data.frame(performance.acc.cutoff@x.values , performance.acc.cutoff@y.values)

colnames(df.acc.cutoff) <- c( "cutoff" , "acc")



performance.sens.cutoff <- performance(predicciones , measure = "sens" , x.measure = "cutoff")

df.sens.cutoff <- data.frame(performance.sens.cutoff@x.values , performance.sens.cutoff@y.values)

colnames(df.sens.cutoff) <- c( "cutoff" , "sens")



performance.spec.cutoff <- performance(predicciones , measure = "spec" , x.measure = "cutoff")

df.spec.cutoff <- data.frame(performance.spec.cutoff@x.values , performance.spec.cutoff@y.values)

colnames(df.spec.cutoff) <- c( "cutoff" , "spec")



performance.npv.cutoff <- performance(predicciones , measure = "npv" , x.measure = "cutoff")

df.npv.cutoff <- data.frame(performance.npv.cutoff@x.values , performance.npv.cutoff@y.values)

colnames(df.npv.cutoff) <- c( "cutoff" , "npv")



grafico <- ggplot () + 
        
        geom_line(data = df.mat.cutoff , aes(x = cutoff, y = mat , color = "mat" ) , size = 1) + 
        
        geom_line(data = df.ppv.cutoff , aes(x = cutoff, y = ppv , color = "ppv" ) , size = 1) + 
        
        geom_line(data = df.acc.cutoff , aes(x = cutoff, y = acc , color = "acc" ) , size = 1) +
        
        geom_line(data = df.sens.cutoff , aes(x = cutoff, y = sens , color = "sens" ) , size = 1) +
        
        geom_line(data = df.spec.cutoff , aes(x = cutoff, y = spec , color = "spec" ) , size = 1) +
        
        geom_line(data = df.npv.cutoff , aes(x = cutoff, y = npv , color = "npv" ) , size = 1) +
        
        scale_colour_manual(values = c("red", "blue", "green" , "violet" , "yellow" , "orange")) +
        
        ggtitle("Training set") +
        
        labs(colour = "Variable", y = NULL) 


ggplotly(grafico)










######### GRAFICOS PARA ANALIZAR LOS RESULTADOS - TEST SET ############






test <- "S-M test set.csv"  ### nombre del test set

test <- as.data.frame(fread(input = test, check.names = TRUE)) #leo el archivo con mis descriptores del test set

predicciones.test <- as.data.frame(predict(deepmodel, newdata = test.h2o )) ## hago la prediccion en el training set obteniendo los resultados como probabilidad

names(predicciones.test) <- c("clase predicha","Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

library(ROCR) ## abro el paquete ROCR

predicciones <- prediction(predictions = predicciones.test$Activo, labels = test$clase) ## genero los valores para armar los diferentes graficos

plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte

abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria

plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo

plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte

plot(performance(predicciones , measure = "acc" , x.measure = "cutoff"), main ="Accuracy vs cutoff") ## grafico de accuracy versus punto de corte



library(ggplot2)

library(plotly)


performance.mat.cutoff <- performance(predicciones , measure = "mat" , x.measure = "cutoff")

df.mat.cutoff <- data.frame(performance.mat.cutoff@x.values , performance.mat.cutoff@y.values)

colnames(df.mat.cutoff) <- c( "cutoff" , "mat")



performance.ppv.cutoff <- performance(predicciones , measure = "ppv" , x.measure = "cutoff")

df.ppv.cutoff <- data.frame(performance.ppv.cutoff@x.values , performance.ppv.cutoff@y.values)

colnames(df.ppv.cutoff) <- c( "cutoff" , "ppv")



performance.acc.cutoff <- performance(predicciones , measure = "acc" , x.measure = "cutoff")

df.acc.cutoff <- data.frame(performance.acc.cutoff@x.values , performance.acc.cutoff@y.values)

colnames(df.acc.cutoff) <- c( "cutoff" , "acc")



performance.sens.cutoff <- performance(predicciones , measure = "sens" , x.measure = "cutoff")

df.sens.cutoff <- data.frame(performance.sens.cutoff@x.values , performance.sens.cutoff@y.values)

colnames(df.sens.cutoff) <- c( "cutoff" , "sens")



performance.spec.cutoff <- performance(predicciones , measure = "spec" , x.measure = "cutoff")

df.spec.cutoff <- data.frame(performance.spec.cutoff@x.values , performance.spec.cutoff@y.values)

colnames(df.spec.cutoff) <- c( "cutoff" , "spec")



performance.npv.cutoff <- performance(predicciones , measure = "npv" , x.measure = "cutoff")

df.npv.cutoff <- data.frame(performance.npv.cutoff@x.values , performance.npv.cutoff@y.values)

colnames(df.npv.cutoff) <- c( "cutoff" , "npv")



grafico <- ggplot () + 
        
        geom_line(data = df.mat.cutoff , aes(x = cutoff, y = mat , color = "mat" ) , size = 1) + 
        
        geom_line(data = df.ppv.cutoff , aes(x = cutoff, y = ppv , color = "ppv" ) , size = 1) + 
        
        geom_line(data = df.acc.cutoff , aes(x = cutoff, y = acc , color = "acc" ) , size = 1) +
        
        geom_line(data = df.sens.cutoff , aes(x = cutoff, y = sens , color = "sens" ) , size = 1) +
        
        geom_line(data = df.spec.cutoff , aes(x = cutoff, y = spec , color = "spec" ) , size = 1) +
        
        geom_line(data = df.npv.cutoff , aes(x = cutoff, y = npv , color = "npv" ) , size = 1) +
        
        scale_colour_manual(values = c("red", "blue", "green" , "violet" , "yellow" , "orange")) +
        
        ggtitle("Test set") +
        
        labs(colour = "Variable", y = NULL) 


ggplotly(grafico)




##############################################################

##############################################################

## Para guardar el modelo y despues volver a cargarlo 
## cuando lo necesito asi no lo calculo otra vez.

#############################################################

#############################################################




saveRDS(deepmodel, "deepmodel.rds") ## guardo el modelo 


deepmodel <- readRDS("deepmodel.rds") ## vuelvo a cargar el modelo




 
#####           PARA GUARDAR Y VOLVER A CARGAR EL MODELO  ##############
                
#####           EN CASO DE QUE NO SEA REPRODUCIBLE       #################
                
#####           Forma alternativa                        #####





modelo_guardado <- h2o.saveModel(object =deepmodel , path = getwd() , force = TRUE)  ## con esto puedo guardar el modelo en el caso de que no sea reproducible

modelo_guardado_path <- "D:\\Dropbox\\DOCTORADO\\Kpuu\\ACTUALIZACION 2017\\descriptores moleculares\\DRAGON\\slice-microdialisis-homogenato 2\\deep_model" ### aca deberia poner el path del deep learning guardado

deepmodel <-  h2o.loadModel(path = modelo_guardado_path)  ### con esto vuelvo a cargar el modelo guardado. Es el nombre entero del archivo junto con el path




h2o.shutdown() ## cuando termino la sesion hacer esto. Pierdo todos los datos
