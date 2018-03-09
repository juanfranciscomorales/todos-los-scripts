

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("keras") == FALSE) {install.packages("keras")} #si keras no est? instalado hago que me lo instale automaticamente

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("caret") == FALSE) {install.packages("caret")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("pROC") == FALSE) {install.packages("pROC")} #si openxlsx no est? instalado hago que me lo instale automaticamente

Sys.setenv(KERAS_BACKEND = "theano") ## seteo para que use theano como backend

library(keras) ## cargo el paquete para usar el deep learning

library(data.table) ## cargo este paquete para leer rapido los archivos

library(caret) ## cargo el paquete caret que tiene varias funciones que voy a usar


# fix random seed for reproducibility

set.seed(1) ## seteo la semilla para hacer reproducibles los resultados


file  <- "dataset completo ADME.csv"  ### nombre del archivo con todos los datos



## LEO Y LIMPIO MI DATASET


dataset <- as.data.frame(fread(input = file, check.names = TRUE)) #leo el archivo con mis descriptores del training set

Y_dataset <- dataset[,2:8] ### este es el dataframe de las respuestas dependientes o Y´s

X_dataset <- dataset[,9:ncol(dataset)] ## este es el dataframe de las respuestas independientes o X´s

X_dataset <- X_dataset[ , apply(X_dataset, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

sin.varianza <-  nearZeroVar(x = X_dataset) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

X_dataset <- X_dataset[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

X_dataset_log <- log10(X_dataset + 1)  ## calculo el log +1 de cada elemento del data frame con solo los descriptores. Es logaritmo base 10

X_dataset_log <- X_dataset.log[ , apply(X_dataset.log, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

X_dataset_scale <- as.data.frame(scale(x= X_dataset , center = colMeans(X_dataset), scale = apply(X = X_dataset, MARGIN =  2, FUN = sd)))  ## escalo los descriptores para que tengan media 0 y desciacion estandar 1.




## reparto el dataset en training y test set

size_training = 0.8

smp_size <- floor(size_training * nrow(X_dataset.log)) ## con esto calcula cuantas filas son para el training

train_ind <- sample(seq_len(nrow(X_dataset.log)), size = smp_size) ## me da los indices de las filas que forman parte del training

X_train_log <- X_dataset_log[train_ind, ] 

X_test_log <- X_dataset_log[-train_ind, ]

Y_train <- Y_dataset[train_ind, ]  ### este es el dataframe de las respuestas dependientes o Y´s

Y_test <- Y_dataset[-train_ind, ]  ### este es el dataframe de las respuestas dependientes o Y´s


## reparto el training set  en training y validation set

size_training = 0.75

smp_size <- floor(size_training * nrow(X_dataset.log)) ## con esto calcula cuantas filas son para el training

train_ind <- sample(seq_len(nrow(X_train_log)), size = smp_size) ## me da los indices de las filas que forman parte del training

X_train_log <- X_train_log[train_ind, ]

X_val_log <- X_train_log[-train_ind, ]

Y_train <- Y_train[train_ind, ]  ### este es el dataframe de las respuestas dependientes o Y´s

Y_val <- Y_train[-train_ind, ]  ### este es el dataframe de las respuestas dependientes o Y´s



## HAGO CORRER EL DEEP LEARNING EN SI                                        


entrenamiento <- X_train_log

prueba <- X_test_log

validacion <- X_val_log

ncol <- ncol(entrenamiento)

model <- keras_model_sequential()  ## con esto le digo que voy a armar el modelo de forma secuencial. Agrego las capas de neuronas de forma secuencial

model %>%
        layer_dense(units = 4000, activation = 'relu', input_shape = ncol , kernel_initializer = "he_uniform" , kernel_constraint = constraint_maxnorm(max_value = 2)) %>% ## esta es la primer capa interna
        layer_dropout(rate = 0.5) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2000, activation = 'relu', kernel_initializer = "he_uniform", kernel_constraint = constraint_maxnorm(max_value = 2)) %>% ## esta es la segunda capa interna
        layer_dropout(rate = 0.5) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1000, activation = 'relu', kernel_initializer = "he_uniform", kernel_constraint = constraint_maxnorm(max_value = 2)) %>% ## esta es la tercer capa interna
        layer_dropout(rate = 0.5) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1000, activation = 'relu', kernel_initializer = "he_uniform", kernel_constraint = constraint_maxnorm(max_value = 2)) %>% ## esta es la tercer capa interna
        layer_dropout(rate = 0.2) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 7, activation = 'linear', kernel_initializer = "he_uniform") ## esta es la capa final donde tira el output.


summary(model)

#sgd <- optimizer_sgd(lr=0.005, decay=0.000001, momentum=0.99, nesterov=TRUE) ## seteo el optimizador sgd que es Stochastic Gradient Descent

adam <- optimizer_adam(lr = 0.0005 , beta_1=0.9, beta_2 = 0.999, epsilon=1e-08, decay = 0.0) ## seteo el optimizador Adam

#rmsprop <- optimizer_rmsprop()

model %>% compile(    ## compilo el modelo. Le digo cual es mi optimizador, cual es la función a minimizar y que me mida la precisión mientras optimiza
        loss = 'categorical_crossentropy',
        optimizer = adam ,
        metrics = c('accuracy')
)

# checkpoint

checkpoint <- callback_model_checkpoint(filepath = "weights.hdf5", monitor = 'loss' , verbose = 1 , save_best_only = TRUE , mode='auto' , period = 1) ## le digo que solo me guarde el mejor modelo obtenido durante la corrida. Se basa en la accuracy

#early_stopping <- callback_early_stopping(monitor = "acc" , patience = 10, verbose = 1) ## seteo el early stopping

callbacks_list <- list(checkpoint  
                       
                       # , early_stopping  ## armo la lista donde pongo el checkpoint y el early_stopping
)

history <- model %>% fit(  ## Hago el ajuste del modelo en si.  El batch size es del 5% de mi training set.
        x = as.matrix(entrenamiento),
        y = Y_train, 
        epochs = 100,
        callbacks = callbacks_list,
        #validation_split = 0.2 ,
        batch_size = nrow(entrenamiento) 
        
)

plot(history) ## The history object returned by fit() includes loss and accuracy metrics which we can plot:

load_model_weights_hdf5(object = model, filepath = 'weights.hdf5', by_name = FALSE) ## cargo el mejor modelo obtenido durante el ajuste. Lo hice guardar con checkpoint y ahora lo cargo


train_predict <- predict_proba(object=model , x =as.matrix(entrenamiento), batch_size= nrow(entrenamiento), verbose = 1) ## hago las predicciones para el training set

test_predict <- predict_proba(object=model , x =as.matrix(prueba), batch_size= nrow(entrenamiento), verbose = 1) ## hago las predicciones para el test set


library(pROC) ## abro el paquete pROC para hacer las curvas ROC

#names(train_predict) <- c("Inactivo","Activo") ## le cambio los nombres a las columnas de la tabla de predicciones del training, para que sea activo y inactivo

auc.training <- auc(roc(predictor = train_predict[,2],response = Y_train[,2], direction = "<", plot = TRUE, main ="ROC Training set" , print.auc =TRUE)) ## calculo la curva ROC para el training set

auc.test <- auc(roc(predictor=test_predict[,2], response = Y_test[,2], direction = "<", plot = TRUE, main ="ROC Test set", print.auc=TRUE)) ## calculo de curva ROC para el test set 


