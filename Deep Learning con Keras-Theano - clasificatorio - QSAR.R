

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


training.set  <- "S-M training set.csv"  ### nombre del archivo con el training set

test.set <- "S-M test set.csv" ### nombre del archivo con el test set


## LEO Y LIMPIO MI TRAINING SET


training <- as.data.frame(fread(input = training.set, check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

drops <- colnames(test)[colSums(is.na(test)) > 0]## me fijo que columnas en el test set contiene NaN para poder eliminarlas del training set

training <- training[ , !(names(training) %in% drops)] ## elimino las columnas que en el test set contiene NaN.

training <- training[,-c(1,2,4)] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

sin.varianza <-  nearZeroVar(x = training) ### con esto se cuales son las columnas que tienen sd = 0 ( o sea sin varianza) y las que tienen muy muy poca varianza

training <- training[, -sin.varianza] ## elimino las columnas que tiene varianza cercana a cero

#training$clase <- as.factor(make.names(training$clase)) ## hago que la columna clase sea como factor y con nombres validos para poder hacer que el boosting sea clasificatorio

training.log <- log10(training[,2:ncol(training)]+1)  ## calculo el log +1 de cada elemento del data frame con solo los descriptores. Es logaritmo base 10

training.log <- training.log[ , apply(training.log, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training.scale.1 <- training[,2:ncol(training)] ## dejo solos los descriptores para escalar

training.scale <- as.data.frame(scale(x= training.scale.1 , center = colMeans(training.scale.1), scale = apply(X = training.scale.1, MARGIN =  2, FUN = sd)))  ## escalo los descriptores para que tengan media 0 y desciacion estandar 1.

Y_train <- training$clase ## en Y esta la columna con las clase. Tiene que estar como integer para que lo lea keras           

dim(Y_train) <- c(length(Y_train),1) ## le doy la forma que se necesita. que es una columna con varias filas, sino era una sola fila  

Y_train <- to_categorical(Y_train, num_classes = 2) ## Para preparar estos datos para el entrenamiento, uno-caliente codifica los vectores en matrices binarias de la clase usando la función de Keras to_categorical ():




## LEO Y LIMPO MI TEST SET



test <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores del test set

test.log <- test[,colnames(training.log)] ## dejo solo las columnas que tambien están en el dataframe training.log

test.log <- log10(test.log + 1)  ## calculo el log +1 de cada elemento del data frame con solo los descriptores. Es logaritmo base 10

test.scale <- test[,colnames(training.scale.1)]## dejo solo las columnas que tambien estan en el dataframe training.scale

test.scale <- as.data.frame(scale(x= test.scale , center = colMeans(training.scale.1), scale = apply(X = training.scale.1, MARGIN =  2, FUN = sd)))  ## escalo los descriptores, utilizando la media y desviacion estandar del training set.

Y_test <- test$clase    ## en Y esta la columna con las clase. Tiene que estar como integer para que lo lea keras             

dim(Y_test) <- c(length(Y_test),1) ## le doy la forma que se necesita. que es una columna con varias filas, sino era una sola fila  

Y_test <- to_categorical(Y_test, num_classes = 2) ## Para preparar estos datos para el entrenamiento, uno-caliente codifica los vectores en matrices binarias de la clase usando la función de Keras to_categorical ():





## HAGO CORRER EL DEEP LEARNING EN SI                                        


entrenamiento <- training.scale

prueba <- test.scale

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
        layer_dense(units = 2, activation = 'softmax', kernel_initializer = "he_uniform") ## esta es la capa final donde tira el output.


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


