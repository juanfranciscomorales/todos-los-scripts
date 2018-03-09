
########################
##    Install Keras   ##
########################

# First, install the keras R package from GitHub as follows:

install.packages("Rcpp")
install.packages("devtools")
devtools::install_github("rstudio/reticulate")
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")


# The Keras R interface uses the TensorFlow backend engine by default.
# To install both the core Keras library as well as the TensorFlow backend use the install_keras() function:

library(keras)

install_keras()

###############################################################
###############################################################


########################
## Preparing the Data ##
########################

## The MNIST dataset is included with Keras and can be accessed using the dataset_mnist() function. Here we load the dataset then create variables for our test and training data:

library(keras)

mnist <- dataset_mnist() 

x_train <- mnist$train$x

y_train <- mnist$train$y

x_test <- mnist$test$x

y_test <- mnist$test$y


# reshape
dim(x_train) <- c(nrow(x_train), 784)

dim(x_test) <- c(nrow(x_test), 784)

# rescale
x_train <- x_train / 255

x_test <- x_test / 255


# The y data is an integer vector with values ranging from 0 to 9. To prepare this data for training we one-hot encode the vectors into binary class matrices using the Keras to_categorical() function:
y_train <- to_categorical(y_train, 10)

y_test <- to_categorical(y_test, 10)

#########################
## Defining the Model ##
########################

# The core data structure of Keras is a model, a way to organize layers. The simplest type of model is the Sequential model, a linear stack of layers.

# We begin by creating a sequential model and then adding layers using the pipe (%>%) operator:

model <- keras_model_sequential()  ## con esto le digo que voy a armar el modelo de forma secuencial. Agrego las capas de neuronas de forma secuencial

model %>% 
        layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% ## esta es la primer capa interna
        layer_dropout(rate = 0.4) %>% 
        layer_dense(units = 128, activation = 'relu') %>% ## esta es la segunda capa interna
        layer_dropout(rate = 0.3) %>%
        layer_dense(units = 10, activation = 'softmax') ## esta es la capa final donde tira el output.

# The input_shape argument to the first layer specifies the shape of the input data (a length 784 numeric vector representing a grayscale image). The final layer outputs a length 10 numeric vector (probabilities for each digit) using a softmax activation function.

# Use the summary() function to print the details of the model:

summary(model) ## me da los detalles del modelo

# Next, compile the model with appropriate loss function, optimizer, and metrics:

model %>% compile(    ## compilo el modelo. Le digo cual es mi optimizador, cual es la función a minimizar y que me mida la precisión mientras optimiza
        loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(),
        metrics = c('accuracy')
)

#############################
## Training and Evaluation ##
#############################


history <- model %>% fit(  ## Hago el ajuste del modelo en si
        x_train, y_train, 
        epochs = 30, batch_size = 128, 
        validation_split = 0.2
)

plot(history) ## The history object returned by fit() includes loss and accuracy metrics which we can plot:

# Evaluate the model's performance on the test data:
model %>% evaluate(x_test, y_test)

# Generate predictions on new data:
model %>% predict_classes(x_test)

