### VOY A INTENTAR ARMAR UN SCRIPT para Y-RANDOMIZATION PARA LM partiendo del pool de descriptores

y.randomization.testLRT.glm <- function ( conjunto ,iteraciones = 500,punto.corte=0.05,steps=6) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} 
        
        if (is.installed("caret") == FALSE) {install.packages("caret")} 
        
        library(plyr)
        
        library(caret)
        
        # Number of iterations
        k <- iteraciones
        
        # Initialize progress bar
        pbar <- create_progress_bar('text')
        
        pbar$init(k)
        
        # Accuracy
        porcentaje.buenas.clasificaciones <- vector()
        
        set.seed(123)
        
        for(i in 1:k) {
                train <- transform( conjunto, clase = sample(clase)) # mezclo los valores de la columna clase
                
                model <- forward.stepwise.testLRT.glm( train ,punto.corte,steps)#armo el nuevo modelo partiendo del conjunto de descriptores
                
                # Predict results
                results_prob <- predict(object = model,type='response')
                
                # If prob > 0.5 then 1, else 0
                results <- ifelse(test = results_prob > 0.5, yes = 1 ,no = 0 )
                
                # Actual answers
                answers <- train$clase
                
                # Accuracy calculation
                misClasificError <- mean(answers != results)
                
                # Collecting results
                porcentaje.buenas.clasificaciones[i] <- 100*(1-misClasificError)
                
                pbar$step()
        }
        # Average accuracy and confidence interval of the model
        resultado <- t.test(porcentaje.buenas.clasificaciones)
        
        resultado
}

############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER LO DE ABAJO


resultados.y.randomization.testLRT.glm <- lapply(lista.conjuntos2, y.randomization.testLRT.glm, iteraciones = 500,punto.corte=0.05,steps=6)# aplico a cada conjunto la función llamada y.randomization para hacer la validacion interna y obtengo los intervalos de confianza
