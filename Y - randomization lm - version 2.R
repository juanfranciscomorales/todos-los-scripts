### VOY A INTENTAR ARMAR UN SCRIPT para Y-RANDOMIZATION PARA LM partiendo del pool de descriptores

y.randomization.lm <- function ( conjunto , iteraciones = 10 , punto.corte=0.05 , steps=6 , punto.corte.vif = 2) {
        
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
        
        
        for(i in 1:k) {
                
                set.seed(i)
                
                train <- transform( conjunto, clase = sample(clase)) # mezclo los valores de la columna clase
                
                model <- forward.stepwise.testF.lm ( train , punto.corte, steps , punto.corte.vif )#armo el nuevo modelo partiendo del conjunto de descriptores
                 
                # Predict results
                results_prob <- predict(object = model , type='response')
                
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
        resultado.int.conf95 <- t.test(porcentaje.buenas.clasificaciones , conf.int = TRUE)$conf.int[1:2]
        
        resultado.int.conf95
}

############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER LO DE ABAJO


resultados.y.randomization.lm <- lapply(lista.conjuntos2 , y.randomization.lm , iteraciones = 10 , punto.corte=0.05 , steps=9 , punto.corte.vif = 2)# aplico a cada conjunto la función llamada y.randomization para hacer la validacion interna y obtengo los intervalos de confianza

tabla.resultados.y.randomization.lm <- data.frame(matrix(unlist(resultados.y.randomization.lm), nrow= length(resultados.y.randomization.lm), ncol = 2 , byrow=TRUE))

colnames(tabla.resultados.y.randomization.lm) <- c("int.conf.95.inferior" ,"int.conf.95.superior")

tabla.resultados.y.randomization.lm$modelo <- 1:length(lista.modelos)

library(openxlsx)

write.xlsx(x= tabla.resultados.y.randomization.lm, file= "resultados validacion Y-randomization.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos
