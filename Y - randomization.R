### VOY A INTENTAR ARMAR UN SCRIPT para Y - Randomization partiendo de los descriptores que ya habia elegido el modelo

y.randomization <- function (modelo , archivo.con.datos = "Dtraining.xlsx",iteraciones = 500) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} 
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} 
        
        if (is.installed("caret") == FALSE) {install.packages("caret")} 
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        library(plyr)
        
        library(caret)
        
        df <- read.xlsx(xlsxFile=archivo.con.datos, check.names = TRUE) ## vuelvo data frame a mi training set
        
        # Number of iterations
        k <- iteraciones
        
        # Initialize progress bar
        pbar <- create_progress_bar('text')
        
        pbar$init(k)
        
        # Accuracy
        acc <- vector()
        
        set.seed(123)
        
        for(i in 1:k) {
                train <- transform( df, clase = sample(clase))
                
                # re-Fitting con el training set menos los datos que se saco para hacer LGOCV
                model <- update( object = modelo, formula. = . ~ ., data = train)
                
                # Predict results
                results_prob <- predict(object = model,type='response')
                
                # If prob > 0.5 then 1, else 0
                results <- ifelse(test = results_prob > 0.5, yes = 1 ,no = 0 )
                
                # Actual answers
                answers <- train$clase
                
                # Accuracy calculation
                misClasificError <- mean(answers != results)
                
                # Collecting results
                acc[i] <- 100*(1-misClasificError)
                
                pbar$step()
        }
        # Average accuracy and confidence interval of the model
        resultado <- t.test(acc)
        
        resultado
}

############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO

resultados.y.randomization<- lapply(lista.modelos, y.randomization, archivo.con.datos = "Dtraining.xlsx", iteraciones = 500,punto.corte=0.05,steps=6)#





