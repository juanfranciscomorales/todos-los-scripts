

k.fold.CV.lm <- function (modelo , punto.corte.optimizado = 0.5, archivo.con.datos = "training set.csv", folds = 10 , repeticiones = 1) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} 
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} 
        
        if (is.installed("caret") == FALSE) {install.packages("caret")} 
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        library(plyr)
        
        library(caret)
        
        df <- as.data.frame(fread(input = archivo.con.datos, check.names = TRUE)) #leo el archivo con mis descriptores
        
         # Initialize progress bar
        
        pb <- txtProgressBar(min = 0, max = repeticiones, style = 3)
        
         # Accuracy
        acc <- matrix(nrow = folds , ncol = repeticiones)
        
        
        for(j in 1:repeticiones) {
        
        set.seed(j)
        
        index <- createFolds( y = factor(df$clase) , k = folds , list = TRUE , returnTrain = FALSE)
        
        for(i in 1:folds) {
                
                ### train - test splitting
               
                train <- df[-index[[i]], ]
                
                test <- df[index[[i]], ]
                
                # re-Fitting con el training set menos los datos que se saco para hacer LGOCV
                model <- update( object = modelo, formula. = . ~ ., data = train)
                
                # Predict results
                results_prob <- predict(object = model, newdata = test,type='response')
                
                # If prob > punto de corte optimizado then 1, else 0
                results <- ifelse(test = results_prob > punto.corte.optimizado, yes = 1 ,no = 0 )
                
                # Actual answers
                answers <- test$clase
                
                # Accuracy calculation
                misClasificError <- mean(answers != results)
                
                # Collecting results
                acc[i,j] <- 1-misClasificError
                
                
        }
        
         setTxtProgressBar(pb, j)

        }
        
        # Average accuracy of the model
        porcentaje.buenas.clasificaciones <- mean(colMeans(acc))*100
        
        close(pb)
        
        porcentaje.buenas.clasificaciones
        
       }
        



############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO


lista.porcentajes.buenas.clasificaciones.k.fold.CV.lm <- lapply(lista.modelos , k.fold.CV.lm , punto.corte.optimizado = 0.5 , archivo.con.datos = "S-M training set lm.csv", folds = 10 , repeticiones = 5)# aplico a cada conjunto la funci?n llamada forward.stepwise.testF.lm para obtener los modelos. si quiero modifico punto de corte y numero de steps

tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm <- data.frame(modelo = 1:length(lista.modelos) , porcentaje.BC.kfold.CV =unlist(lista.porcentajes.buenas.clasificaciones.k.fold.CV.lm)) ## paso de lista a tabla

tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm <- tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm[order(tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm$porcentaje.BC.kfold.CV, decreasing = TRUE) ,] ## ordeno la tabla de manera decreciente

library(openxlsx)

write.xlsx(x= tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm, file= "Resultado k fold CV.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos


## LOO  ###

## SI QUIERO LEAVE ONE OUT HAGO LO SIGUIENTE  ##############


lista.porcentajes.buenas.clasificaciones.LOO.lm <- lapply( lista.modelos , k.fold.CV.lm , punto.corte.optimizado = 0.5 , archivo.con.datos = "training set.csv", folds = nrow(lista.conjuntos2[[1]]) , repeticiones = 1)# aplico a cada conjunto la funci?n llamada forwar

tabla.porcentajes.buenas.clasificaciones.LOO.lm <- data.frame(modelo = 1:length(lista.modelos) , porcentaje.BC.LOO = unlist(lista.porcentajes.buenas.clasificaciones.LOO.lm))

tabla.porcentajes.buenas.clasificaciones.LOO.lm <- tabla.porcentajes.buenas.clasificaciones.LOO.lm[order(tabla.porcentajes.buenas.clasificaciones.LOO.lm$porcentaje.BC.LOO , decreasing = TRUE) ,] ## ordeno la tabla de manera decreciente


library(openxlsx)

write.xlsx(x= tabla.porcentajes.buenas.clasificaciones.LOO.lm, file= "Resultado LOO CV.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos


