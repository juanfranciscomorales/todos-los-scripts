### VOY A INTENTAR ARMAR UN SCRIPT PARA HACER LEAVE GROUP OUT "LGO"

LGO <- function (modelo , punto.corte.optimizado = 0.5, archivo.con.datos = "Dtraining.csv", fraccion.grupo.out = 0.1, iteraciones = 500) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} 
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} 
        
        if (is.installed("caret") == FALSE) {install.packages("caret")} 
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        library(plyr)
        
        library(caret)
        
        df <- as.data.frame(fread(input = archivo.con.datos, check.names = TRUE)) #leo el archivo con mis descriptores
        
        # Number of iterations
        k <- iteraciones

        # Initialize progress bar
        
        pb <- txtProgressBar(min = 0, max = iteraciones, style = 3)
        
        # Accuracy
        acc <- vector()

        set.seed(123)

        for(i in 1:k) {
        # Train-test splitting
        # 95% of samples -> fitting
        # 5% of samples -> testing
        smp_size <- floor( (1- fraccion.grupo.out) * nrow(df))
        
        index <- sample(seq_len(nrow(df)),size=smp_size)
        
        train <- df[index, ]
        
        test <- df[-index, ]
        
        # re-Fitting con el training set menos los datos que se saco para hacer LGO
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
        acc[i] <- 1-misClasificError
        
        setTxtProgressBar(pb, i)
        
        
        }
        
# Average accuracy of the model
porcentaje.buenas.clasificaciones <- mean(acc)*100

close(pb)

porcentaje.buenas.clasificaciones

}

############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO

lista.resultados.porcentajes.buenas.clasificaciones.LGO <- lapply(lista.modelos , LGO ,punto.corte.optimizado = 0.5 , archivo.con.datos = "training set.csv", fraccion.grupo.out = 0.1, iteraciones = 50) ## lista donde se van a guardar los resultados de los LOO para cada modelo



