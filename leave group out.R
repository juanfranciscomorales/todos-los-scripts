### VOY A INTENTAR ARMAR UN SCRIPT PARA HACER LEAVE GROUP OUT CROSS VALIDATION LOGOCV

LGOCV <- function (modelo , punto.corte.optimizado, archivo.con.datos = "Dtraining.xlsx", fraccion.grupo.out = 0.05, iteraciones = 500) {
        
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
        # Train-test splitting
        # 95% of samples -> fitting
        # 5% of samples -> testing
        smp_size <- floor( (1- fraccion.grupo.out) * nrow(df))
        index <- sample(seq_len(nrow(df)),size=smp_size)
        train <- df[index, ]
        test <- df[-index, ]
        
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
        acc[i] <- 1-misClasificError
        
        pbar$step()
}
# Average accuracy of the model
porcentaje.buenas.clasificaciones <- mean(acc)*100

porcentaje.buenas.clasificaciones
}

############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO

resultados.porcentajes.buenas.clasificaciones.LGOCV<-list() ## lista donde se van a guardar los resultados de los LOO para cada modelo

############# LOOP PARA LM

for (i in 1:length(lista.modelos)) { 
        resultados.porcentajes.buenas.clasificaciones.LGOCV[[i]]<- LGOCV (modelo = lista.modelos[[i]],  punto.corte.optimizado = puntos.corte.ROC.lm$cutoff[[i]], archivo.con.datos = "Dtraining.xlsx", fraccion.grupo.out = 0.05, iteraciones = 500)
}

############# LOOP PARA GLM

for (i in 1:length(lista.modelos)) { 
        resultados.porcentajes.buenas.clasificaciones.LGOCV[[i]]<- LGOCV (modelo = lista.modelos[[i]],  punto.corte.optimizado = puntos.corte.ROC.glm$cutoff[[i]], archivo.con.datos = "Dtraining.xlsx", fraccion.grupo.out = 0.05, iteraciones = 500)
}

