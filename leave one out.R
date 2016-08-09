### VOY A INTENTAR ARMAR UN SCRIPT PARA HACER LEAVE ONE OUT CROSS VALIDATION LOOCV
LOOCV <- function (modelo , punto.corte.optimizado, archivo.con.datos = "Dtraining.xlsx") {
       
         is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} 
        
         library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
         
         df <- read.xlsx(xlsxFile=archivo.con.datos, check.names = TRUE) ## vuelvo data frame a mi training set
         
         acc <- vector() ## creo un vector vacio donde voy a poner los resultados de cada split
        
for(i in 1:nrow(df)) {
        # Train-test splitting, como es LOOCV saco de a un dato
       
        train <- df[-i,]
        test <- df[i,]
        
        # re-Fitting con el training set menos el dato que se saco para hacer LOOCV
        model <- update( object = modelo, formula. = . ~ ., data = train)
       
        
        # Predict results
        results_prob <- predict(object = model, newdata = test,type='response')
        
        # If prob > punto de corte optimizado then 1, else 0
        results <- ifelse(test = results_prob > punto.corte.optimizado, yes = 1 ,no = 0 )
        
        # Actual answers
        answers <- test$clase
        
        # Calculate accuracy
        acc[i] <- ifelse(test = results == answers, yes = 1 ,no = 0 )
        
        
}

# Average accuracy of the model
porcentaje.buenas.clasificaciones <- mean(acc)*100

porcentaje.buenas.clasificaciones
}
############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO

resultados.porcentajes.buenas.clasificaciones.LOOCV<-list() ## lista donde se van a guardar los resultados de los LOO para cada modelo

############# LOOP PARA LM

for (i in 1:length(lista.modelos)) { 
        resultados.porcentajes.buenas.clasificaciones.LOOCV[[i]]<- LOOCV (modelo = lista.modelos[[i]],  punto.corte.optimizado = puntos.corte.ROC.lm$cutoff[[i]], archivo.con.datos = "Dtraining.xlsx")
}

############# LOOP PARA GLM

for (i in 1:length(lista.modelos)) { 
        resultados.porcentajes.buenas.clasificaciones.LOOCV[[i]]<- LOOCV (modelo = lista.modelos[[i]],  punto.corte.optimizado = puntos.corte.ROC.glm$cutoff[[i]], archivo.con.datos = "Dtraining.xlsx")
}
 