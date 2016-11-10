clasificaciones.test.set.ensemble.minimo.lm2 <- function (test.set = "dude.csv",cant.modelos = 10, x = tabla.AUC.ordenadas) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion fread
        
        df.test.set <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con mis descriptores
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        minimo<-apply(tabla.valores.prediccion.test,1,min, na.rm=FALSE)#aplico operador minimo en los valores predichos de los mejores modelos para cada compuesto
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(minimo,clase)
        
        colnames(resultado)<- c("minimo", "clase")
        
        as.data.frame(resultado)
        
}

df <- clasificaciones.test.set.ensemble.minimo.lm2(test.set  = "dude.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set)  

df$clase[df$clase == -1] <- 0

curva.roc.dude <- roc(predictor = df$minimo ,response = df$clase, direction="<")

AUC.dude <- auc(curva.roc.dude)

predicciones <- ifelse( df$minimo > resultados.ensemble.minimo[[4]], yes = 1,no = 0) ## predicciones aplicando el ensemble de operador minimo y usando el punto de corte que obtuve con el training

clase <-df$clase #extraigo los valores de la columna clase

tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany")  ##armo la tabla clasificatoria 

bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien

porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el test set

resultado.final <- list("AUC de la curva ROC", AUC.dude, "punto de corte", resultados.ensemble.minimo[[4]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion

resultado.final ## pongo el resultado final



library(pROC)

library(openxlsx)

curva.ROC.dude <- roc(response = df$clase, predictor = df$minimo, direction = "<") ## calculo de la curva ROC para los resultados de la base dude

tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de sensibilidad y especificidad, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold sensitivity y specificity

write.xlsx(x= tabla.puntos.curva.roc, file= "tabla.puntos.curva.roc.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de la dude si quiero

tabla.puntos.curva.roc.limpia<- read.xlsx(xlsxFile="tabla.puntos.curva.roc.xlsx", check.names = TRUE) 

### usar todos los datos de la tabla tabla.puntos.curva.roc hacer que no se pueda visualizar bien. Conviene acotar la tabla a los datos donde sensibilidad/especificidad es de 2.5 a 0.01 aprox

sensibilidad <- tabla.puntos.curva.roc.limpia$sensitivity ##extraigo los valores de sensibilidad

especificidad <- tabla.puntos.curva.roc.limpia$specificity##extraigo los valores de especificidad

prevalencia <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de prevalencias donde voy a calcular el PPV

list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados

for (i in 1:length(prevalencia)){ ##loop donde para cada prevalencia hago un barrido para los diferentes valores de sensibilidad/especificidad, asi calculo el PPV
        
        list.PPV[[i]] <- (sensibilidad*prevalencia[i])/(sensibilidad*prevalencia[i] + (1- especificidad)*(1- prevalencia[i]))##calculo del PPV
        
}

PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes prevalencias y las filas son las diferentes relaciones sensibilidad/especificidad, y los valores de cada celda es la PPV correspondiente para esos valores

prevalencia <- as.list(prevalencia)

sensibilidad.especificidad <- as.list(sensibilidad/especificidad)

library(plotly)

p<-plot_ly(x= prevalencia, y = sensibilidad.especificidad, z = PPV, type = "surface") #grafico de superficie 3D

####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

htmlwidgets::saveWidget(as.widget(p), "PPV.html") ### GUARDO EL GRÁFICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB




