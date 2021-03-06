clasificaciones.test.set.ensemble.ranking.lm2 <- function (test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est� instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est� instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        ranking<-apply(-tabla.valores.prediccion.test,2,rank, na.last= "keep", ties.method = "first")#aplico operador ranking en los valores predichos de los mejores modelos para cada compuesto. Me da una matriz donde tengo el lugar del ranking que ocupa mi compuesto para cada modelo. filas son los compuestos y las columnas son los modelos
        
        promedio.ranking <- apply(ranking, 1, mean, na.rm = FALSE) ## calculo para cada compuesto cual es el promedio de ranking
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        resultado <- cbind(promedio.ranking,clase)
        
        colnames(resultado)<- c("promedio.ranking", "clase")
        
        as.data.frame(resultado)
        
}

### LA FUNCION ANTERIOR  LO QUE HACE ES TIRARME EL VALOR DE LA CLASE 
### Y EL VALOR  ranking promedio PARA CADA COMPUESTO DE LA BASE DUDE

df <- clasificaciones.test.set.ensemble.ranking.lm2(test.set  = "Dudepoliaminas2016.csv",cant.modelos = 15, x = tabla.AUC.ordenadas.test.set)  ## aplico la funcion anterior para obtener un data frame con los valores de score  voto promedio y la clase real

df$clase[df$clase == -1] <- 0 ## esto es por si llega a haber en el archivo original haber puesto a los inactivos como -1, pasarlos a 0

curva.roc.dude <- roc(predictor = df$promedio.ranking ,response = df$clase, direction=">" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE , plot = TRUE) ## calculo la curva ROC con los datos del score  voto promedio y la clase verdadera

AUC.dude <- curva.roc.dude$auc[[1]] ## calculo el AUC ROC

int.conf.95.AUC.ROC <- curva.roc.dude$ci ## extraigo el intervalo de confianza del AUC ROC

predicciones <- ifelse( df$promedio.ranking < resultados.ensemble.ranking[[6]], yes = 1,no = 0) ## predicciones aplicando el ensemble de operador voto y usando el punto de corte que obtuve con el training

clase <-df$clase #extraigo los valores de la columna clase

tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany")  ##armo la tabla clasificatoria 

bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien

porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el test set

resultado.final <- list("AUC de la curva ROC", AUC.dude,"Int Confianza AUC ROC" , int.conf.95.AUC.ROC , "punto de corte", resultados.ensemble.ranking[[6]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion

resultado.final ## pongo el resultado final. Sirve para verificar que esta todo bien



library(pROC)

library(openxlsx)

curva.ROC.dude <- roc(response = df$clase, predictor = df$promedio.ranking, direction = ">") ## calculo de la curva ROC para los resultados de la base dude

tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de sensibilidad y especificidad, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold sensitivity y specificity

tabla.puntos.curva.roc$se.sp <- tabla.puntos.curva.roc$sensitivity/tabla.puntos.curva.roc$specificity ## creo una columna donde estan los valores de sensiblidad/especificidad, o sea la division de estos valores

tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc[ tabla.puntos.curva.roc$se.sp <= 2, ]  ### elimino los valores de sensiblidad/especificidad mayores a 2 porque no me dan informacion para el grafico

tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc.limpia[ !tabla.puntos.curva.roc.limpia$se.sp == 0, ] ## elimino los valores de sensibilidad/espeficidad que son iguales a cero porque tampoco me sirven para graficar

write.xlsx(x= tabla.puntos.curva.roc.limpia, file= "tabla.puntos.curva.roc.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

Sensitivity <- tabla.puntos.curva.roc.limpia$sensitivity ##extraigo los valores de Sensitivity

Specificity <- tabla.puntos.curva.roc.limpia$specificity##extraigo los valores de Specificity

Prevalence <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de Prevalences donde voy a calcular el PPV

list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados

for (i in 1:length(Prevalence)){ ##loop donde para cada Prevalence hago un barrido para los diferentes valores de Sensitivity/Specificity, asi calculo el PPV
        
        list.PPV[[i]] <- (Sensitivity*Prevalence[i])/(Sensitivity*Prevalence[i] + (1- Specificity)*(1- Prevalence[i]))##calculo del PPV
        
}

PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes Prevalences y las filas son las diferentes relaciones Sensitivity/Specificity, y los valores de cada celda es la PPV correspondiente para esos valores

Prevalence <- as.list(Prevalence)

library(plotly)

f1 <- list( size = 18) ## esto es si quiero cambiar algo de la fuente del titulo de los ejes

f2 <- list( size = 14) ## esto es si quiero cambiar algo de la fuente de las marcas de los ejes

axis.x <- list(title="Ya", ## opciones para el eje x
               titlefont = f1,## para cambiar la fuente del titulo
               tickfont = f2,## para cambiar la fuente de la marca de los ejes
               showgrid = T, ## si se muestra la cuadricula
               gridwidth = 10, ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje


axis.y <- list(title="Sensitivity/Specificity", ## opciones para el eje y
               titlefont = f1,## para cambiar la fuente del titulo
               tickfont = f2,## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 10, ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje


axis.z <- list(title="PPV",  ## opciones para el eje z
               titlefont = f1,## para cambiar la fuente del titulo
               tickfont = f2,## para cambiar la fuente de la marca de los ejes
               showgrid = T , ## si se muestra la cuadricula
               gridwidth = 10, ## el ancho de la linea de la cuadricula
               linewidth = 10) ## el ancho de la linea del eje


scene <- list(              ## resumo las info de los ejes en esta variable llamada "scene"
        xaxis = axis.x,
        yaxis = axis.y,
        zaxis = axis.z)

####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

p<-plot_ly(x= ~Prevalence, y = ~Sensitivity/Specificity, z = ~PPV, type = "surface") %>% layout( title ="3D Surface PPV" , scene = scene)  # hago el grafico de superficie 3D, aca especifico el nombre del grafico y luego en scene pongo la variable scene que tiene los formatos deseados

p

htmlwidgets::saveWidget(as.widget(p), "PPV.html") ### GUARDO EL GR�FICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB





##########################################################################

#     LOOP PARA HACER VARIOS GRAFICOS 3D DE MANERA AUTOMATICA

##########################################################################

##########################################################################


secuencia <- seq(from = 5, to=30 , by = 5) 

for( j in secuencia) {
        
        df <- clasificaciones.test.set.ensemble.ranking.lm2(test.set  = "Descriptores DUDE completa sofi.csv",cant.modelos = j, x = tabla.AUC.ordenadas.test.set)  ## aplico la funcion anterior para obtener un data frame con los valores de score  voto promedio y la clase real
        
        df$clase[df$clase == -1] <- 0 ## esto es por si llega a haber en el archivo original haber puesto a los inactivos como -1, pasarlos a 0
        
        library(pROC)
        
        curva.ROC.dude <- roc(response = df$clase, predictor = df$promedio.ranking, direction = ">") ## calculo de la curva ROC para los resultados de la base dude
        
        tabla.puntos.curva.roc<- as.data.frame(t(as.data.frame(coords(roc=curva.ROC.dude,x="all"))))## lo que hago es obtener todos los valores de sensibilidad y especificidad, los vuelvo data frame y los traspongo para obtener una tabla con columnas thershold sensitivity y specificity
        
        tabla.puntos.curva.roc$se.sp <- tabla.puntos.curva.roc$sensitivity/tabla.puntos.curva.roc$specificity ## creo una columna donde estan los valores de sensiblidad/especificidad, o sea la division de estos valores
        
        tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc[ tabla.puntos.curva.roc$se.sp <= 2, ]  ### elimino los valores de sensiblidad/especificidad mayores a 2 porque no me dan informacion para el grafico
        
        tabla.puntos.curva.roc.limpia <- tabla.puntos.curva.roc.limpia[ !tabla.puntos.curva.roc.limpia$se.sp == 0, ] ## elimino los valores de sensibilidad/espeficidad que son iguales a cero porque tampoco me sirven para graficar
        
        Sensitivity <- tabla.puntos.curva.roc.limpia$sensitivity ##extraigo los valores de Sensitivity
        
        Specificity <- tabla.puntos.curva.roc.limpia$specificity##extraigo los valores de Specificity
        
        Prevalence <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de Prevalences donde voy a calcular el PPV
        
        list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados
        
        for (i in 1:length(Prevalence)){ ##loop donde para cada Prevalence hago un barrido para los diferentes valores de Sensitivity/Specificity, asi calculo el PPV
                
                list.PPV[[i]] <- (Sensitivity*Prevalence[i])/(Sensitivity*Prevalence[i] + (1- Specificity)*(1- Prevalence[i]))##calculo del PPV
                
        }
        
        PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes Prevalences y las filas son las diferentes relaciones Sensitivity/Specificity, y los valores de cada celda es la PPV correspondiente para esos valores
        
        Prevalence <- as.list(Prevalence)
        
        library(plotly)
        
        f1 <- list( size = 18) ## esto es si quiero cambiar algo de la fuente del titulo de los ejes
        
        f2 <- list( size = 14) ## esto es si quiero cambiar algo de la fuente de las marcas de los ejes
        
        axis.x <- list(title="Ya", ## opciones para el eje x
                       titlefont = f1,## para cambiar la fuente del titulo
                       tickfont = f2,## para cambiar la fuente de la marca de los ejes
                       showgrid = T, ## si se muestra la cuadricula
                       gridwidth = 10, ## el ancho de la linea de la cuadricula
                       linewidth = 10) ## el ancho de la linea del eje
        
        
        axis.y <- list(title="Sensitivity/Specificity", ## opciones para el eje y
                       titlefont = f1,## para cambiar la fuente del titulo
                       tickfont = f2,## para cambiar la fuente de la marca de los ejes
                       showgrid = T , ## si se muestra la cuadricula
                       gridwidth = 10, ## el ancho de la linea de la cuadricula
                       linewidth = 10) ## el ancho de la linea del eje
        
        
        axis.z <- list(title="PPV",  ## opciones para el eje z
                       titlefont = f1,## para cambiar la fuente del titulo
                       tickfont = f2,## para cambiar la fuente de la marca de los ejes
                       showgrid = T , ## si se muestra la cuadricula
                       gridwidth = 10, ## el ancho de la linea de la cuadricula
                       linewidth = 10) ## el ancho de la linea del eje
        
        
        scene <- list(              ## resumo las info de los ejes en esta variable llamada "scene"
                xaxis = axis.x,
                yaxis = axis.y,
                zaxis = axis.z)
        
        ####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 
        
        p<-plot_ly(x= ~Prevalence, y = ~Sensitivity/Specificity, z = ~PPV, type = "surface") %>% layout( title ="3D Surface PPV" , scene = scene)  # hago el grafico de superficie 3D, aca especifico el nombre del grafico y luego en scene pongo la variable scene que tiene los formatos deseados
        
        htmlwidgets::saveWidget(as.widget(p), file =  paste("GRAFICO 3D PPV " , j," MEJORES MODELOS ENSEMBLE RANKING", ".html", sep="")) ### GUARDO EL GR�FICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB
        
}
