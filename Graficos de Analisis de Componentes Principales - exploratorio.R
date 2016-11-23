
#####################################################################################

#               GRAFICOS DE ANALISIS DE COMPONENTES PRINCIPALES             

#####################################################################################


library(data.table)

datos <- "Dtrainingmiristoil.csv"  ## aca va el nombre del archivo con los datos

training <- as.data.frame(fread(input = datos, check.names = TRUE)) #leo el archivo con mis descriptores 

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

training <- training[,-1] # elimino la columna nombres, dejo solo las columnas con los descriptores y la clase

training <- Filter(function(x) sd(x) != 0, training) ## para eliminar las columnas que su desviacion estandar sd = 0 , o sea no tienen variabilidad esas columnas

training$clase <- as.factor(training$clase) ## me aseguro que la columna clase este guardada como factor

clase <- training$clase ## creo el elemento clase, para usarlo luego en los graficos

res.pca <- prcomp(x= training[, -1] , center = TRUE, scale. = TRUE) ## hago componentes principales. Escalo y centro los datos, dado que sino me va a dar diferente resultado

plot(res.pca, type ="l") ### me grafica la varianza en funcion de la cantidad de componentes

library(ggplot2) ## abro el paquete ggplot2 para poder graficar

df <- data.frame(res.pca$x , clase ) ## pongo en un solo data frame los datos de las coordenadas de cada punto en las componentes principales y la clase de cada elemento

library(plyr) ## abro el paquete plyr que lo necesito para armar el hull

find_hull <- function(df) df[chull(df$PC1, df$PC2), ] ## creo la funcion find_hull que es necesaria para encontrar el hull

hulls <- ddply(df, "clase", find_hull) ## en este paso encuentro los puntos que son el hull

plot0 <- ggplot(data  = df , aes(x = PC1 , y = PC2)) + ## en este paso comienzo el grafico de componentes principales
        geom_point(alpha = 0.6 , size = 2.5) +  ## aca le doy formato a los puntos a graficar
        labs(title = "Individuals factor  map - PCA") + ## aca le pongo el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot0

plot1 <- ggplot(data  = df , aes(x = PC1 , y = PC2 , color = clase)) + ## en este paso comienzo el grafico de componentes principales
        geom_point(alpha = 0.6 , size = 2.5) +  ## aca le doy formato a los puntos a graficar
        labs(title = "Individuals factor  map - PCA") + ## aca le pongo el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot1

plot2 <- ggplot(data  = df , aes(x = PC1 , y = PC2 , color = clase)) + ## en este paso comienzo el grafico de componentes principales
        geom_polygon(data =  hulls , alpha = 0.1) + ## aca hago que me grafique los hull
        geom_point(alpha = 0.5 , size = 2.5) + ## aca le doy formato a los puntos a graficar
        labs(title = "Individuals factor  map - PCA") + ## aca le pongo el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot2

plot3 <- ggplot(data  = df , aes(x = PC1 , y = PC2 , color = clase)) + ## en este paso comienzo el grafico de componentes principales
        stat_ellipse(type = "norm" , level = 0.95 , geom = "polygon" , alpha = 0.2) + ## en este paso hago que me grafique las elipses suponiendo distribucion normal de los puntos y con un 0.95 de nivel de confianza
        geom_point(alpha = 0.6 , size = 2.5) +  ## aca le doy formato a los puntos a graficar
        labs(title = "Individuals factor  map - PCA") + ## aca le pongo el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot3

library(plotly)

plotly0 <- ggplotly(plot0)

plotly0

plotly1 <- ggplotly(plot1)

plotly1

plotly2 <- ggplotly(plot2)

plotly2

plotly3 <- ggplotly(plot3)

plotly3
