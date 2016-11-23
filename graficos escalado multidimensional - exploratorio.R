

######################################################################


        #  GRAFICOS DE MULTIDIMENSIONAL SCALING (MDS)   #


######################################################################

# https://en.wikipedia.org/wiki/Multidimensional_scaling




library(data.table) # abro el paquete data.table

datos <- "Descriptores Training Set Sofi.csv" ## el nombre del archivo con los datos que quiero graficar

set.seed(100)

training <- as.data.frame(fread(input = datos , check.names = TRUE)) #leo el archivo con mis descriptores que quiero analizar por MDS

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training <- training[,-c(1,2)] # elimino las columnas clase y nombre, dejo solo las columnas con los descriptores

escalado.multidimensional <- as.data.frame(cmdscale(dist(training), k=3)) ## aca hago el classical multidimensional scaling (MDS)

escalado.multidimensional$clase <- clase ### le agrego la columna clase a mi resultado

library(plotly) ### abro el paquete plotly

pal <- c("blue", "green") ## estos son los colores con los que voy a graficar

plot_ly(escalado.multidimensional, x = ~V1, y = ~V2 , color = ~clase, text = ~paste("clase: ", clase), colors = pal) ### grafico 2D del resultado del MDS

plot_ly(escalado.multidimensional, x = ~V1, y = ~V2, z = ~V3 , color = ~clase, text = ~paste("clase: ", clase), colors = pal) ## grafico 3D del resultado del MDS

library(ggplot2) ## abro el paquete ggplot2

library(plyr) ## abro el paquete plyr que lo necesito para armar el hull

df<-escalado.multidimensional  ##  guardo el dataframe escalado.multidimensional en el elemento df

find_hull <- function(df) df[chull(df$V1, df$V2), ] ## creo la funcion find_hull que es necesaria para encontrar el hull

hulls <- ddply(df, "clase", find_hull) ## en este paso encuentro los puntos que son el hull

plot0 <- ggplot(data = df, aes(x = V1, y = V2)) + ## con esto comienzo a armar el grafico, le digo cuales son mis datos
        geom_point(alpha = 0.5 , size = 2) +  ## en este paso doy formato a los puntos que se grafican
        labs(x = "V1", y = "V2") + ## con esto le doy nombre a los ejes
        ggtitle("Multidimensional Scaling (MDS)") + ## con esto le doy el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot0 ## imprimo el grafico con los datos crudos

plot1 <- ggplot(data = df, aes(x = V1, y = V2, colour=clase, fill = clase)) + ## con esto comienzo a armar el grafico, le digo cuales son mis datos
        geom_point(alpha = 0.5 , size = 2) +  ## en este paso doy formato a los puntos que se grafican
        labs(x = "V1", y = "V2") + ## con esto le doy nombre a los ejes
        ggtitle("Multidimensional Scaling (MDS)") + ## con esto le doy el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot1 ## imprimo el grafico con los datos separados solo por clase


plot2 <- ggplot(data = df, aes(x = V1, y = V2, colour=clase, fill = clase)) + ## con esto comienzo a armar el grafico, le digo cuales son mis datos
        geom_polygon(data = hulls, alpha = 0.1) + ## aca hago que me grafique los hull
        geom_point(alpha = 0.5 , size = 2) +  ## en este paso doy formato a los puntos que se grafican
        labs(x = "V1", y = "V2") + ## con esto le doy nombre a los ejes
        ggtitle("Multidimensional Scaling (MDS)") + ## con esto le doy el nombre al grafico
        theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado
        
plot2 ## imprimo el grafico con los hull

plot3 <- ggplot(data = df, aes(x = V1, y = V2, colour=clase, fill = clase)) + ## con esto comienzo a armar el grafico, le digo cuales son mis datos
         stat_ellipse(type = "norm" , level = 0.95 , geom = "polygon" , alpha = 0.1) + ## en este paso hago que me grafique las elipses suponiendo distribucion normal de los puntos y con un 0.95 de nivel de confianza
         geom_point(alpha = 0.5 , size = 2) +   ## en este paso doy formato a los puntos que se grafican
         labs(x = "V1", y = "V2") + ## con esto le doy nombre a los ejes
         ggtitle("Multidimensional Scaling (MDS)") + ## con esto le doy el nombre al grafico
         theme(plot.title = element_text(hjust = 0.5)) ## con esto hago que el titulo este centrado

plot3  ## imprimo el grafico con las elipses

ggplotly(plot0) ## hago que el grafico con los hull se vuelva interactivo como los de plotly

ggplotly(plot1)## hago que el grafico con las elipses se vuelva interactivo como los de plotly

ggplotly(plot2)## hago que el grafico con las elipses se vuelva interactivo como los de plotly

ggplotly(plot3)## hago que el grafico con las elipses se vuelva interactivo como los de plotly
