
library(data.table)

training <- as.data.frame(fread(input = "Dtrainingmiristoil.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

training <- training[ , apply(training, 2, function(x) !any(is.na(x)))] ### elimino las columnas que contienen NaN

clase <- as.factor(training$clase) ## guardo la columna clase en un elemento

training <- training[,-c(1,2)] # elimino las columnas clase y nombre, dejo solo las columnas con los descriptores

escalado.multidimensional <- as.data.frame(cmdscale(dist(training), k=3)) ## aca hago el classical multidimensional scaling (MDS)

escalado.multidimensional$clase <- clase ### le agrego la columna clase a mi resultado

library(plotly)

set.seed(100)

pal <- c("blue", "green") ## estos son los colores con los que voy a graficar

plot_ly(escalado.multidimensional, x = ~V1, y = ~V2 , color = ~clase, text = ~paste("clase: ", clase), colors = pal) ### grafico 2D del resultado del MDS

plot_ly(escalado.multidimensional, x = ~V1, y = ~V2, z = ~V3 , color = ~clase, text = ~paste("clase: ", clase), colors = pal) ## grafico 3D del resultado del MDS

library(ggplot2)

library(plyr)

df<-escalado.multidimensional

find_hull <- function(df) df[chull(df$V1, df$V2), ]

hulls <- ddply(df, "clase", find_hull)

plot <- ggplot(data = df, aes(x = V1, y = V2, colour=clase, fill = clase)) +
        geom_point(alpha = 0.5 , size = 2) + 
        geom_polygon(data = hulls, alpha = 0.1) +
        labs(x = "V1", y = "V2")

plot

ggplotly(plot)
