install.packages("webshot")

library(plotly)

webshot::install_phantomjs()

d <- diamonds[sample(nrow(diamonds), 1000), ]

plot <- plot_ly(d, x = ~carat, y = ~price, color = ~carat, size = ~carat, text = ~paste("Clarity: ", clarity))

## una forma de guardar ##

tmpFile <- tempfile(fileext = ".png")

export( p = plot, file = tmpFile ,  zoom = 200) ### supuestamente a mayor zoom mejor resolucion de la imagen, 


## otra forma de guardar ##


export(plot , delay = 0.2 ,  zoom = 2)

## una forma de visualizar en un html

browseURL(tmpFile)
