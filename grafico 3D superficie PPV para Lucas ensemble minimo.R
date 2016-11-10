
library(openxlsx)

lucas <- read.xlsx(xlsxFile="Tabla Sp - Se para valor de corte.xlsx", check.names = TRUE) #leo el archivo con los datos de sensibilidad y especificidad en cada punto de corte de la curva ROC

Se <- lucas$Se##extraigo los valores de sensibilidad

Sp <- lucas$Sp##extraigo los valores de especificidad

Ya <- seq(from =0 , to =0.01, by=0.001) ## armo una secuencia de prevalencias donde voy a calcular el PPV

list.PPV<-list() ##creo lista vacia donde voy a ponerlos valorse de PPV calculados

for (i in 1:length(Ya)) { ##loop donde para cada prevalencia hago un barrido para los diferentes valores de sensibilidad/especificidad, asi calculo el PPV
        
        list.PPV[[i]] <- (Se*Ya[i])/(Se*Ya[i] + (1- Sp)*(1- Ya[i]))##calculo del PPV
        
}

PPV <- matrix(unlist(list.PPV), nrow= length(list.PPV[[1]]), byrow=FALSE) ## es una matriz donde las columnas son las diferentes prevalencias y las filas son las diferentes relaciones sensibilidad/especificidad, y los valores de cada celda es la PPV correspondiente para esos valores

library(plotly)

p<-plot_ly(x= Ya, y = Se/Sp, z = PPV , type = "surface") ##grafico de superficie 3D

####IMPORTANTE##### SABER QUE LAS COLUMNAS DE LA MATRIX EN Z SE CORRESPONDEN A X Y LAS FILAS DE Z SE CORRESPONDEN A Y 

htmlwidgets::saveWidget(as.widget(p), "index.html") ### GUARDO EL GRÁFICO COMO HTML Y LUEGO LO PUEDO VER EN CUALQUIER NAVEGADOR WEB

Png <- plotly_IMAGE(x=p, width = 1280, height = 720, out_file = "Minimo - Poliaminas.png")

Jpeg <- plotly_IMAGE(x=p, width = 1920, height = 1080, format = "jpeg",  out_file = "Minimo - Poliaminas.jpeg")

Sys.setenv("plotly_username" ="juanfranciscomorales") ##seteo mi usuario de plotly

Sys.setenv("plotly_api_key"="9lb4lh4kcy") ##seteo mi api key que figura en la siguiente pagina https://plot.ly/settings/api


