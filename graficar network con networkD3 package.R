
        ######################################################

### grafico rapido de la red en 3D sin ningun formato adicional #####

        ######################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no
        
if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("networkD3") == FALSE) {install.packages("networkD3")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table) ## cargo este paquete para leer rapido los archivos

library(networkD3) ## este paquete sirve para graficar las redes en 3D de forma interactiva

df <- as.data.frame(fread(input = "repoDB.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

df <- df[df$status=="Approved",] ## extraigo solo los farmacos que tienen usos aprobados

links_simple <- data.frame(from = df[,"drug_name"], to = df[,"ind_name"]) ## armo este data frame para simpleNetwork

simpleNetwork(Data = links_simple, fontSize = 15 , opacity = 0.5 , zoom = TRUE , nodeColour = "red",  linkColour ="blue") ## con esto genero una network pero no se diferencia que es enfermedad de droga. La funcion simpleNetwork no permite darle formato a la red



######################################################

### grafico 3D de la red con formato #####

######################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("networkD3") == FALSE) {install.packages("networkD3")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("igraph") == FALSE) {install.packages("igraph")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(networkD3) ## este paquete sirve para graficar las redes en 3D de forma interactiva

library(igraph) ## este paquete sirve para hacer el analisis de las redes

library(data.table) ## cargo este paquete para leer rapido los archivos

set.seed(1)

df <- as.data.frame(fread(input = "repoDB.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

df <- df[df$status=="Approved",] ## extraigo solo los farmacos que tienen usos aprobados

df$drug_name <- gsub('<', '&lt;', df$drug_name) ##con esto cambio en la columna drgu_name los simbolos < por &lt; porque sino me tira error cuando quiero volverlo formato gexf

df$ind_name <- gsub('<', '&lt;', df$ind_name) ##con esto cambio en la columna drgu_name los simbolos < por &lt; porque sino me tira error cuando quiero volverlo formato gexf

drug_name <- rep("drug_name" , length(unique(df[,"drug_name"]))) ## armo un vector que contenga drug_name repetido la cantidad de farmacos que tengo

ind_name <- rep("ind_name" , length(unique(df[,"ind_name"]))) ### armo un vector que contenga ind_name repetido la cantidad de enfermedades que tengo        

group <- c(drug_name,ind_name) ## creo el vector group que tiene ordenado de manera tal para diferenciar cuales son drogas y cuales son enfermedades

df <- data.frame(source = df$drug_name , target = df$ind_name)## Cada fila representa un enlace. creo un dataframe donde tengo solo las drogas y las enfermedades. 

lvls <- unique(unlist(df))  ## armo un vector que contenga de manera no repetida las drogas y las enfermedades que tengo

df[] <-  lapply(df, factor, levels=lvls)  ## hago que ambas columnas tengan los mismos factores. Esto lo hago para que cada enfermedad o droga sea un determinado numero de nodo que no se repite


# Basic Graph

g <- graph.data.frame(df, directed=F) # mi dataframe ahora es una red de igraph

g <- simplify(g) #  A graph is simple if it does not contain loop edges and multiple edges. Elimina enlaces loops(o sea del nodo, a si mismo) y enlaces entre nodos que se repitan


## Make a vertices df
vertices <- data.frame(name = V(g)$name) ## hago un dataframe llamado vertices donde genero una columna name que son los nombres de todos lso vertices

vertices$degree <- degree(g) ## agrego al dataframe vertices el grado (cant de uniones) a cada vertice

vertices$betweenness <- betweenness(g) ## agrego al dataframe vertices el valor de betweenness a cada vertice

vertices$closeness <- closeness(g) ## agrego al dataframe vertices el valor de closeness a cada vertice

vertices$evcent <- evcent(g)$vector ## agrego al dataframe vertices el valor de evcent a cada vertice

vertices$constraint <-  constraint(g) ## agrego al dataframe vertices el valor de constraint a cada vertice

vertices$group <- group ## agrego al dataframe vertices la columna group que indica si el nodo es enfermedad o droga


# create indices (indexing needs to be JS format)

# genero los indices numericos que deben empezar desde el cero, para los nodos source y target, que son los necesarios para poder graficar con forceNetwork

df$source.index = match(df$source, vertices$name)-1

df$target.index = match(df$target, vertices$name)-1

head(df)


# Grafico la red de forma interactiva en 3D con la funcion forceNetwork que me deja darle diferentes formatos, 
# a diferencia de la funcion simpleNetwork, que me hace una red simple, que no puedo darle formato


d3 = forceNetwork(Links = df, # a data frame object with the links between the nodes. It should include the Source and Target for each link. These should be numbered starting from 0. An optional Value variable can be included to specify how close the nodes are to one another.
                  Nodes = vertices, # a data frame containing the node id and properties of the nodes. If no ID is specified then the nodes must be in the same order as the Source variable column in the Links data frame. Currently only a grouping variable is allowed.
                  Source = 'source.index', # character string naming the network source variable in the Links data frame.
                  Target = 'target.index', # character string naming the network target variable in the Links data frame.
                  NodeID = 'name', #character string specifying the node IDs in the Nodes data frame.
                  Group = 'group', # color nodes by group calculated earlier. character string specifying the group of each node in the Nodes data frame.
                  charge = -1, # node repulsion. numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value).
                  #linkDistance = 20,
                  zoom = T,  # logical value to enable (TRUE) or disable (FALSE) zooming.
                  opacity = 0.8, # numeric value of the proportion opaque you would like the graph elements to be.
                  legend=T, # logical value to enable node colour legends.
                  fontSize=24, # numeric font size in pixels for the node text labels.
                  bounded = F, # logical value to enable (TRUE) or disable (FALSE) the bounding box limiting the graph's extent. See http://bl.ocks.org/mbostock/1129492.
                  Nodesize="degree", # character string specifying the a column in the Nodes data frame with some value to vary the node radius's with. See also radiusCalculation.
                  #width = 600, # numeric width for the network graph's frame area in pixels.
                  #height = 400, # numeric height for the network graph's frame area in pixels
                  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);") # character string specifying the categorical colour scale for the nodes. See https://github.com/d3/d3/blob/master/API.md#ordinal-scales.
)

show(d3) ## imprimo el grafico de red interactivo armado anteriormente

saveNetwork(d3, "network.html", selfcontained = TRUE) ## guardo la red interactiva como html



