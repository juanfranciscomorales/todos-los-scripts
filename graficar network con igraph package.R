######################################################

# Uso IGRAPH PARA ARMAR UNA RED 

######################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("igraph") == FALSE) {install.packages("igraph")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(igraph) ## este paquete sirve para hacer el analisis de las redes

library(data.table) ## cargo este paquete para leer rapido los archivos

set.seed(1)

df <- as.data.frame(fread(input = "repoDB.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

df <- df[df$status=="Approved",] ## extraigo solo los farmacos que tienen usos aprobados

drug_name <- rep("drug_name" , length(unique(df[,"drug_name"]))) ## armo un vector que contenga drug_name repetido la cantidad de farmacos que tengo

ind_name <- rep("ind_name" , length(unique(df[,"ind_name"]))) ### armo un vector que contenga ind_name repetido la cantidad de enfermedades que tengo        

group <- c(drug_name,ind_name) ## creo el vector group que tiene ordenado de manera tal para diferenciar cuales son drogas y cuales son enfermedades

df <- data.frame(source = df$drug_name , target = df$ind_name) ## Cada fila representa un enlace. creo un dataframe donde tengo solo las drogas y las enfermedades. 

lvls <- unique(unlist(df))  ## armo un vector que contenga de manera no repetida las drogas y las enfermedades que tengo

df[] <-  lapply(df, factor, levels=lvls)  ## hago que ambas columnas tengan los mismos factores. Esto lo hago para que cada enfermedad o droga sea un determinado numero de nodo que no se repite

# Basic Graph

g <- graph.data.frame(df, directed=F) # mi dataframe ahora es una red de igraph

g <- simplify(g) #  A graph is simple if it does not contain loop edges and multiple edges. Elimina enlaces loops(o sea del nodo, a si mismo) y enlaces entre nodos que se repitan

E(g)$weight <- 1 ## Hago que la variable weight(que tiene que ver con la fuerza del enlace entre nodos) sea 1 para todos los enlaces

g <- set.vertex.attribute(g, "degree", index = V(g), value = degree(g)) ## le genero el atributo degree a mi red de igraph. degree es The degree of a vertex is its most basic structural property, the number of its adjacent edges.

g <- set.vertex.attribute(g, "betweenness", index = V(g), value = betweenness(g)) ## le genero el atributo betweenness a mi red de igraph. betweenness es The vertex and edge betweenness are (roughly) defined by the number of geodesics (shortest paths) going through a vertex or an edge.

g <- set.vertex.attribute(g, "closeness", index = V(g), value = closeness(g)) ## le genero el atributo closeness a mi red de igraph. closeness es  Cloness centrality measures how many steps is required to access every other vertex from a given vertex.

g <- set.vertex.attribute(g, "evcent", index = V(g), value = evcent(g)$vector) ## le genero el atributo evcent a mi red de igraph. evcent es   Eigenvector centrality scores correspond to the values of the first eigenvector of the graph adjacency matrix; these scores may, in turn, be interpreted as arising from a reciprocal process in which the centrality of each actor is proportional to the sum of the centralities of those actors to whom he or she is connected. In general, vertices with high eigenvector centralities are those which are connected to many other vertices which are, in turn, connected to many others (and so on).

g <- set.vertex.attribute(g, "constraint", index = V(g), value = constraint(g))## le genero el atributo constraint a mi red de igraph. constraint es   constraint calculates Burt's constraint for each vertex.

g <- set.vertex.attribute(g, "type", index = V(g), value = group)## le genero el atributo group a mi red de igraph. group es un atributo que me dice si el nodo es una enfermedad o una droga 

# define color and shape mappings.

col <- ifelse( test = V(g)$type == "drug_name", yes = "steelblue" , no = "orange" )

shape <- ifelse( test = V(g)$type == "drug_name", yes = "circle" , no = "square" )


## hago el grafico de mi red per se. tengo diferentes formatos que puedo modificar

plot(g, ## el elemento de igraph a graficar
     
     edge.arrow.size=.2, ## Arrow size, defaults to 1
    
     edge.curved=0, ## Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
     
     vertex.color= col , ## Node color
     
     vertex.shape = shape , ## One of "none", "circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie", "raster", or "sphere"
     
     vertex.size = 5 , ## Size of the node (default is 15)
     
     vertex.frame.color="#555555", ## Node border color
     
     vertex.label=V(g)$name,  ## Character vector used to label the nodes
     
     vertex.label.color="black", ## color of the label
     
     vertex.label.cex=0.3)  ## Font size (multiplication factor, device-dependent)



### ANALISIS DESCRIPTIVO DE LA RED 



vcount(g)
ecount(g)

g.components <- clusters(g)

# which is the largest component
ix <- which.max(g.components$csize)

# get the subgraph correspondent to just the giant component
g.giant <- induced.subgraph(g, which(g.components$membership == ix))

graph.density(g)

average.path.length(g)

centralization.betweenness(g)$centralization

centralization.degree(g)$centralization

com <- edge.betweenness.community(g)

V(g)$memb <- com$membership

modularity(com)

plot(com, g)
