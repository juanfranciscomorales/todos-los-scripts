######################################################

# Uso IGRAPH PARA ARMAR UNA RED QUE LA GUARDO CON FORMATO GEXF

# PARA LUEGO GRAFICARLO CON GEPHI

######################################################


is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("igraph") == FALSE) {install.packages("igraph")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("rgexf") == FALSE) {install.packages("rgexf")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(igraph) ## este paquete sirve para hacer el analisis de las redes

library(data.table) ## cargo este paquete para leer rapido los archivos

library(rgexf) ## con este paquete puedo hacer que una red de igraph, la pueda guardar con un formato que puedo visualizarla con el programa Gephi 

set.seed(1)

df <- as.data.frame(fread(input = "repoDB.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

df <- df[df$status=="Approved",] ## extraigo solo los farmacos que tienen usos aprobados

df$drug_name <- gsub('<', '&lt;', df$drug_name) ##con esto cambio en la columna drgu_name los simbolos < por &lt; porque sino me tira error cuando quiero volverlo formato gexf

df$ind_name <- gsub('<', '&lt;', df$ind_name) ##con esto cambio en la columna drgu_name los simbolos < por &lt; porque sino me tira error cuando quiero volverlo formato gexf

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

g <- set.vertex.attribute(g, "group", index = V(g), value = group)## le genero el atributo group a mi red de igraph. group es un atributo que me dice si el nodo es una enfermedad o una droga 

com <- edge.betweenness.community(g) ## calculo las comunidades y las guardo en un elemento llamado com. Community structure detection based on edge betweenness. Many networks consist of modules which are densely connected themselves but sparsely connected to other modules.

g <- set.vertex.attribute(g, "community", index = V(g), value = com$membership)## le genero el atributo community a mi red de igraph. community es un atributo que me indica a que comunidad pertenece cada nodo

cluster <- clusters(g) ## Me calcucula los componentes conectados de una red (o sea si mi red esta compuesta por subredes que no estan conectadas entre si, esta funcion sabe separarmelas). Connected components of a graph. Calculate the maximal (weakly or strongly) connected components of a graph.

g <- set.vertex.attribute(g, "cluster", index = V(g), value = cluster$membership)## le genero el atributo cluster a mi red de igraph. cluster es un atributo que me indica a que grupo de nodos que estan conectados entre ellos pertenece mi nodo


##

# GUARDO LA RED IGRAPH EN FORMATO GEXF PARA PODER LEVANTARLA DESPUES CON GEPHI

##


g1.gexf <- igraph.to.gexf(g) ## Converts objects between gexf and igraph objects keeping attributes, edge weights and colors.

f <- file("repoDB - aprobados - fase 3.gexf") # You have to create a file connection.

writeLines(g1.gexf$graph, con = f) ## Write text lines to a connection.

close(f) ## close the connection.




### ESTO ES PARA EXTRAER EL SUBGRAFO DE LA COMUNIDAD 
### QUE TIENE EL NODO QUE ME INTERESA


k.name <- "Epilepsy" ## pongo el nombre del nodo DE INTERES

k.community <- com$membership[which(V(g)$name == k.name)] ## genero un vector que tiene todos los nombres de los nodos que pertenecen a la comunidad del nodo que yo quiero

k.community.graph <- induced.subgraph(graph = g , vids = (V(g)$community == k.community)) ## extraigo el subgrafo de la comunidad que tiene al nodo de interes

k.community.graph$name <- paste(k.name , "community graph") # le pongo nombre al subgrafo

k.community.graph  ## imprimo el subgrafo

plot(k.community.graph) ## grafico el subgrafo en R

##

# GUARDO LA RED IGRAPH EN FORMATO GEXF PARA PODER LEVANTARLA DESPUES CON GEPHI

##

g2.gexf <- igraph.to.gexf(k.community.graph) ## Converts objects between gexf and igraph objects keeping attributes, edge weights and colors.

f <- file("Epilepsy - repoDB - aprobados - fase 3.gexf")# You have to create a file connection.

writeLines(g2.gexf$graph, con = f)## Write text lines to a connection.

close(f)## close the connection.




##### COn lo siguiente lo que hago es armar las tablas para poder leer en gephi 

##### basandome en la pagina https://pegasusdata.com/2013/01/10/facebook-friends-network-mapping-a-gephi-tutorial/

##### ES OTRA ALTERNATIVA A LA ANTERIOR



edges <- data.frame(source = df$source.index , target = df$target.index)  ## genero la tabla con la info de los enlaces (edges)

nodes <- data.frame(Id = seq(from=0, to = (nrow(vertices)-1), by = 1) , Label = vertices$name) ## genero la tabla con la info de los nodos (nodes)


write.gexf(nodes = nodes, edges = edges , output = "repoDB.gexf") ## guardo la red que tengo en los dataframes anteriores como gexf

fwrite(x= edges , file = "edges.csv" , col.names = TRUE , sep = ";" ,  na = "" ,  dec = ".") ## guardo la tabla con la info de edges

fwrite(x= nodes , file = "nodes.csv" , col.names = TRUE , sep = ";" ,  na = "" ,  dec = ".") ## guardo la tabla con la info de nodes

## Luego a los csv anteriores tengo que leerlos con gephi 


##################################################


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

