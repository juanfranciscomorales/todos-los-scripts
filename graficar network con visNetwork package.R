
######################################################

### grafico interactivo de la red con VisNetwork #####

######################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("visNetwork") == FALSE) {install.packages("visNetwork")} #si openxlsx no est? instalado hago que me lo instale automaticamente

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(visNetwork) ## Network visualization using vis.js library.

library(data.table) ## cargo este paquete para leer rapido los archivos

set.seed(1)

df <- as.data.frame(fread(input = "repoDB.csv", check.names = TRUE)) #leo el archivo con mis descriptores del training set

df <- df[df$status=="Approved",] ## extraigo solo los farmacos que tienen usos aprobados

edges <- data.frame(from = df[,"drug_name"], to = df[,"ind_name"]) ## Cada fila representa un enlace. creo un dataframe donde tengo solo las drogas y las enfermedades. 

df_names <- data.frame(names = c(df[,"drug_name"], df[,"ind_name"])) ## armo un data frame de una sola columna para poder hacer que todos los nodos tengan un numero relacionado a traves del factor

df_names <- unique(df_names) ## elimino los nombres que se repiten

nodes <- data.frame(id = df_names$names , label = df_names$names) ## armo un data frame de una sola columna para poder hacer que todos los nodos tengan un numero relacionado a traves del factor

drug_name <- rep("drug_name" , length(unique(df[,"drug_name"]))) ## armo un vector que contenga drug_name repetido la cantidad de farmacos que tengo

ind_name <- rep("ind_name" , length(unique(df[,"ind_name"]))) ### armo un vector que contenga ind_name repetido la cantidad de enfermedades que tengo        

group <- c(drug_name,ind_name) ## creo el vector group que tiene ordenado de manera tal para diferenciar cuales son drogas y cuales son enfermedades

nodes$group <- group ## agrego a la tabla con la info de los nodos, la columna que me dice a que grupo pertenece(droga o enfermedad)

shape <- ifelse(group == "drug_name" ,yes = "circle" , no = "square") ## genero un vector con las formas de los nodos. Hago que si es un farmaco tiene que tener una forma de circulo y si es una enfermedad que sea cuadrado

nodes$shape <- shape # agrego a la tabla con la info de los nodos, la columna que me dice que forma tiene cada nodo( la forma depende de a que grupo pertenece)

## Realizo el gráfico de la red en si

visNetwork(nodes = nodes, # data.frame or a list with nodes informations. Needed at least column "id".
           
           edges = edges #  data.frame or a list with edges informations. Needed at least columns "from" and "to".
           
           ) 
