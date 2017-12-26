
setwd("C:/Users/Administrador/Desktop")

library(xml2)

x <- read_xml("full database.xml")

xml_name(x) ## primer nombre del xml

xml_children(x) ## me da los nombres de los nodos que siguen

