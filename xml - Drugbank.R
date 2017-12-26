
###########################

## EL QUE FUNCIONA PARA EXTRAER INFO ##

############################

setwd("C:/Users/Administrador/Desktop")

library(XML)

drugbank <- xmlTreeParse(file = "full database.xml" , useInternalNodes = TRUE)

rootnode <- xmlRoot(drugbank)

rootsize <- xmlSize(rootnode)

xmlName(rootnode) ## nombre del nodo

#names(rootnode) ## nombre de los subnodos

xmlName(rootnode[[1]]) ## nombre del nodo

names(rootnode[[1]]) ## nombres de los subnodos

#art <- rootnode[[1]][["categories"]] ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id

#names(art)

#names(art[[1]])

#list <- xmlToList(art)

#df <- data.frame(do.call(rbind, list))





categories.list <- list()

for (i in 1:rootsize) {
     
        drug <- rootnode[[i]][["categories"]] ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        list <- xmlToList(drug) ## convierto a lista
        
        if (is.null(list)) {
                
                list <- list()
        }
        
        df <- data.frame(do.call(rbind,list)) ## paso la info a dataframe
       
        categories.list[[i]] <- df 
}

length(categories.list)

categories.list <- lapply(categories.list , function (x) { as.data.frame(as.data.frame(t(x))[1,])}) ## traspongo y dejo solo la fila con las categorias

length(categories.list)

#categories.list <- lapply( categories.list , function(x) { if ( dim(x) == c(1,0) ) { data.frame(c(1,1)) }}) 

#length(categories.list)

#class <- vector()

#for( i in 1:length(categories.list)){
        
#        class[i] <- class(categories.list[[i]])
#}



names.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["name"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        names.vector[i] <- drug 
       
        }
        
length(names.vector)





MW.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["average-mass"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        MW.vector[i] <- drug 
        
}

length(MW.vector)

class(MW.vector) <- "numeric"





groups.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- paste(xmlToList(rootnode[[i]][["groups"]]), collapse = " - ") ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        groups.vector[i] <- drug 
        
}

length(groups.vector)





synonyms.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- paste(xmlToList(rootnode[[i]][["synonyms"]] , addAttributes = FALSE), collapse = " ; ") ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        synonyms.vector[i] <- drug 
        
}

length(synonyms.vector)





id.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][[1]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        id.vector[i] <- drug 
        
}

length(id.vector)






cas.number <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["cas-number"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        cas.number[i] <- drug 
        
}

length(cas.number)






unii.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["unii"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        unii.vector[i] <- drug 
        
}

length(unii.vector)







half.life <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["half-life"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        half.life[i] <- drug 
        
}

length(half.life)





protein.binding <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["protein-binding"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        protein.binding[i] <- drug 
        
}

length(protein.binding)







clearance <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["clearance"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        clearance[i] <- drug 
        
}

length(clearance)








volume_distribution <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["volume-of-distribution"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        volume_distribution[i] <- drug 
        
}

length(volume_distribution)






absorption <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["absorption"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        absorption[i] <- drug 
        
}

length(absorption)

### Revisar, todavia falta corregir  ####
#atc.code <- vector()

#for (i in 1:rootsize) {
        
#        drug <- xmlAttrs(rootnode[[i]])[["atc-codes"]][[1]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
#        type.vector[i] <- drug 
        
#}

#length(type.vector)
### Revisar, todavia falta corregir  ####




type.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlAttrs(rootnode[[i]])[["type"]] ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        type.vector[i] <- drug 
        
}

length(type.vector)







library(plyr)

#df <- do.call("rbind.fill", categories.list) 

df <- rbind.fill(categories.list)

df.completo <- cbind(name = names.vector , type = type.vector , drugbank_id = id.vector , groups = groups.vector, MW = MW.vector , synonyms = synonyms.vector , cas_number = cas.number , UNII_Unique.Ingredient.Identifier = unii.vector , protein_binding = protein.binding , clearance = clearance , volume_distribution = volume_distribution , absorption = absorption , df)

View(df.completo)

library(openxlsx)

write.xlsx(x= df.completo, file= "drugbank.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado







ns<-c("db"="http://www.drugbank.ca")

#getNodeSet(drugbank, "//db:drug/db:name", namespaces=ns)

xpathSApply(xmlRoot(drugbank), "//db:drug/db:categories/db:category", xmlValue, namespaces=ns)

xpathSApply( doc = rootnode, path = "//db:drug/db:name", xmlValue, namespaces=ns)



#############

setwd("C:/Users/Administrador/Desktop")

library(XML)

library(plyr)

drugbank <- xmlParse(file = "full database.xml")

#drugbanklist <- xmlToList(drugbank)

drugbank.df <- ldply(xmlToList(drugbank), data.frame)

