
###########################

## EL QUE FUNCIONA PARA EXTRAER INFO ##

############################

setwd("~/MEGA")

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




categories <- vector()

for (i in 1:rootsize) {
        
        node.length <- length(xmlToList(rootnode[[i]][["categories"]])) ## me dice el numero de atc-codes que tiene el farmaco
        
        if (node.length == 0 ) { # si no tiene atc-code le asigo el valor NA
                
                drug <- NA
                
        } else { # si tiene atc-code hago el siguiente for loop
                
                category.list <- xmlToList(rootnode[[i]][["categories"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
                
                drug <- paste( unlist( lapply(category.list  , function (x){x[[1]]} ) , use.names = FALSE )  , collapse = " - " )
                
        }
        
        categories[i] <- drug 
        
}

length(categories)





mesh.id <- vector()

for (i in 1:rootsize) {
        
        node.length <- length(xmlToList(rootnode[[i]][["categories"]])) ## me dice el numero de atc-codes que tiene el farmaco
        
        if (node.length == 0 ) { # si no tiene atc-code le asigo el valor NA
                
                drug <- NA
                
        } else { # si tiene atc-code hago el siguiente for loop
                
                mesh.id.list <- xmlToList(rootnode[[i]][["categories"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
                
                mesh.id.list <- lapply(mesh.id.list  , function (x){x[[2]]} ) ## extraigo los valores de mesh.id
                
                mesh.id.list <- lapply(mesh.id.list, function(x) ifelse(is.null(x),  NA, x)) ## los que son NULL los vuelvo NA para que no se pierda cuando se hace unlist
                
                drug <- paste( unlist( mesh.id.list  , use.names = FALSE )  , collapse = " - " ) ## armo la frase con todos los mesh.id por compuesto
                
        }
        
        mesh.id[i] <- drug 
        
}

length(mesh.id)





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






atc.code.root <- vector()

for (i in 1:rootsize) {
        
        node.length <- length(xmlToList(rootnode[[i]][["atc-codes"]])) ## me dice el numero de atc-codes que tiene el farmaco
        
        if (node.length == 0 ) { # si no tiene atc-code le asigo el valor NA
                
                drug <- NA
        
                } else { # si tiene atc-code hago el siguiente for loop
                        
                        colect <- vector() ## creo el vector donde voy a guardar los diferentes valores de atc-code
                        
                        for(j in 1:node.length) {  # extraigo los todos los atc-codes que tiene el farmaco y los coloco en un vector
                                
       colect[j]  <- unname(xmlToList(rootnode[[i]][["atc-codes"]])[[j]][[1]]$.attrs) ## extraigo el subnodo de atc-codes como lista, lo cual me da una lista con 5 sublistas. Selecciono la sublista 1, y me quedo con el atributo de esa lista que es el atc-code raiz, al cual le hago unname para sacar el attribute y quedarme solo con el atc-code raiz
        
                        }
                        
                        drug <- paste(colect , collapse = " - ")  # hago que todos los atc codes esten en un vector con length 1     
        
        
        
                }
        
atc.code.root[i] <- drug 

}

length(atc.code.root)






atc.code.unique <- vector()

for (i in 1:rootsize) {
        
        node.length <- length(xmlToList(rootnode[[i]][["atc-codes"]])) ## me dice el numero de atc-codes que tiene el farmaco
        
        if (node.length == 0 ) { # si no tiene atc-code le asigo el valor NA
                
                drug <- NA
                
        } else { # si tiene atc-code hago el siguiente for loop
                
                colect <- vector() ## creo el vector donde voy a guardar los diferentes valores de atc-code
                
                for(j in 1:node.length) {  # extraigo los todos los atc-codes que tiene el farmaco y los coloco en un vector
                        
                        colect[j]  <- unname(xmlToList(rootnode[[i]][["atc-codes"]])[[j]][[5]]) ## extraigo el subnodo de atc-codes como lista, lo cual me da una lista con 5 sublistas. Selecciono la sublista 1, y me quedo con el atributo de esa lista que es el atc-code raiz, al cual le hago unname para sacar el attribute y quedarme solo con el atc-code raiz
                        
                }
                
                drug <- paste(colect , collapse = " - ")  # hago que todos los atc codes esten en un vector con length 1     
                
                
                
        }
        
        atc.code.unique[i] <- drug 
        
}

length(atc.code.unique)






type.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlAttrs(rootnode[[i]])[["type"]] ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        type.vector[i] <- drug 
        
}

length(type.vector)








df.completo <- cbind(name = names.vector , type = type.vector , drugbank_id = id.vector , groups = groups.vector, MW = MW.vector , synonyms = synonyms.vector , cas_number = cas.number , atc_code_unique = atc.code.unique  , atc_code_root = atc.code.root , UNII_Unique.Ingredient.Identifier = unii.vector , protein_binding = protein.binding , clearance = clearance , volume_distribution = volume_distribution , absorption = absorption , mesh_id = mesh.id , categories = categories)

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

