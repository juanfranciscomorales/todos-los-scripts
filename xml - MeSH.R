
###########################

## EL QUE FUNCIONA PARA EXTRAER INFO ##

############################

setwd("D:/MEGA")

library(XML)

mesh <- xmlTreeParse(file = "desc2018.xml" , useInternalNodes = TRUE)

rootnode <- xmlRoot(mesh)

rootsize <- xmlSize(rootnode)

xmlName(rootnode) ## nombre del nodo

#names(rootnode) ## nombre de los subnodos

xmlName(rootnode[[1]]) ## nombre del nodo

names(rootnode[[1]]) ## nombres de los subnodos




TreeNumbers.vector <- vector()

for (i in 1:rootsize) {
        
        if (length(rootnode[[i]][["TreeNumberList"]]) == 0 ) {
                
                TreeNumbers.vector [i] <- NA
                
        }
        
        else {
                
        drug <- paste(xmlToList(rootnode[[i]][["TreeNumberList"]]), collapse = " - ") ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        TreeNumbers.vector [i] <- drug 
        
        }
        
}

length(TreeNumbers.vector)




MeSHID.vector <- vector()

for (i in 1:rootsize) {
        
        drug <- xmlValue(rootnode[[i]][["DescriptorUI"]]) ## extraigo solo el subnodo categories que esta compuesto a su vez por 2 subnodos category y mesh.id
        
        MeSHID.vector[i] <- drug 
        
}

length(MeSHID.vector)




df.completo <- as.data.frame(cbind( MeSH_ID = MeSHID.vector , Tree_Numbers = TreeNumbers.vector))

View(df.completo)


library(data.table)

fwrite(x = df.completo, file = "MeSH.csv")

#library(openxlsx)

#write.xlsx(x= df.completo, file= "MeSH.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado


##########
##########
##########
##########

prueba <- unlist(strsplit( x = TreeNumbers.vector[2] , split = " - "))

substr(  prueba , start = 1 , stop = 3 )