lista.drugbank <- c("drug bank 1.xlsx","drug bank 2.xlsx","drug bank 3.xlsx","drug bank 4.xlsx","drug bank 5.xlsx","drug bank 6.xlsx","drug bank 7.xlsx","drug bank 8.xlsx","drug bank 9.xlsx","drug bank 10.xlsx","drug bank 11.xlsx","drug bank 12.xlsx","drug bank 13.xlsx","drug bank 14.xlsx")

lista.base.datos <- list()

for ( i in 1:14) {
        
        lista.base.datos[[i]] <- clasificaciones.base.datos.ensemble.minimo.lm(base.datos  = lista.drugbank[i],cant.modelos = 50, x = tabla.AUC.ordenadas)  
}

tabla.scores.drugbank <- ldply(lista.base.datos, data.frame)

write.xlsx(x= tabla.scores.drugbank, file= "tabla scores drugbank.xlsx", colNames= TRUE, rowNames=TRUE, keepNA=TRUE) # funcion para g

