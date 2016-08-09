dput(x=lista.modelos, file = "lista de modelos glm Chisq test 13-05-16.R") #funcion para guardar los elementos de R

write.xlsx(x= tabla.AUC.ordenadas2, file= "ROC-AUC glm 13-05-16.xlsx", colNames= TRUE, rowNames=TRUE, keepNA=TRUE) # funcion para guardar los data frames como archivos de excel

cv.glm(data = lista.conjuntos2[[1]], glmfit = lista.modelos[[1]])

for (i 1:length(lista.modelos){ # para calcular el % de bien clasificados dentro del mismo training, yo le doy el punto de corte que yo quiero, obtengo valores similares que con la funcion de optimizacion de punto de corte
     hitmiss(obj = lista.modelos[[1]],k= puntos.corte.ROC.glm[1,"cutoff"])
}