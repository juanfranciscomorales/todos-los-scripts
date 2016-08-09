set.seed(125)

library(openxlsx)

df <- read.xlsx(xlsxFile="descriptores IC 50 t cruzi amastigote 3D (hubo una no calculada).xlsx", check.names = TRUE) #leo el archivo con mis descriptores

library(caret)

intrain <- createDataPartition(y = df$clase, p = 0.7, list=FALSE) ## de manera aleatoria armo mi training y test set.

training <- df[intrain,]

testing <- df[-intrain,]

write.xlsx(x= training, file= "training set.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar el training set que armé

write.xlsx(x= testing, file= "test set.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar el test set que armé
