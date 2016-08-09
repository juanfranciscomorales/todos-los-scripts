library(openxlsx)

df <- read.xlsx(xlsxFile="Dtraining.xlsx", check.names =TRUE) # leo el archivo donde tengo los datos

df2 <- df[,-c(1,3)] ## elimino las columnas que no me sirven, dejo la clase y los descriptores solamente

df2$clase<- as.factor(df2$clase) #la columna clase la hago que tenga clase factor para poder desarrollar el random forest

library(randomForest)

rf<- randomForest(formula = clase ~.,data  = df2, na.action= na.fail, importance = TRUE, ntree =1000, mtry = 200, do.trace =TRUE , replace= FALSE, nodesize=10) 



modfit<-train( x = clase ~., data = df2, method = "rf", prox = TRUE)##llama a la funcion randomForest que es la que tengo arriba

