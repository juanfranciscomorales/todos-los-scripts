## boosting

library(openxlsx)

library(mboost)
df <- read.xlsx(xlsxFile="Dtraining.xlsx", check.names =TRUE) # leo el archivo donde tengo los datos

df2 <- df[,-c(1,3)] ## elimino las columnas que no me sirven, dejo la clase y los descriptores solamente

df2$clase<- as.factor(df2$clase) #la columna clase la hago que tenga clase factor para poder desarrollar el random forest

glmboost(formula = clase ~.,data=df2, family = binomial)

####Error in glmboost(formula = clase ~ ., data = df2, family = binomial) : argument "x" is missing, with no default

glmboost( clase ~.,df2, family = binomial)

###Error in mboost_fit(bl, response = response, weights = weights, control = control,  : trying to get slot "ngradient" from an object of a basic class ("function") with no slots


matriz<- as.matrix(x=df3,nrow= nrow(df3), ncol = ncol(df3))

formulacion <-formula(paste(" clase ~ ",paste(noquote(colnames(df2)[-1]) ,collapse = " + "))) 

glmboost(x=matriz, y = clase, family = binomial )