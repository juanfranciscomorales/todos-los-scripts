#ejemplo de como hacer analisis lineal discriminante con R

# Linear Discriminant Analysis with Jacknifed Prediction 
library(MASS)
fit <- lda(G ~ x1 + x2 + x3, data=mydata, 
           na.action="na.omit", CV=TRUE)
fit # show results

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(mydata$G, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))