
setwd("~/MEGA")

library(data.table)

chembl <- fread(input = "chembl_indications-17_18_09_26.txt")

dim(chembl)

colnames(chembl)

View(chembl)
