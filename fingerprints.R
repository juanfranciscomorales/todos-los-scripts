install.packages("fingerprint")

install.packages("rcdk")

library(fingerprint)

library(rcdk)

library(data.table)

setwd("D:/Dropbox/DOCTORADO/Kpuu/ACTUALIZACION 2017/estructuras de las moleculas/dataset microdialisis-slice")

matrix.name <- "todaspachoOK.txt"

df.similaridad <- as.data.frame(fread(input=matrix.name ))

matrix.similaridad<-as.matrix(df.similaridad[,-1])

rownames(matrix.similaridad)<-df.similaridad[,1]

View(matrix.similaridad)

library(gplots)

mypalete <- colorpanel(n = 1000, low ="green" , mid="black", high = "red")


heatmap.2(x = matrix.similaridad,
          denscol="yellow",
          dendrogram = "none",
          scale = "none" ,
          trace = "none",
          col = mypalete,
          labRow = FALSE,
          labCol = FALSE,
          main = NULL
         
)






mols.activas <- load.molecules(molfile ="dataset activas.sdf") ### leo las moleculas activas

mols.inactivas <- load.molecules(molfile ="dataset inactivas.sdf") ## leo las moleculas inactivas

fps.activas <- lapply(mols.activas, get.fingerprint, type='circular') ## calculo el fingerprint ECFP_6 para las activas

fps.inactivas <- lapply(mols.inactivas, get.fingerprint, type='circular') ## calculo el fingerprint ECFP_6 para las inactivas

fp.sim <- fingerprint::fp.sim.matrix(c(fps.activas,fps.inactivas), method='tanimoto') ## calculo la matrix de similaridad de los ECFP6, usando el metodo de tanimoto

fp.disim <- (fp.sim -1)*c(-1) # hago que la matriz se vuelva de disimilaridad, mientras mas cercano a 1, mas disimil

dim(fp.disim) ## me da las dimensiones de la matrix de 

View(fp.disim)

library(gplots)

mypalete <- colorpanel(n = 1000, low ="green" , mid="black", high = "red")



heatmap.2(x = fp.disim,
          dendrogram = "none",
          scale = "none" ,
          trace = "none",
          col = mypalete,
          labRow = FALSE,
          labCol = FALSE
          
          )






###############################
################################
#################################


fp.inactivos <- fp.read(f='ECFP4 inactivos.csv', size=1024, lf=cdk.lf, header=TRUE, binary=TRUE)

fp.activos <- fp.read(f='ECFP4 activos.csv', size=1024, lf=ecfp.lf, header=TRUE, binary=TRUE)

fp.sim.matrix(fplist = fp.activos, fplist2 = fp.inactivos)


        