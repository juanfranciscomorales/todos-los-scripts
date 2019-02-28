setwd("C:/Users/Administrador/Desktop")

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(data.table) ## cargo este paquete para leer rapido los archivos



exp.gen  <- "expresion genica.csv"  ### nombre del archivo con el training set

ic50 <- "v17.3_fitted_dose_response.csv"  ### nombre del archivo con el test set



df.exp.gen <- as.data.frame(fread(input = exp.gen , check.names = FALSE , header = TRUE)) #leo el archivo con mis descriptores del training set

df_simple <- df.exp.gen[df.exp.gen$ensembl_gene == "ENSG00000118596",]

dim(df_simple)

View(df_simple)

ENSG00000118596 <- data.frame(matrix(nrow=1018, ncol=2))

colnames(ENSG00000118596) <- c("COSMIC_ID" , "Expresion")

ENSG00000118596$COSMIC_ID <- colnames(df_simple)[-1]

ENSG00000118596$Expresion <- as.numeric(as.vector(df_simple[1,]))[-1]

head(ENSG00000118596)

str(ENSG00000118596)



df.ic50 <- as.data.frame(fread(input = ic50 , check.names = FALSE , header = TRUE)) #leo el archivo con mis descriptores del training set

df.ic50.simple <- df.ic50[df.ic50$DRUG_NAME == "DMOG" , ]

dim(df.ic50.simple)

head(df.ic50.simple)

df.ic50.simple <- merge(df.ic50.simple , ENSG00000118596 , by = "COSMIC_ID" , all.x = TRUE)

dim(df.ic50.simple)

library(ggplot2)

grafico <- ggplot () + 
        
        geom_point(data = df.ic50.simple , aes(x = Expresion, y = LN_IC50)  , color = "blue4"  ,size = 3.5 ,  alpha = 0.3) +
        
        theme_classic()

grafico        
