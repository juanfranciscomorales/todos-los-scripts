
is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no

if ( is.installed("XGR") == FALSE) {
        
        source("http://bioconductor.org/biocLite.R")
        
        biocLite("XGR")} #si pROC no está instalado hago que me lo instale automaticamente


library(XGR) ## el paquete que tiene la funcion xRDataLoader()

ChEMBL <- xRDataLoader(RData='ChEMBL') ## con esto cargo chembl a R en un dataframe
colnames(ChEMBL) ## los nombres de las columnas del dataframe con la info de chembl
dim(ChEMBL)
View(ChEMBL) ## con esto abro el dataframe de chembl en una pestaña para poder visualizarlo mejor
ChEMBL[1:5, ] ## veo las primeras 5 filas

# find drugs used to treat Ankylosing Spondylitis (AS)
subset(ChEMBL, code=="AS") ## con esto filtro y armo un subconjunto




if ( is.installed("Rcpi") == FALSE) {
        
        source("http://bioconductor.org/biocLite.R")
        
        biocLite("Rcpi")} #si pROC no está instalado hago que me lo instale automaticamente

#Rcpi: Molecular Informatics Toolkit for Compound-Protein Interaction in Drug Discovery
library(Rcpi) ## el paquete que tiene la funcion getDrug()

id.vector <- 'CHEMBL1430' # el vector que tiene los ID de los compuestos que quiero de chembl. el ID es el que usa chembl

getDrug(id = id.vector, from = "chembl" , type = "smile" , parallel = 5) ## extraigo las estructuras de las moleculas que le pedi 

getDrug(id = id.vector, from = "chembl" , type = "mol" , parallel = 5) ## extraigo las estructuras de las moleculas que le pedi 

##########################
#########################
#########################



DrugBank <- xRDataLoader(RData='DrugBank')## con esto cargo drugbank a R en un dataframe
colnames(DrugBank)## los nombres de las columnas del dataframe con la info de drugbank
View(DrugBank)## con esto abro el dataframe de drugbank en una pestaña para poder visualizarlo mejor
# linking drugs to targets
DrugBank[1:5, 1:3]
# info about genes
DrugBank[1:5, 4:6]
# drug types
table(DrugBank$drug_type)
# drug groups
table(DrugBank$groups)
# find drugs used to treat Ankylosing Spondylitis (AS)
ind <- grep('ankylosing',DrugBank$indication, ignore.case=T, perl=T)
unique(DrugBank$drug_id[ind])
# find approved or experimental drugs used to treat AS
ind1 <- grep('ankylosing',DrugBank$indication, ignore.case=T, perl=T)
ind2 <- grep('approved|experimental',DrugBank$groups, ignore.case=T, perl=T)
ind <- intersect(ind1, ind2)
unique(DrugBank$drug_id[ind])
## also find AS approved or experimental drug target genes
unique(DrugBank[ind, c("GeneID","Symbol","description")])




#########################

if ( is.installed("devtools") == FALSE) {
        
        library(devtools)
        
        devtools::install_bitbucket("larsss/cheminf", subdir = "cheminf/")} #si pROC no está instalado hago que me lo instale automaticamente


library(cheminf) 

help(package=cheminf) ## me la lista de la ayuda de todas las funciones del paquete cheminf

lcheminf() # me genera la lista de todas las funciones disponibles en el paquete cheminf

id.vector <- 'CHEMBL1430' # el vector que tiene los ID de los compuestos que quiero de chembl. el ID es el que usa chembl

report <- chembl.report( id.vector) ## me genera un reporte con todos los datos del compuesto con el ID de chembl que le di

write.chembl.report(act = report, file = "chembl-report.pdf", verbose = FALSE) ## me genera un pdf que se guarda en la carpeta de trabajo
