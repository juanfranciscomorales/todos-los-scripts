

setwd("D:/Dropbox/R/Framework/grafico clustering")

library(data.table) ## cargo este paquete para leer rapido los archivos

dataset <- "para.pachi.csv" ## nombre del archivo con mis datos

df <- as.data.frame(fread(input = dataset, check.names = TRUE)) #leo el archivo con los descriptores

df <- df[ , - c(1,3,6,9,12,17,20,21,22,23)] ## elimino las columas que no voy a usar para hacer el clustering

colnames(df) ## nombres de las columnas para verificar si estan bien los datos que quiero

df_clear <- na.omit(df) ## saco las filas que tienen NA

df_scale <- as.data.frame(scale(df_clear[,-1])) ## escalo los datos

fviz_pca_ind(prcomp(df_scale), title = "PCA - Full dataset", ## grafico de PCA con todos los datos
             habillage = df_clear$INDICATION_CLASS, 
             #palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

df_mean <- aggregate(df_scale, list(df_clear$INDICATION_CLASS), mean , na.rm = TRUE) ## calculo la media por grupo terapeutico

fviz_pca_ind(prcomp(df_mean[,-1]), title = "PCA - Means", ## grafico de PCA con las medias de los grupos terapeuticos
             habillage = df_mean$Group.1, 
             #palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

row.names(df_mean) <- df_mean$Group.1 ## hago que la tabla con las medias por grupo terapeutico tengan nombre por fila

dist.eucl <- dist(df_mean[,-1], method = "euclidean")  ## calculo la matirx con las distancias euclideas entre las diferentes filas

library("factoextra")

dist.cor <- get_dist(df_mean[,-1], method = "pearson") ## calculo la matrix con las distancias de correlacion de pearson entre las diferesntes filas

dist.spearman <- get_dist(df_mean[,-1], method = "spearman") ## calculo la matrix con las distancias de correlacion de spearman entre las diferesntes filas

fviz_dist(dist.eucl) ## heatmap calculado con las distancias euclideas

fviz_dist(dist.cor) ## heatmap calculado con las distancias de correlacion de pearson

fviz_dist(dist.spearman) ## heatmap calculado con las distancias de correlacion de spearman





##### Kmeans usando las medias (promedios)   ######





fviz_nbclust(df_mean[,-1], kmeans, method = "wss") ## convenient solution to estimate the optimal number of clusters.

set.seed(123)  ## seteo la semilla para poder hacer el kmeans

km.res <- kmeans(x = df_mean[,-1], centers =  4, nstart = 1000 , iter.max = 100) ## calculo kmeans con k = 4 (el optimo por el paso anterior)

print(km.res) ## imprimo resultado de kmeans

fviz_cluster(km.res, data = df_mean[,-1],   ## grafico el resultado del kmeans en los componentes principales
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


########      Clustering por el metodo de PAM         ###########



library(cluster)
 
fviz_nbclust(df_mean[,-1], pam, method = "silhouette") + theme_classic() ## encuentro el valor optimo de numero de cluster segun PAM

pam.res <- pam(df_mean[,-1], 2 ) ## hago el clustering por PAM

print(pam.res) ## imprimo los resultados del clustering de PAM

fviz_cluster(pam.res,    ## grafico el resultado del clustering por PAM en los componentes principales
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


###########################################

#               Dendrogramas              #

############################################


res.hc <- hclust(d = dist.eucl, method = "ward.D2") ## dendrograma usando distancia euclidia y metodo de ward.D2 para hacer el encadenamiento

fviz_dend(res.hc, cex = 0.5) ### dendrograma basico

# Compute cophentic distance

res.coph <- cophenetic(res.hc)  

# Correlation between cophenetic distance and
# the original distance

cor(dist.eucl, res.coph) ## este valor me dice cuanto correlaciona mi dendrograma a los datos originales. Mientras mas cercano a 1 mejor




res.hc <- hclust(d = dist.eucl, method = "average") ## dendrograma usando distancia euclidia y metodo de average para hacer el encadenamiento

fviz_dend(res.hc, cex = 0.5)

# Compute cophentic distance

res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and
# the original distance

cor(dist.eucl, res.coph)  ## este valor me dice cuanto correlaciona mi dendrograma a los datos originales. Mientras mas cercano a 1 mejor


grp <- cutree(res.hc, k = 4)  ## hago que a mi dendrograma me lo divida en 4 clusters

fviz_dend(res.hc, k = 4,         ### Grafico el dendrograma donde se visualizan bien los clusters que se forman
          cex = 0.5, # label size
          #k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          horiz = TRUE,
          rect = TRUE # Add rectangle around groups
)

fviz_dend(res.hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco", ### Grafico el dendrograma donde se visualizan bien los clusters que se forman
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)








######## dendrogramas que tienen formatos diferentes, como circulares o redes ###########


fviz_dend(res.hc, cex = 0.5, k = 5,   ### Grafico el dendrograma donde se visualizan bien los clusters que se forman. Es un dendrograma circular
          k_colors = "jco", type = "circular")

require("igraph")

fviz_dend(res.hc, k = 5, k_colors = "jco",    ##### Grafico de dendrograma en forma de red
          type = "phylogenic", repel = TRUE , phylo_layout = "layout.gem")


##########################################################################################







fviz_cluster(list(data = df_mean[,-1], cluster = grp),   ### grafico los clusters formados en el dendrograma pero en el plano 2D de las componentes principales y con las elipses convexas
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())





library(cluster)

# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = df_mean[,-1], # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
# DIvisive ANAlysis Clustering
res.diana <- diana(x = df_mean[,-1], # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)

fviz_dend(res.agnes, cex = 0.6, k = 5)  #### dendrograma hecho por agnes

fviz_dend(res.diana, cex = 0.6, k = 5) ## me tira error, no se bien porque


####  COMPARING DENDOGRAMS  #######

library(dendextend)

# Compute 2 hierarchical clusterings
hc1 <- hclust(dist.eucl, method = "average")
hc2 <- hclust(dist.eucl, method = "ward.D2")
# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

##  ¿¿¿¿¿¿ quiero bajo entanglement (entrelazado) ??????????????????



# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2) ## tiene que ser cercano a -1 o 1. Si da cerca de cero significa que son diferentes los dendrogramas

# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)## tiene que ser cercano a -1 o 1. Si da cerca de cero significa que son diferentes los dendrogramas

# Create multiple dendrograms by chaining
dend1 <- df_mean[,-1] %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df_mean[,-1] %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df_mean[,-1] %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- df_mean[,-1] %>% dist %>% hclust("centroid") %>% as.dendrogram
dend5 <- df_mean[,-1] %>% dist %>% hclust("ward.D2") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4, "ward.D2" = dend5)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

library(corrplot)
corrplot(cors, "pie", "lower")


## Verificacion si hay tendencia en los datos o sin son aleatorios.

library(clustertend)

# Compute Hopkins statistic. Si es menor a 0.5, significa que hay tendencia en los datos
set.seed(123)

hopkins(df_mean[,-1], n = nrow(df_mean) -1)

fviz_pca_ind(prcomp(df_scale), title = "PCA - Full dataset",
             habillage = df_clear$INDICATION_CLASS, 
             #palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

fviz_pca_ind(prcomp(df_mean[,-1]), title = "PCA - Means",
             habillage = df_mean$Group.1, 
             #palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

library(FactoMineR)

df_scale$INDICATION_CLASS <- df_clear$INDICATION_CLASS

data.pca <- PCA(df_scale, quali.sup=18)

plot(data.pca, habillage = 18, 
     #col.hab = c("green", "blue", "red"), 
     ellipse = TRUE ,
     title = "Dataset projected onto PC1-2 Subspace")

fviz_dist(dist(df_mean[,-1]), show_labels = FALSE) + labs(title = "Datos de los Promedios")  ## hago un heatmap para ver si observo tendencia en los datos

fviz_dist(dist(df_scale), show_labels = FALSE) + labs(title = "Dataset entero")  ## hago un heatmap para ver si observo tendencia en los datos


####  DETERMINACION DEL NUMERO OPTIMO DE CLUSTERS ###########

library("NbClust")

nb <- NbClust(df_mean[,-1], distance = "euclidean", min.nc = 2,  #### determinacion de numero de cluster optimos para hierarchical clustering
              max.nc = 10, method = "ward.D2")

fviz_nbclust(nb)

nb <- NbClust(df_mean[,-1], distance = "euclidean", min.nc = 2,  #### determinacion de numero de cluster optimos para kmeans
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

########## CLUSTER VALIDATION STATISTICS  ######################

# Hierarchical clustering
hc.res <- eclust(df_mean[,-1], "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)

fviz_silhouette(hc.res, palette = "jco",  ## calculo el Si. Tienen que ser valores cercanos a 1, si es asi esta bien el clustering
                ggtheme = theme_classic())

 #Observations with a large Si (almost 1) are very well clustered.
 #A small Si (around 0) means that the observation lies between two clusters.
 #Observations with a negative Si are probably placed in the wrong cluster



# Statistics for Hierarchical clustering
hc_stats <- cluster.stats(dist(df_mean[,-1]), hc.res$cluster)
# Dunn index
hc_stats$dunn ### mientras mas grande, mejor

hc_stats





###### CHOOSING THE BEST CLUSTER ALGORITHM ####################



library(clValid)

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")

validation <- c("internal", "stability")

intern <- clValid(df_mean[,-1], nClust = 2:10,
                  clMethods = clmethods, validation = validation , metric = "euclidean", method = "ward")
# Summary
summary(intern)  ### me da un resumen de cual seria el mejor algoritmo para hacer el clustering segun diferentes mediciones




########## COMPUTE P-VALUE FOR HIERARCHICAL CLUSTERING #################

library(pvclust)

set.seed(123)

res.pv <- pvclust(df_mean[,-1], method.dist="cor",
                  method.hclust="ward.D2", nboot = 1000)

# Default plot
plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv) ## Clusters with AU > = 95% are indicated by the rectangles and are considered to be strongly supported by data.





### Analisis de variables e individuos por los componentes Principales ###############



res.pca <- prcomp(df_mean[,-1], scale = TRUE)  ## hago el componente principal per se

fviz_screeplot(res.pca, ncp=10) ## gráfico de porcentaje de explicacion de varianza por cada componente principal

fviz_pca_var(res.pca, col.var="contrib") + scale_color_gradient2(low="white", mid="blue", high="red") + theme_minimal() ## gráfico las variables en el PCA junto con su contribución

fviz_pca_ind(res.pca) ## gráficos los individuos en el PCA

fviz_pca_ind(res.pca, col.ind="cos2")  + scale_color_gradient2(low="white", mid="blue", high="red") ## gráfico los individuos en el PCA junto con su valor de coseno cuadrado(relacionado con cuanto explica el PCA a ese punto)

fviz_pca_biplot(res.pca,  geom = "text") + theme_minimal() ## gráfico de variables e individuos en el mismo PCA


