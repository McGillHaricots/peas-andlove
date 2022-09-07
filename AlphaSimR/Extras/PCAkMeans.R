## load in data and packages##


library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

geno <- read.csv("simulationGenotypes.csv")

##remove zero var columns##

newgeno <- geno %>%  select(where(~ n_distinct(.) > 1))

##remove colnames and nownames##
colnames(newgeno) =NULL


##use PCA function##

PCAgeno <- prcomp(newgeno, center=TRUE, scale=TRUE) ##take out categorical columns##

##look at PCs
summary(PCAgeno)

##select 3 features with highest eigenvalues
PCAselected = as.data.frame(-PCAgeno$x[,1:3])

##visualize to determine value of K##
##high silhouette width means good clustering##

fviz_nbclust(PCAselected, kmeans, method = 'silhouette')

##apply k means clustering##

k=9
kmeans_geno = kmeans(PCAselected, centers = k, nstart = 50)
clusters <- fviz_cluster(kmeans_geno, data = PCAselected)
clusters ##to view graph##

clusterData <- clusters$data

clusterData <- clusterData[order(clusterData$cluster),]

cluster1 <- clusterData[clusterData$cluster==1,]
cluster2 <- clusterDate[clusterDate$cluster==2,]
cluster3 <- clusterDate[clusterDate$cluster==3,]
cluster4 <- clusterDate[clusterDate$cluster==4,]
cluster5 <- clusterDate[clusterDate$cluster==5,]
cluster6 <- clusterDate[clusterDate$cluster==6,]
cluster7 <- clusterDate[clusterDate$cluster==7,]
cluster8 <- clusterDate[clusterDate$cluster==8,]
cluster9 <- clusterDate[clusterDate$cluster==9,]

trn1 <- cluster1[sample(0.3*nrow(cluster1)),]
trn2 <- cluster2[sample(0.3*nrow(cluster2)),]
trn3 <- cluster3[sample(0.3*nrow(cluster3)),]
trn4 <- cluster4[sample(0.3*nrow(cluster4)),]
trn5 <- cluster5[sample(0.3*nrow(cluster5)),]
trn6 <- cluster6[sample(0.3*nrow(cluster6)),]
trn7 <- cluster7[sample(0.3*nrow(cluster7)),]
trn8 <- cluster8[sample(0.3*nrow(cluster8)),]
trn9 <- cluster9[sample(0.3*nrow(cluster9)),]
TRN <- rbind(trn1, trn2,trn3,trn4,trn5,trn6,trn7,trn8,trn9)

write.csv(TRN, "TRNpca.csv")
TRN <- (read.csv("TRNpca.csv"))
TRNset <- TRN[,1]

optimTRN <- geno[TRNset,]
