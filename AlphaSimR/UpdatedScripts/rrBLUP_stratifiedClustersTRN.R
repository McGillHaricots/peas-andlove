library(devtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

M <- as.data.frame(TrainingGeno)
y <- as.data.frame(TrainingPheno)

newgeno <- M %>%  select(where(~ n_distinct(.) > 1))

colnames(newgeno) =NULL

PCAgeno <- prcomp(newgeno, center=TRUE, scale=TRUE) ##take out categorical columns##

PCAselected = as.data.frame(-PCAgeno$x[,1:3])

silhouette <- fviz_nbclust(PCAselected, kmeans, method = 'silhouette')
kvalues <- silhouette$data ##largest value tells how many clusters are optimal ##
kvalues <- kvalues[order(-kvalues$y),]

k=as.numeric(kvalues[1,1])

kmeans_geno = kmeans(PCAselected, centers = k, nstart = 50)
clusters <- fviz_cluster(kmeans_geno, data = PCAselected)

clusterData <- clusters$data

clusterData <- clusterData[order(clusterData$cluster),]

cluster1 <- clusterData[clusterData$cluster==1,]
cluster2 <- clusterData[clusterData$cluster==2,]
cluster3 <- clusterData[clusterData$cluster==3,]
cluster4 <- clusterData[clusterData$cluster==4,]
cluster5 <- clusterData[clusterData$cluster==5,]
cluster6 <- clusterData[clusterData$cluster==6,]
cluster7 <- clusterData[clusterData$cluster==7,]
cluster8 <- clusterData[clusterData$cluster==8,]
cluster9 <- clusterData[clusterData$cluster==9,]

trn1 <- cluster1[sample(0.8*nrow(cluster1)),]
trn2 <- cluster2[sample(0.8*nrow(cluster2)),]
trn3 <- cluster3[sample(0.8*nrow(cluster3)),]
trn4 <- cluster4[sample(0.8*nrow(cluster4)),]
trn5 <- cluster5[sample(0.8*nrow(cluster5)),]
trn6 <- cluster6[sample(0.8*nrow(cluster6)),]
trn7 <- cluster7[sample(0.8*nrow(cluster7)),]
trn8 <- cluster8[sample(0.8*nrow(cluster8)),]
trn9 <- cluster9[sample(0.8*nrow(cluster9)),]
TRN <- rbind(trn1, trn2,trn3,trn4,trn5,trn6,trn7,trn8,trn9)

TRN <- TRN[,1]

rownames(M) <- c(1:nrow(M))
OptimGeno <- M[TRN,]
y <- as.data.frame(y)
OptimPheno <- y[TRN,]

BV <- OptimPheno

EBVans <-mixed.solve(BV, Z=OptimGeno, K=NULL, X=NULL, SE=FALSE, return.Hinv=FALSE)

markerEffects <- as.matrix(EBVans$u)
markerEffects <- as.vector(markerEffects)

