library(devtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

rm(TrainingPheno)
rm(TrainingGeno)
rm(y)
rm(M)
rm(markerEffects)


TrainingPheno <- pheno(F5)
TrainingGeno <- pullSegSiteGeno(F5)

y <- as.data.frame(TrainingPheno)
M <- as.data.frame(TrainingGeno)

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

nclusters <- as.numeric(clusterData[as.numeric(nrow(clusterData)),as.numeric(ncol(clusterData))])

i = 1
datalist = vector("list", length = nclusters)
for (i in 1:nclusters) {
  clustername <- paste0("cluster",i)
  clustername <- clusterData[clusterData$cluster==i,] 
  
  assign(paste0("cluster",i), clustername)
  
  trnname <- paste0("trn",i)
  trnname <- clustername[sample(0.3*nrow(clustername)),]
  datalist[[i]] <- trnname

  i = i + 1
  if (i > nclusters){ 
    break
  }
  }
  
TRN <- do.call(rbind, datalist)

TRN <- TRN[,1]

M <- as.data.frame(TrainingGeno)
OptimGeno <- M[TRN,]
y <- as.data.frame(y)
OptimPheno <- y[TRN,]

BV <- OptimPheno

EBVans <-mixed.solve(BV, Z=OptimGeno, K=NULL, X=NULL, SE=FALSE, return.Hinv=FALSE)

markerEffects2 <- as.matrix(EBVans$u)
markerEffects2 <- as.vector(markerEffects2)
