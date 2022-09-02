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

##remove colnames and nownames##
colnames(newgeno) =NULL
rownames(newgeno)=NULL

##remove zero var columns##

newgeno <- geno %>%  select(where(~ n_distinct(.) > 1))

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
fviz_cluster(kmeans_geno, data = PCAselected)


##plot##
ggbiplot(PCA) #will plot by PCA1 and PCA2

##plot and color by a variable##
ggbiplot(PCA, groups=srdata$CLASS)

##plot and show ellipses##
ggbiplot(PCA, ellipse=TRUE, groups=srdata$CLASS)
