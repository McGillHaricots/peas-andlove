
geno <- read.csv("simulationGenotypes.csv")

##remove colnames and nownames##
colnames(newgeno) =NULL
rownames(newgeno)=NULL

##remove zero var columns##

newgeno <- geno %>%  select(where(~ n_distinct(.) > 1))

fviz_nbclust(newgeno, kmeans, method = 'silhouette')

genoClusters<- kmeans(newgeno, 3, iter.max = 10, nstart = 1)


source: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/#accessing-to-the-results-of-kmeans-function
