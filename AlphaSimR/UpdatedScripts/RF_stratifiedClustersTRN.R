

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

cluster1 <- clusterData[clusterData$cluster==1,]
cluster2 <- clusterData[clusterData$cluster==2,]
cluster3 <- clusterData[clusterData$cluster==3,]
cluster4 <- clusterData[clusterData$cluster==4,]
cluster5 <- clusterData[clusterData$cluster==5,]
cluster6 <- clusterData[clusterData$cluster==6,]
cluster7 <- clusterData[clusterData$cluster==7,]
cluster8 <- clusterData[clusterData$cluster==8,]
cluster9 <- clusterData[clusterData$cluster==9,]
cluster10 <- clusterData[clusterData$cluster==10,]

trn1 <- cluster1[sample(0.9*nrow(cluster1)),]
trn2 <- cluster2[sample(0.9*nrow(cluster2)),]
trn3 <- cluster3[sample(0.9*nrow(cluster3)),]
trn4 <- cluster4[sample(0.9*nrow(cluster4)),]
trn5 <- cluster5[sample(0.9*nrow(cluster5)),]
trn6 <- cluster6[sample(0.9*nrow(cluster6)),]
trn7 <- cluster7[sample(0.9*nrow(cluster7)),]
trn8 <- cluster8[sample(0.9*nrow(cluster8)),]
trn9 <- cluster9[sample(0.9*nrow(cluster9)),]
trn10 <- cluster9[sample(0.9*nrow(cluster10)),]
TRN <- rbind(trn1, trn2,trn3,trn4,trn5,trn6,trn7,trn8,trn9, trn10)
TRN <- TRN[,1]

M <- as.data.frmae(TrainingGeno)
OptimGeno <- M[TRN,]

y <- as.data.frame(TrainingPheno)
OptimPheno <- as.data.frame(y[TRN,])


Training <- cbind(OptimPheno, OptimGeno)
colnames(Training) <- paste("ID",1:ncol(Training), sep="") ##1-605 because the SNP chip has 605 SNPs + phenotypes may have to change if you have a different num. SNPS###
##note ID1 will be the phenotype, IDs 2-606 are genotypes##


## create cross validation strategy ##
control <- trainControl(method='repeatedcv', 
                        number=10, ##will test 10 different values for mtry (number of variables for splitting) ##
                        repeats=3,
                        search = "random")        

##build model##
na.omit(Training)
rf_fit = train(ID1 ~ ., 
               data = Training, 
               method = "rf",
               tuneLength= 10,
               trControl=control) ## search a random tuning grid ##


### This command takes about 90 minutes in an compute canada interactive session ###

## look at the parameters of the model ##
print(rf) 
