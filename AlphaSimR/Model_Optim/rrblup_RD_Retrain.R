
rm(TrainingPheno)
rm(TrainingGeno)
rm(y)
rm(M)
rm(markerEffects)

TrainingGeno <- pullSegSiteGeno(F5)
TrainingPheno <- pheno(F5)

M <- as.data.frame(TrainingGeno)
y <- as.data.frame(TrainingPheno)

trainIndex <- as.matrix(sample(1:nInd(F5), 0.75*(nrow(M)))) 

phenoTrain <- y[trainIndex,]
genoTrain <- M[trainIndex,]

BV <- phenoTrain

EBVans <-mixed.solve(BV, Z=genoTrain, K=NULL, SE=FALSE, return.Hinv=FALSE)
markerEffects2 <- EBVans$u
markerEffects2 <- as.vector(markerEffects2)