
M <- TrainingGeno
y <- TrainingPheno

trainIndex <- as.matrix(sample(1:nInd(TrainingPop), 0.8*(nrow(M)))) 

phenoTrain <- y[trainIndex,]
genoTrain <- M[trainIndex,]

BV <- phenoTrain

EBVans <-mixed.solve(BV, Z=genoTrain, K=NULL, SE=FALSE, return.Hinv=FALSE)
markerEffects <- EBVans$u
markerEffects <- as.vector(markerEffects)
