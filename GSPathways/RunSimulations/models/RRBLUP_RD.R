


phenoTrain <- as.matrix(Y)
genoTrain <- as.matrix(M)

EBVans <-mixed.solve(phenoTrain, Z=genoTrain, K=NULL, SE=FALSE, return.Hinv=FALSE)
markerEffects <- EBVans$u
markerEffects <- as.vector(markerEffects)

cli_alert_success("Calculated marker effects at {args$trainGen} using {args$trainingData} data")