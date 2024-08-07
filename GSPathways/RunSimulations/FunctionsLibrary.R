# Create argument parser for command line options
# `parameters` variable needs to have list of all parameters for the parser
parseArgs <- function(){
  parser <- ArgumentParser()
  for (par in parameters)
    do.call(parser$add_argument, par)
  parser$parse_args() # Returns arguments
}

loadModelLibs <- function(){
  for (libname in modelLibs)
    suppressMessages(library(libname, character.only=TRUE))
}

# Defining trait parameters (AEG)
defineTraitAEG <- function(nQtl,mean,h2) {
  SP <<- SimParam$new(founderPop)
  SP$addTraitAEG(nQtl, mean=mean)
  SP$setVarE(h2=h2)
}

# Defining trait parameters (A)
defineTraitA <- function(nQtl,mean,h2) {
  SP <<- SimParam$new(founderPop)
  SP$addTraitA(nQtl, mean=mean)
  SP$setVarE(h2=h2)
}

# Selecting parents for the next cycle (pheno)
selectNewParents <- function(gen,nInd,criterion){
  selectInd(gen, nInd, use=criterion, top=TRUE)
}

# Within Family Selections (pheno/ebv)
TopWithinFam <- function(gen,nFam,nIndPerFam,criterion){
  TopFam <-selectFam(gen,nFam, use=criterion, top=TRUE)
  Selections <<- selectWithinFam(TopFam, nIndPerFam,use=criterion, top=TRUE)
  self(Selections)
}

# Family Selections (pheno/ebv)
TopFamily <- function(gen,nFam,criterion){
  Top = selectFam(gen,nFam, use=criterion, top=TRUE)
  self(Top)
}

#Stratified Clusters

StratClusTRN <- function(y,M) { #y= matrix of training phenotypes M= matrix training genotypes
  
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
  
  datalist = vector("list", length = nclusters)
  
  for (x in 1:nclusters) {
    clustername <- paste0("cluster",x)
    clustername <- clusterData[clusterData$cluster==x,] 
    
    assign(paste0("cluster",x), clustername)
    
    trnname <- paste0("trn",x)
    trnname <- clustername[sample(0.75*nrow(clustername)),]
    datalist[[x]] <- trnname
    
  }
  
  TRN <<- do.call(rbind, datalist)
  
  TRN <<- TRN[,1]
  
  M <- as.data.frame(M)
  rownames(M) <- c(1:nrow(M))
  OptimGeno <<- as.matrix(M[TRN,])
  y <- as.data.frame(y)
  OptimPheno <<- y[TRN,]
  
}

# gets alleles matrix of genObj
getAllelesMat <- function(genObj, genName){
    allelesMat <- pullSegSiteHaplo(genObj)
    Gen <- as.data.frame(rep(genName, times=nInd(genObj)))
    colnames(Gen) <- "Gen"
    allelesMat <- cbind(Gen, allelesMat)
    allelesMat
}

# gets dataframe with bv and ebv
getBvEbv <- function(genObj, genName){
    bvebv <- cbind(bv(genObj), ebv(genObj))
    Gen <- as.data.frame(rep(genName, times=nInd(genObj)))
    bvebv <- cbind(Gen, bvebv)
    colnames(bvebv) <- c("Gen","bv","ebv")
    bvebv
}

## Functions to build dataframes

getAllGeneticValues <- function(geneticValues){
  geneticValues <- as.data.frame(geneticValues)
  colnames(geneticValues) <- 1:args$nReps
  rownames(geneticValues) <- c("PrevCycPYT","NewParents","F1","F2","F3","F4","F5","PYT","AYT","Variety")
  colnames(geneticValues) <- c(1:args$nReps)
  geneticValues
}

getCorrelations <- function(correlations){
  correlations <- as.data.frame(correlations)
  rownames(correlations) <- c("NewParents","F2","F3","F4","F5","PYT","AYT")
  colnames(correlations) <- c(1:args$nReps)  
  correlations
}

getVariances <- function(variances){
  variances <- as.data.frame(variances)
  colnames(variances) <- c(1:args$nReps)
  rownames(variances) <- c("PrevCycPYT", "newParents","F1","F2", "F3","F4", "F5", "PYT","AYT")
  variances
}

getPheno <- function(pheno){
  pheno <- as.data.frame(valuesMat)
  pheno
}

# use trainGen to retrain the model
trainModel <- function(){
  
  if(cycle ==1){
    if (args$trainingData == "F2") { 
      traincycle = cycle
      fullSetGeno = allTrainingDataGeno[[traincycle]]
      fullSetPheno = allTrainingDataPheno[[traincycle]]
      M <- fullSetGeno[[2]]
      y <- fullSetPheno[[2]]
      nIndF5 =nInd(genZero$F5)
      nIndF2 = nInd(genZero$F2)
      remove = nIndF2-nIndF5
      M <<- M[-c(1:remove),]
      Y <<- as.matrix(y[-c(1:remove),])
    }
    
    if (args$trainingData == "F5") {
      traincycle = cycle
      fullSetGeno = allTrainingDataGeno[[traincycle]]
      fullSetPheno = allTrainingDataPheno[[traincycle]]
      M <<- fullSetGeno[[5]]
      Y <<- as.matrix(fullSetPheno[[5]])
    }
    
    if (args$trainingData == "F2_and_F5") {
      traincycle = cycle
      fullSetGeno = allTrainingDataGeno[[traincycle]]
      fullSetPheno = allTrainingDataPheno[[traincycle]]
      F2M = fullSetGeno[[2]]
      F2y = fullSetPheno[[2]] 
      F5M = fullSetGeno[[5]]
      F5y = fullSetPheno[[5]] 
      F2M = F2M[1:60,]
      F5M = F5M[1:60,]
      F2y = F2y[1:60,]
      F5y = F5y[1:60,]
      M <<- rbind(F2M,F5M)
      F2y <<- data.frame(F2y)
      F5y <<- data.frame(F5y)
      Y <- rbind(F2y, F5y)
      Y <<- as.matrix(Y)
    }
    
    if (args$trainingData == "ALL") {
      M <<- as.data.frame(do.call("rbind",trainingGenotypes))
      Y <<- as.data.frame(do.call("rbind",trainingPhenotypes))
    }
    
  source(file.path(MODEL_DIR, fileTrain))
}
}

updateResults <- function(ind, genObj, genName){
  varMat[ind,] <<- varG(genObj) 
  gvMat[ind,] <<- mean(gv(genObj)) 
  curAllelesMat <- getAllelesMat(genObj, genName) 
  allelesMat <<- rbind(allelesMat, curAllelesMat)
  curValuesMat <- updatePheno(genObj, genName) 
  valuesMat <<- rbind(valuesMat, curValuesMat)
}

updatePheno <- function(genObj,genName){
  
  valuesMat = matrix(nrow=nInd(genObj),ncol=5)
                     
  valuesMat[,1] <- as.matrix(rep(paste0(genName,"C",cycle,sep=""), times=nInd(genObj)))
  valuesMat[,2] <- pheno(genObj)
  valuesMat[,3] <- gv(genObj)
  valuesMat[,4] <- bv(genObj)

  noEBV = c("NP", "F1","Variety")
                     
  if (genName %in% noEBV == TRUE){
    valuesMat[,5] <- NA
  }
  if (args$model == "NoModel"){
      valuesMat[,5] <- NA
    }
    
   if(genName %in% noEBV == FALSE & args$model != "NoModel"){
    valuesMat[,5] <- ebv(genObj)}
  

    
  valuesMat = as.data.frame(valuesMat)
  colnames(valuesMat) = c("gen","pheno","gv","tbv","ebv")
  valuesMat
}
                     
  

# The simulation returns is a list of reps. Each rep has a series of variables.
# This function unifies all the reps into one variable.
bindSimResults <- function(reps){
    # Gets names and amount of matrices to be bound
    mat_names <- names(reps[[1]])
    mat_num <- length(mat_names)

    # Create list to store final results. First stores NULL values
    res <- lapply(1:mat_num, function(i) {
        vector("list", length=args$nCycles)
    })
    names(res) <- mat_names

    # Binds / appends column results and stores in res
    for (rep in reps)
        for (mat in mat_names)
            for (cycle in 1:args$nCycles){
                curMat <- rep[[mat]][[cycle]]
                if(ncol(curMat) == 1)
                    res[[mat]][[cycle]] <- cbind(res[[mat]][[cycle]], curMat)
                else
                    res[[mat]][[cycle]] <- appendMat(res[[mat]][[cycle]], curMat)
            }
    res
}

# Appends matrix to list
appendMat <- function(lis, mat){
    if (is.null(lis)) 
        lis <- list()
    lis[[ length(lis)+1 ]] <- mat
    lis
}

readGenVals <- function() {
  #collect results from all cycles into a DF
  
  nCycle = 3
  datalist = list()
  
  for (cycle in 1:nCycle) {
    filename = paste("C", cycle, "_", args$model,"_trainAt",args$trainGen,"_trainWith",args$trainingData,"_",args$parentSelections, "Parents_gvs_snp_yield.csv", sep="")
    data = read.csv(filename)
    datalist[[cycle]] = data
  }
  geneticValues <- do.call(rbind, datalist)
  geneticValues <- geneticValues[-c(11,21),]
  gens = as.data.frame(geneticValues[-1,1])
  values = as.matrix(geneticValues[,-1])
  
  
  cumulativeGain = matrix(nrow = (nrow(values)-1), ncol = (ncol(values)))
  
  #find the cumulative gain across each generation from the start of the sim
  for (x in 1:(nrow(values)-1)) {
    gen1 = as.numeric((values[x+1,]))
    gen2 = as.numeric((values[1,]))
    change = gen1 - gen2
    cumulativeGain[x,] <- change
  }
  
  #find the mean cumulative gain across reps 
  cumulativeGain = as.data.frame(cumulativeGain)
  meanGain = as.data.frame(rowMeans(cumulativeGain))
  resultsGVs = cbind(gens, meanGain) # this DF has each genetic value in consecutive order from the beginning of C1 to the end of C3
  
  #find standard deviation of cumulative mean across reps (columns)
  stdMat = matrix(nrow=nrow(cumulativeGain), ncol=1)
  for (x in 1:nrow(cumulativeGain)) {
    row = cumulativeGain[x,]
    sd = sd(row)
    stdMat[x,1] = sd
  }
  std = as.data.frame(stdMat)
  
  FINAL = cbind(resultsGVs, std)
  FINAL = FINAL[-c(10,19),]
  write.csv(FINAL, paste("GV_", filename, ".csv", sep=""))
}

readPCCs <- function(){
  nCycle = 3
  datalist = list()

for (cycle in 1:nCycle) {
  filename = paste("C", cycle, "_", args$model,"_trainAt",args$trainGen,"_trainWith",args$trainingData,"_",args$parentSelections, "Parents_cors_snp_yield.csv", sep="")
  data = read.csv(filename)
  datalist[[cycle]] = data
}

correlationValues <- do.call(rbind, datalist)
correlationValues = correlationValues[ , colSums(is.na(correlationValues))==0] #remove columns(reps) containing an NA

# take the mean across reps 
values = correlationValues[,-1]
means = rowMeans(values)
gens = as.data.frame(correlationValues[,1])
resultsCORs = cbind(gens,means) # this DF has each correlation value in consecutive order from the beginning of C1 to the end of C3

#find standard deviation of cumulative mean across reps (columns)
stdMat = matrix(nrow=nrow(correlationValues), ncol=1)
for (x in 1:nrow(correlationValues)) {
  row = correlationValues[x,-1]
  sd = sd(row)
  stdMat[x,1] = sd
}
std = as.data.frame(stdMat)

FINAL = cbind(resultsCORs, std)
write.csv(FINAL, paste("COR_", filename, ".csv", sep=""))
}

readVars <- function(){
  nCycle=3
  datalist = list()
  
  model = "rrblup"
  trainGen = "F2"
  trainingData="F2"
  parentSelections="F2"
  
  
  for (cycle in 1:nCycle) {
    filename = paste("C", cycle, "_", args$model,"_trainAt",args$trainGen,"_trainWith",args$trainingData,"_",args$parentSelections, "Parents_vars_snp_yield.csv", sep="")
    data = read.csv(filename)
    datalist[[cycle]] = data
  }
  
  varianceValues <- do.call(rbind, datalist)
  varianceValues = varianceValues[ , colSums(is.na(varianceValues))==0] #remove columns(reps) containing an NA
  varianceValues = varianceValues[-c(10,19),]
  
  values = as.matrix(varianceValues[,-1])
  cumulativeVar = matrix(nrow = (nrow(values)-1), ncol = (ncol(values)))
  
  #find the cumulative delta variance across each generation from the start of the sim
  for (x in 1:(nrow(values)-1)) {
    gen1 = as.numeric((values[x+1,]))
    gen2 = as.numeric((values[1,]))
    change = gen1 - gen2
    cumulativeVar[x,] <- change
  }
  
  
  #find the mean cumulative delta variance across reps 
  gens = as.data.frame(varianceValues[-1,1])
  cumulativeVar = as.data.frame(cumulativeVar)
  meanVar = as.data.frame(rowMeans(cumulativeVar))
  resultsVars = cbind(gens, meanVar) # this DF has each variance value in consecutive order from the beginning of C1 to the end of C3
  
  
  #find standard deviation of cumulative mean across reps (columns)
  stdMat = matrix(nrow=nrow(cumulativeVar), ncol=1)
  for (x in 1:nrow(cumulativeVar)) {
    row = cumulativeVar[x,]
    sd = sd(row)
    stdMat[x,1] = sd
  }
  std = as.data.frame(stdMat)
  
  FINAL = cbind(resultsVars, std)
  write.csv(FINAL, paste("VAR_", filename, ".csv", sep=""))
  
}
  