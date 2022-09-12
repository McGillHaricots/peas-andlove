## define variables ##

nIndF1 = 100
nIndF2 = 500
nIndF3 = 250
nIndF4 = 150
nIndF5 = 75
nIndPYT = 60
nIndAYT = 20
nIndVariety = 1
nGen = 9
nModels = 6
nReps = 25

## establish empty matrices to hold outputs##
F1results <- matrix(nrow=nIndF1, ncol=nReps)
F2results <- matrix(nrow=nIndF2, ncol=nReps) 
F3results <- matrix(nrow=nIndF3, ncol=nReps) 
F4results <- matrix(nrow=nIndF4, ncol=nReps) 
F5results <- matrix(nrow=nIndF5, ncol=nReps) 
PYTresults <- matrix(nrow=nIndPYT, ncol=nReps) 
AYTresults <- matrix(nrow=nIndAYT, ncol=nReps)
Varietyresults <- matrix(nrow=nIndVariety, ncol=nReps) 
gvAveResults <- matrix(nrow=nGen, ncol=nReps)
corResults <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1 
repeat{
  source("rrBLUP_random.R") ##Source the script for the scenario you would like to run##
  F1results[,i] = F1gv
  F2results[,i] = F2gv
  F3results[,i] = F3gv
  F4results[,i] = F4gv
  F5results[,i] = F5gv
  PYTresults[,i] = PYTgv
  AYTresults[,i] = AYTgv
  Varietyresults[,i] = Varietygv
  corResults[,i] = corMat
  
  i <- i + 1
  
  if (i > 25){ ##break at number of desired reps##
    break
  }
  F1results <- as.data.frame(F1results)
  F1results$generation <- rep("F1", times=nIndF1)
  
  F2results <- as.data.frame(F2results)
  F2results$generation <- rep("F2", times=nIndF2)
  
  F3results<-as.data.frame(F3results)
  F3results$generation <- rep("F3", times=nIndF3)
  
  F4results<-as.data.frame(F4results)
  F4results$generation <- rep("F4", times=nIndF4)
  
  F5results<-as.data.frame(F5results)
  F5results$generation <- rep("F5", times=nIndF5)
  
  PYTresults<-as.data.frame(PYTresults)
  PYTresults$generation <- rep("PYT", times=nIndPYT)
  
  AYTresults<-as.data.frame(AYTresults)
  AYTresults$generation <- rep("AYT", times=nIndAYT)
  
  Varietyresults<-as.data.frame(Varietyresults)
  Varietyresults$generation <- rep("Variety", times=nIndVariety)
  
  ##compile into one data frame##
  allResults <- rbind(F1results, F2results, F3results, F4results, F5results, PYTresults, AYTresults, Varietyresults)
  
  
  ##write files
  write.csv(allResults, "BLUP_Random_Allgvs_SR_Yield.csv")
  write.csv(corResults, "BLUP_Random_Correlation_SR_Yield.csv")
  
           
  
}
