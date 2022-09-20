# This scipt will run a given prediction model and training population scenario for a given trait.
# Each of the three populations will be run separately within the script
# Be sure to load proper scrips for the corresponding population 


## define variables ##

nIndF1 = 100
nIndF2 = 500
nIndF3 = 250
nIndF4 = 150
nIndF5 = 75
nIndPYT = 60
nIndAYT = 20
nIndVariety = 1
nModels = 6
nReps = 25

## establish empty matrices to hold outputs for Selfing and Recombination Population ##
F1resultsSR <- matrix(nrow=nIndF1, ncol=nReps)
F2resultsSR <- matrix(nrow=nIndF2, ncol=nReps) 
F3resultsSR <- matrix(nrow=nIndF3, ncol=nReps) 
F4resultsSR <- matrix(nrow=nIndF4, ncol=nReps) 
F5resultsSR <- matrix(nrow=nIndF5, ncol=nReps) 
PYTresultsSR <- matrix(nrow=nIndPYT, ncol=nReps) 
AYTresultsSR <- matrix(nrow=nIndAYT, ncol=nReps)
VarietyresultsSR <- matrix(nrow=nIndVariety, ncol=nReps) 
corResultsSR <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1 
repeat{
  source("rrBLUP_Random_SR_Flowering.R") ##Source the script for the scenario you would like to run##
  F1resultsSR[,i] <- F1gv ##fills i column with F1gv results etc
  F2resultsSR[,i] <- F2gv
  F3resultsSR[,i] <- F3gv
  F4resultsSR[,i] <- F4gv
  F5resultsSR[,i] <- F5gv
  PYTresultsSR[,i] <- PYTgv
  AYTresultsSR[,i] <- AYTgv
  VarietyresultsSR[,i] <- Varietygv
  corResultsSR[,i] <- corMat
  
  i <- i + 1
  
  if (i > 25){ ##break at number of desired reps##
    break
  }
  F1resultsSR <- as.data.frame(F1resultsSR)
  F1resultsSR$generation <- rep("F1", times=nIndF1)
  
  F2resultsSR <- as.data.frame(F2resultsSR)
  F2resultsSR$generation <- rep("F2", times=nIndF2)
  
  F3resultsSR<-as.data.frame(F3resultsSR)
  F3resultsSR$generation <- rep("F3", times=nIndF3)
  
  F4resultsSR<-as.data.frame(F4resultsSR)
  F4resultsSR$generation <- rep("F4", times=nIndF4)
  
  F5resultsSR<-as.data.frame(F5resultsSR)
  F5resultsSR$generation <- rep("F5", times=nIndF5)
  
  PYTresultsSR<-as.data.frame(PYTresultsSR)
  PYTresultsSR$generation <- rep("PYT", times=nIndPYT)
  
  AYTresultsSR<-as.data.frame(AYTresultsSR)
  AYTresultsSR$generation <- rep("AYT", times=nIndAYT)
  
  VarietyresultsSR<-as.data.frame(VarietyresultsSR)
  VarietyresultsSR$generation <- rep("Variety", times=nIndVariety)
  
  ##compile into one data frame##
  allResultsSR <- rbind(F1resultsSR, F2resultsSR, F3resultsSR, F4resultsSR, F5resultsSR, PYTresultsSR, AYTresultsSR, VarietyresultsSR)
  
  
  ##write files
  write.csv(allResultsSR, "RRBLUP_random_Allgvs_SR_Flowering.csv")
  write.csv(corResultsSR, "RRBLUP_random_Correlation_SR_Flowering.csv")
  
  
}

##########################################################################


## establish empty matrices to hold outputs for Selfing,Recombination, and Natural Selection pop##
F1resultsSRN <- matrix(nrow=nIndF1, ncol=nReps)
F2resultsSRN <- matrix(nrow=nIndF2, ncol=nReps) 
F3resultsSRN <- matrix(nrow=nIndF3, ncol=nReps) 
F4resultsSRN <- matrix(nrow=nIndF4, ncol=nReps) 
F5resultsSRN <- matrix(nrow=nIndF5, ncol=nReps) 
PYTresultsSRN <- matrix(nrow=nIndPYT, ncol=nReps) 
AYTresultsSRN <- matrix(nrow=nIndAYT, ncol=nReps)
VarietyresultsSRN <- matrix(nrow=nIndVariety, ncol=nReps) 
corResultsSRN <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1 
repeat{
  source("rrBLUP_Random_SRN_Flowering.R") ##Source the script for the scenario you would like to run##
  F1resultsSRN[,i] <- F1gv ##fills i column with F1gv results etc
  F2resultsSRN[,i] <- F2gv
  F3resultsSRN[,i] <- F3gv
  F4resultsSRN[,i] <- F4gv
  F5resultsSRN[,i] <- F5gv
  PYTresultsSRN[,i] <- PYTgv
  AYTresultsSRN[,i] <- AYTgv
  VarietyresultsSRN[,i] <- Varietygv
  corResultsSRN[,i] <- corMat
  
  i <- i + 1
  
  if (i > 25){ ##break at number of desired reps##
    break
  }
  F1resultsSRN <- as.data.frame(F1resultsSRN)
  F1resultsSRN$generation <- rep("F1", times=nIndF1)
  
  F2resultsSRN <- as.data.frame(F2resultsSRN)
  F2resultsSRN$generation <- rep("F2", times=nIndF2)
  
  F3resultsSRN<-as.data.frame(F3resultsSRN)
  F3resultsSRN$generation <- rep("F3", times=nIndF3)
  
  F4resultsSRN<-as.data.frame(F4resultsSRN)
  F4resultsSRN$generation <- rep("F4", times=nIndF4)
  
  F5resultsSRN<-as.data.frame(F5resultsSRN)
  F5resultsSRN$generation <- rep("F5", times=nIndF5)
  
  PYTresultsSRN<-as.data.frame(PYTresultsSRN)
  PYTresultsSRN$generation <- rep("PYT", times=nIndPYT)
  
  AYTresultsSRN<-as.data.frame(AYTresultsSRN)
  AYTresultsSRN$generation <- rep("AYT", times=nIndAYT)
  
  VarietyresultsSRN<-as.data.frame(VarietyresultsSRN)
  VarietyresultsSRN$generation <- rep("Variety", times=nIndVariety)
  
  ##compile into one data frame##
  allResultsSRN <- rbind(F1resultsSRN, F2resultsSRN, F3resultsSRN, F4resultsSRN, F5resultsSRN, PYTresultsSRN, AYTresultsSRN, VarietyresultsSRN)
  
  
  ##write files
  write.csv(allResultsSRN, "RRBLUP_random_Allgvs_SRN_Flowering.csv")
  write.csv(corResultsSRN, "RRBLUP_random_Correlation_SRN_Flowering.csv")
  
  
}


##########################################################################


## establish empty matrices to hold outputs for Selfing, Recombination, Natural Selection, and Migration pop##
F1resultsSRNM <- matrix(nrow=nIndF1, ncol=nReps)
F2resultsSRNM <- matrix(nrow=nIndF2, ncol=nReps) 
F3resultsSRNM <- matrix(nrow=nIndF3, ncol=nReps) 
F4resultsSRNM <- matrix(nrow=nIndF4, ncol=nReps) 
F5resultsSRNM <- matrix(nrow=nIndF5, ncol=nReps) 
PYTresultsSRNM <- matrix(nrow=nIndPYT, ncol=nReps) 
AYTresultsSRNM <- matrix(nrow=nIndAYT, ncol=nReps)
VarietyresultsSRNM <- matrix(nrow=nIndVariety, ncol=nReps) 
corResultsSRNM <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1 
repeat{
  source("rrBLUP_Random_SRNM_Flowering.R") ##Source the script for the scenario you would like to run##
  F1resultsSRNM[,i] <- F1gv ##fills i column with F1gv results etc
  F2resultsSRNM[,i] <- F2gv
  F3resultsSRNM[,i] <- F3gv
  F4resultsSRNM[,i] <- F4gv
  F5resultsSRNM[,i] <- F5gv
  PYTresultsSRNM[,i] <- PYTgv
  AYTresultsSRNM[,i] <- AYTgv
  VarietyresultsSRNM[,i] <- Varietygv
  corResultsSRNM[,i] <- corMat
  
  i <- i + 1
  
  if (i > 25){ ##break at number of desired reps##
    break
  }
  F1resultsSRNM <- as.data.frame(F1resultsSRNM)
  F1resultsSRNM$generation <- rep("F1", times=nIndF1)
  
  F2resultsSRNM <- as.data.frame(F2resultsSRNM)
  F2resultsSRNM$generation <- rep("F2", times=nIndF2)
  
  F3resultsSRNM<-as.data.frame(F3resultsSRNM)
  F3resultsSRNM$generation <- rep("F3", times=nIndF3)
  
  F4resultsSRNM<-as.data.frame(F4resultsSRNM)
  F4resultsSRNM$generation <- rep("F4", times=nIndF4)
  
  F5resultsSRNM<-as.data.frame(F5resultsSRNM)
  F5resultsSRNM$generation <- rep("F5", times=nIndF5)
  
  PYTresultsSRNM<-as.data.frame(PYTresultsSRNM)
  PYTresultsSRNM$generation <- rep("PYT", times=nIndPYT)
  
  AYTresultsSRNM<-as.data.frame(AYTresultsSRNM)
  AYTresultsSRNM$generation <- rep("AYT", times=nIndAYT)
  
  VarietyresultsSRNM<-as.data.frame(VarietyresultsSRNM)
  VarietyresultsSRNM$generation <- rep("Variety", times=nIndVariety)
  
  ##compile into one data frame##
  allResultsSRNM <- rbind(F1resultsSRNM, F2resultsSRNM, F3resultsSRNM, F4resultsSRNM, F5resultsSRNM, PYTresultsSRNM, AYTresultsSRNM, VarietyresultsSRNM)
  
  
  ##write files
  write.csv(allResultsSRNM, "RRBLUP_random_Allgvs_SRNM_Flowering.csv")
  write.csv(corResultsSRNM, "RRBLUP_random_Correlation_SRNM_Flowering.csv")
  
  
}

##########################################################################
