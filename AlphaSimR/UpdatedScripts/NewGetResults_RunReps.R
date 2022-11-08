# This script will run a given prediction model and training population scenario for a given trait.
# Each of the three populations will be run separately within the script
# Be sure to load proper scrips for the corresponding population 


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_yield_sr.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }


  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SR_yield.csv")
  write.csv(correlations, "rrblup_random_Cors_SR_yield.csv")
  
}

######################################################################################


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_yield_srn.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SRN_yield.csv")
  write.csv(correlations, "rrblup_random_Cors_SRN_yield.csv")
  
}

######################################################################################


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_yield_srnm.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SRNM_yield.csv")
  write.csv(correlations, "rrblup_random_Cors_SRNM_yield.csv")
  
}

######################################################################################


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_flowering_sr.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SR_flowering.csv")
  write.csv(correlations, "rrblup_random_Cors_SR_flowering.csv")
  
}


######################################################################################


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_flowering_srn.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SRN_flowering.csv")
  write.csv(correlations, "rrblup_random_Cors_SRN_flowering.csv")
  
}


######################################################################################


## define variables ##

nModels = 6
nReps = 50

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_random_flowering_srnm.R") ##Source the script for the scenario you would like to run##
  gvresults[1,i] <- F1gv
  gvresults[2,i] <- F2gv
  gvresults[3,i] <- F3gv
  gvresults[4,i] <- F4gv
  gvresults[5,i] <- F5gv
  gvresults[6,i] <- PYTgv
  gvresults[7,i] <- AYTgv
  gvresults[8,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  i <- i + 1
  
  if (i > 50){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:50)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:50)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_random_Allgvs_SRNM_flowering.csv")
  write.csv(correlations, "rrblup_random_Cors_SRNM_flowering.csv")
  
}
