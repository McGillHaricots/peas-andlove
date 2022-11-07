# This script will run a given prediction model and training population scenario for a given trait.
# Each of the three populations will be run separately within the script
# Be sure to load proper scrips for the corresponding population 

### RANDOM TRAINING ###

## define variables ##

nModels = 6
nReps = 25

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=8, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("run_rrBLUP_stratifiedClusters_flowering_srn.R") ##Source the script for the scenario you would like to run##
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
  
  if (i > 25){ ##break at number of desired reps##
    break
  }


  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:25)
  rownames(gvresults) <- c("F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:25)
  rownames(correlations) <- c("F3","F4","F5","PYT","AYT","Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_stratifiedClusters_Allgvs_SRN_flowering.csv")
  write.csv(correlations, "rrblup_random_stratifiedClusters_SRN_flowering.csv")
  
}