## define variables ##

nModels = 7
nReps = 50
nGen = 9
nVar = 10

## establish empty matrices to hold outputs for Selfing and Recombination Population ##

gvresults <- matrix(nrow=nGen, ncol=nReps)
correlations <- matrix(nrow=nModels, ncol=nReps)
variances <- matrix(nrow=nVar,ncol=nReps)

## Run repeat loop to run reps ##

i = 1
repeat{
  source("runUpdated_rrblup_RD_yield_SNP.R") ##Source the SCript for the SCenario you would like to run##
  gvresults[1,i] <- parentsgv
  gvresults[2,i] <- F1gv
  gvresults[3,i] <- F2gv
  gvresults[4,i] <- F3gv
  gvresults[5,i] <- F4gv
  gvresults[6,i] <- F5gv
  gvresults[7,i] <- PYTgv
  gvresults[8,i] <- AYTgv
  gvresults[9,i] <- Varietygv
  
  correlations[,i] <- corMat
  
  variances[,i] <- varMat
  
  i <- i + 1
  
  if (i > nReps){ ##break at number of desired reps##
    break
  }
  
  
  
  ##create data frames and label##
  gvresults <- as.data.frame(gvresults)
  colnames(gvresults) <- c(1:nReps)
  rownames(gvresults) <- c("Parents","F1","F2","F3","F4","F5","PYT","AYT","Variety")
  
  correlations <- as.data.frame(correlations)
  colnames(correlations) <- c(1:nReps)
  rownames(correlations) <- c("newParents", "F3","F4","F5","PYT","AYT","Variety")
  
  variances <- as.data.frame(variances)
  colnames(variances) <- c(1:nReps)
  rownames(variances) <- c("PrevCyclePYT", "newParents","F1","F2", "F3","F4", "F5", "PYT","AYT",
                           "Variety")
  
  ##write files
  write.csv(gvresults, "rrblup_RD_Allgvs_SNP_yield_Up.csv")
  write.csv(correlations, "rrblup_RD_Cors_SNP_yield_Up.csv")
  write.csv(variances,"rrblup_RD_Vars_SNP_yield_Up.csv")
  
}
