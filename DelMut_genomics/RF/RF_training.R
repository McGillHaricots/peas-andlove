#Usage Rscript RF_training.R matrix chrN 

args = commandArgs(trailingOnly=TRUE)
#install.packages("randomForest")
#install.packages("ranger")
#install.packages('R.utils')
library(data.table)
library(randomForest)
library(foreach)
library(doParallel)
library(ranger)
library(dplyr)
library(R.utils)
registerDoParallel(cores=40)
setDTthreads(40)


print("*** Loading data... ***")

RFdata <- fread("raw_matrix.sift.rate.coverage.txt")

print("*** Data Loaded!! ***")

# exclusion of sites with a rate between 0.5 and 1
subset1=subset(RFdata,RFdata$Rate <= 0.5)
subset2=subset(RFdata,RFdata$Rate >= 1)
RFdata=rbind(subset1,subset2)
                

RFdata=RFdata%>%
  mutate(Conserved=ifelse(RFdata$Rate<"0.5","1","0")) %>%
  relocate(Conserved, .after = Rate)

RFdata <- RFdata[,Conserved:=as.factor(Conserved)]
RFdata <- RFdata[,VARIANT_TYPE:=as.factor(VARIANT_TYPE)]

print("*** prediction class set to factor ***")
print(class(RFdata))
Chromosome <- as.numeric(args[2])
Chromosome <- as.numeric("1")


RFdata <- RFdata %>% mutate(Caseweights=ifelse(Conserved==1,1/sum(Conserved==1),1/sum(Conserved==0))) %>%
  relocate(Caseweights, .after = Conserved)

case_weights <- ifelse(RFdata$Chrm== as.numeric(Chromosome), 0, as.numeric(RFdata$Caseweights)) # We are leaving the specified chrm out by assigning a weight of 0 to vectors that belong to that chrm. Later in the model these weights are considered.

Traindata <- RFdata[,c(-1,-2,-3,-5)] # After creating case.weight objects, it is removed from the dataset, so is not included in the model
print(class(Traindata))
n_max <- 1e7
sample_size <- 5e4
include.negative <- FALSE
include.NA <- TRUE
left_out_chromosome <- Chromosome
n_trees <- 1000
min_node_size <- 100
mtry=22
n_cores <- 1
save.memory <- FALSE
p <- 500
chunk_size <- 1e5
n <- nrow(Traindata)
print(n)
print(colnames(Traindata))

print("*** RANDOM FOREST MODEL ***")

fit <- ranger(
  Conserved ~ .,
  data=as.data.frame(Traindata),
  sample.fraction=sample_size/n,
  importance="impurity",
  write.forest=TRUE,
  probability=TRUE,
  min.node.size=min_node_size,
  num.trees=n_trees,
  mtry=mtry,
  oob.error=FALSE,
  num.threads=n_cores,
  always.split.variables=c("SIFT_SCORE","VARIANT_TYPE", "Distance"),
  seed=1,
  save.memory=(save.memory & (p > 100)),
  case.weight = case_weights
)

print("*** Saving data ***")

saveRDS(fit, paste0(args[1],"_Chr",Chromosome,".rds"))
