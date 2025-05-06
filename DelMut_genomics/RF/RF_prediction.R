# Usage Rscript RF_prediction matrix RDSfile chrNumber

args = commandArgs(trailingOnly=TRUE)
library(data.table)
#library(randomForest)
library(foreach)
library(doParallel)
library(ranger)
library(R.utils)
library(magrittr)
library("dplyr")
#registerDoParallel(cores=40)
setDTthreads(80)

cat("\t***Loading data***\n")

#Test_data <- fread(args[1],header=T)
data <- fread("../magicVarCalling/MAGICparentsTOTAL_GENOTYPED.vcf",header=T, skip = 109)

data=data %>%
  mutate(Conserved=ifelse(data$Rate<0.5,2,0))

data=data %>%
  mutate(Conserved=ifelse(data$Rate>1,0,data$Conserved))

data=data %>%
  mutate(Conserved=ifelse(data$Rate>0.5 & data$Rate<1,1,data$Conserved)) %>%
  relocate(Conserved, .after = Rate)

cat("\t***Formatting dataset***\n")

data <- data[,Conserved:=as.factor(Conserved)]
data <- data[,VARIANT_TYPE:=as.factor(VARIANT_TYPE)]
#Test_data <- Test_data[,gene:=as.factor(VARIANT_TYPE)]

#Test_data=subset(data, data$Chrm == args[3])
data=subset(data, data$Chrm == 2)
Site_data <- data[,c(1,2,3,4)]
Test_data <- data[,c(-1,-2,-3,-4)]

#colnames(Test_data)
#Test_data$Conserved

cat("\t***Loading Chrm RF model***\n")

#ranger_model <- readRDS(args[2])
ranger_model<- readRDS("RFmodel_Chrm_1.rds")



importance(ranger_model)

cat("\t***Prediction Started***\n")

predict_ranger <-  predict(ranger_model, data = Test_data)

cat("\t***Done predicting***\n")

print(predict_ranger$predictions)

#write.table(predict_ranger$predictions,"test",quote=F,row.names=F,sep="\t")
#Test_data$Conserved
#predict_ranger$predictions

cat("\t***Final formatting and outputting***\n")

predict_factor =ifelse(predict_ranger$predictions[,1]>0.6,0,1)
predict_factor =ifelse(predict_ranger$predictions[,2]>0.6,1,predict_factor)
predict_factor =ifelse(predict_ranger$predictions[,3]>0.6,2,predict_factor)

#print(predict_factor)
str(predict_factor)
str(Site_data$Conserved) 
print(Site_data$Conserved)
table(Site_data$Conserved, predict_factor)

#print(table(Test_data[5], predict_ranger$predictions))
write.table(table(Site_data$Conserved, predict_factor),paste0(args[3],"_predict.table"))
weights <- cbind.data.frame(Site_data,predict_ranger$predictions[,2])
write.table(weights,paste0("Chrm_",args[3],"_weights.tsv"),row.names=F,quote=F,sep="\t")

