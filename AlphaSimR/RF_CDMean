### This script will carry out a GS breeding strategy on the population you provide in the genotype file. 
### The GS model is Random Forest and the training set is determined by CDMean ###

## load required packages ##

library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)


library(caret)
library(ranger)
library(tidyverse)
library(e1071)
library(randomForest)

## read in genotype file, should have both chromosomes, 1 2 or 0 1 format##

genotypes <- as.data.frame(read_xlsx("SR_geno.xlsx"))
genotypes <- genotypes[1:1000,]


## must be in 0 1 coding. write and reload new excel file to avoid class incompatibility later ##

genotypes[genotypes==1] <- 0
genotypes[genotypes==2] <- 1
write_xlsx(genotypes, "SRAlphaGeno.xlsx")
genotypes <- as.data.frame(read_xlsx("SRAlphaGeno.xlsx"))
rownames(genotypes) = NULL
colnames(genotypes) = NULL

## read in map , must be in Morgans ##

genomap <- read_xlsx("phaseolusmap.xlsx")
genomap <-genomap[,c(2,5)] ##column 2 has chromosome, 5 has morgans##

## create separate map for each chromosome ##

chr1 <- as.data.frame(genomap[genomap$chr==1,])
chr2 <- as.data.frame(genomap[genomap$chr==2,])
chr3 <- as.data.frame(genomap[genomap$chr==3,])
chr4 <- as.data.frame(genomap[genomap$chr==4,])
chr5 <- as.data.frame(genomap[genomap$chr==5,])
chr6 <- as.data.frame(genomap[genomap$chr==6,])
chr7 <- as.data.frame(genomap[genomap$chr==7,])
chr8 <- as.data.frame(genomap[genomap$chr==8,])
chr9 <- as.data.frame(genomap[genomap$chr==9,])
chr10 <- as.data.frame(genomap[genomap$chr==10,])
chr11 <- as.data.frame(genomap[genomap$chr==11,])

## list maps for each chromosome. this list will be read by AlphaSim when creating the Founder Pop##

genMap = list(chr1[,2],
              chr2[,2],
              chr3[,2],
              chr4[,2],
              chr5[,2],
              chr6[,2],
              chr7[,2],
              chr8[,2],
              chr9[,2],
              chr10[,2],
              chr11[,2])

## genotypes must be separated by chromosome ##

chr1geno <- genotypes[,1:nrow(chr1)]
chr2geno <- genotypes[,(nrow(chr1)+1):(nrow(chr1)+nrow(chr2))]
chr3geno <- genotypes[,(nrow(chr1)+nrow(chr2) +1):(nrow(chr1)+nrow(chr2)+nrow(chr3))]
chr4geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4))]
chr5geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5))]
chr6geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6))]
chr7geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7))]
chr8geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8))]
chr9geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+nrow(chr9))]
chr10geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+nrow(chr9)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+nrow(chr9)+nrow(chr10))]
chr11geno <- genotypes[,(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+nrow(chr9)+nrow(chr10)+1):(nrow(chr1)+nrow(chr2)+nrow(chr3)+nrow(chr4)+nrow(chr5)+nrow(chr6)+nrow(chr7)+nrow(chr8)+nrow(chr9)+nrow(chr10)+nrow(chr11))]

## above genotypes must be matrices ##

chr1geno = as.matrix(chr1geno,nrow=2000,ncol=ncol(chr1geno))
chr2geno = as.matrix(chr2geno,nrow=2000,ncol=ncol(chr2geno))
chr3geno = as.matrix(chr3geno,nrow=2000,ncol=ncol(chr3geno))
chr4geno = as.matrix(chr4geno,nrow=2000,ncol=ncol(chr4geno))
chr5geno = as.matrix(chr5geno,nrow=2000,ncol=ncol(chr5geno))
chr6geno = as.matrix(chr6geno,nrow=2000,ncol=ncol(chr6geno))
chr7geno = as.matrix(chr7geno,nrow=2000,ncol=ncol(chr7geno))
chr8geno = as.matrix(chr8geno,nrow=2000,ncol=ncol(chr8geno))
chr9geno = as.matrix(chr9geno,nrow=2000,ncol=ncol(chr9geno))
chr10geno = as.matrix(chr10geno,nrow=2000,ncol=ncol(chr10geno))
chr11geno = as.matrix(chr11geno,nrow=2000,ncol=ncol(chr11geno))

## list genotypes matrices. this file will be read by AlphaSim when creating the Founder Pop ##

haplotypes = list(chr1geno,chr2geno, chr3geno, chr4geno,chr5geno,chr6geno,chr7geno,chr8geno,chr9geno,chr10geno,chr11geno)

## confirm geno and map are the same length ##
length(genMap)
length(haplotypes)

####### SIMULATION STARTS HERE ######

## establish founder population ##
founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE,
                       ploidy = 2L)


##define simulation parameters##

SP <- SimParam$new(founderPop)
SP$addTraitADE(6, mean=1350)
SP$setVarE(h2=0.25)
SP$addSnpChip(55)

## generate parents and cross to form F1 ##

Parents = newPop(founderPop)
F1 = randCross(Parents, 100) 

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 6) 

###### BUILD GS MODEL #####

##Pull genotypes and phenotypes##

y = pheno(F2)
M = pullSnpGeno(F2)
M = M-1

dim(M) ## tells how large samples should be ##
dim(y)



SampleOptimiz <- read.csv("TRN_SR.csv") ##This file was created by running the Rincent et al 2012 optimization algorithm##
SampleOptimiz <- SampleOptimiz[,2]

M <- as.data.frame(pullSnpGeno(F2))
genoTrain <- M[SampleOptimiz,]

y <- pheno(F2)
y <- as.data.frame(y)
phenoTrain <- as.matrix(y[SampleOptimiz,])

Optim_train <- cbind(phenoTrain, genoTrain)
colnames(Optim_train) <- paste("ID", 1:ncol(Optim_train), sep="")

##build model##

control <- trainControl(method='repeatedcv', 
                        number=10, ##will test 10 different values for mtry (number of variables for splitting) ##
                        repeats=3,
                        search = "random")    

bestmtry <- tuneRF(Optim_train,Optim_train$ID1,stepFactor = 1.2, improve = 0.01, trace=T, plot= T)

rf_fit = train(ID1 ~ ., 
               data = Optim_train, 
               method = "rf",
               tuneLength= 10,
               trControl=control) ## search a random tuning grid ##

### This command takes about 90 minutes in an compute canada interactive session ###

## look at the parameters of the model ##
print(rf) 

#make predictions##


phenoF2 <- pheno(F2)
genoF2 <- pullSnpGeno(F2)
popF2 <- cbind(phenoF2, genoF2)
colnames(popF2) <- paste("ID",1:ncol(popF2), sep="")  

predictionsF2 <- as.numeric(predict(rf_fit, popF2))

cor1 = cor(predictionsF2, gv(F2))

#set ebvs#

F2@ebv= as.matrix(predictionsF2)

## select top individuals to form F3 ##

F3Sel = selectFam(F2, 50, use="ebv", top=TRUE) 
F3 = self(F3Sel)

##RUN THE RF MODEL##

set.seed(23489)
phenoF3 <- pheno(F3)
genoF3 <- pullSnpGeno(F3)
popF3 <- cbind(phenoF3, genoF3)
colnames(popF3) <- paste("ID",1:606, sep="")

#make predictions##

predictionsF3 <- as.numeric(predict(rf_fit, popF3))

cor2 = cor(predictionsF3, gv(F3))

#set ebvs#

F3@ebv= as.matrix(predictionsF3)

##select top families from F3 to form F4 ##

F4Sel = selectFam(F3, 30, use="ebv", top=TRUE) 
F4 = self(F4Sel)

##RUN THE RF MODEL##

set.seed(23489)
phenoF4 <- pheno(F4)
genoF4 <- pullSnpGeno(F4)
popF4 <- cbind(phenoF4, genoF4)
colnames(popF4) <- paste("ID",1:606, sep="")

#make predictions##

predictionsF4 <- as.numeric(predict(rf_fit, popF4))

cor3 = cor(predictionsF4, gv(F4))

#set ebvs#

F4@ebv= as.matrix(predictionsF4)


## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 15, use="ebv", top=TRUE)
F5 = self(F5Sel)

##RUN THE RF MODEL##

set.seed(23489)
phenoF5 <- pheno(F5)
genoF5 <- pullSnpGeno(F5)
popF5 <- cbind(phenoF5, genoF5)
colnames(popF5) <- paste("ID",1:606, sep="")

#make predictions##

predictionsF5 <- as.numeric(predict(rf_fit, popF5))

cor4 = cor(predictionsF5, gv(F5))

#set ebvs#

F5@ebv= as.matrix(predictionsF5)


## select top families from F5 to form preliminary yield trial ##

PYTSel = selectWithinFam(F5, 4, use="ebv", top=TRUE) 
PYT = self(PYTSel)

##RUN THE RF MODEL##

set.seed(23489)
phenoPYT <- pheno(PYT)
genoPYT <- pullSnpGeno(PYT)
popPYT <- cbind(phenoPYT, genoPYT)
colnames(popPYT) <- paste("ID",1:606, sep="")

#make predictions##

predictionsPYT <- as.numeric(predict(rf_fit, popPYT))

cor5 = cor(predictionsPYT, gv(PYT))

#set ebvs#

PYT@ebv= as.matrix(predictionsPYT)

## select top plants from PYT to form advanced yield trial ##

AYTSel = selectInd(PYT,  20, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTSel)

##RUN THE RF MODEL##

set.seed(23489)
phenoAYT <- pheno(AYT)
genoAYT <- pullSnpGeno(AYT)
popAYT <- cbind(phenoAYT, genoAYT)
colnames(popAYT) <- paste("ID",1:606, sep="")

#make predictions##

predictionsAYT <- as.numeric(predict(rf_fit, popAYT))

cor6 = cor(predictionsAYT, gv(AYT))

#set ebvs#

AYT@ebv= as.matrix(predictionsAYT)

## select top plants to form variety ##

VarietySel = selectInd(AYT, 1, use="ebv", top=TRUE)
Variety = self(VarietySel)

## pull genetic value for each generation to write results ##

gv = list(Parents = mean(gv(Parents)),
          F1 = mean(gv(F1)),
          F2 = mean(gv(F2)),
          F3 = mean(gv(F3)),
          F4 = mean(gv(F4)),
          F5 = mean(gv(F5)),
          PYT = mean(gv(PYT)),
          AYT = mean(gv(AYT)),
          Variety = mean(gv(Variety)))

F1gv <- gv(F1)
F2gv <- gv(F2)
F3gv <- gv(F3)
F4gv <- gv(F4)
F5gv <- gv(F5)
PYTgv <- gv(PYT)
AYTgv <- gv(AYT)
Varietygv <- gv(Variety)

F1gv <- as.data.frame(F1gv)
F1gv$generation <- rep("F1", times=nrow(F1gv))

F2gv <- as.data.frame(F2gv)
F2gv$generation <- rep("F2", times=nrow(F2gv))

F3gv <- as.data.frame(F3gv)
F3gv$generation <- rep("F3", times=nrow(F3gv))


F4gv <- as.data.frame(F4gv)
F4gv$generation <- rep("F4", times=nrow(F4gv))

F5gv <- as.data.frame(F5gv)
F5gv$generation <- rep("F5", times=nrow(F5gv))

PYTgv <- as.data.frame(PYTgv)
PYTgv$generation <- rep("PYT", times=nrow(PYTgv))

AYTgv <- as.data.frame(AYTgv)
AYTgv$generation <- rep("AYT", times=nrow(AYTgv))

Varietygv <- as.data.frame(Varietygv)
Varietygv$generation <- rep("Variety", times=nrow(Varietygv))

allResults <- rbind(F1gv, F2gv, F3gv,F4gv,F5gv,PYTgv,AYTgv,Varietygv)
write.csv(allResults, "RF_cdMean_Allgvs_SR_Yield.csv")


###list correlations to view model performacne ##
corMat <- matrix(nrow=6, ncol=1)
corMat[1,] <- cor1
corMat[2,] <- cor2
corMat[3,] <- cor3
corMat[4,] <- cor4
corMat[5,] <- cor5
corMat[6,] <- cor6
corMat <- as.data.frame(corMat)
write.csv(corMat, "RF_cdMean_Correlation_SR_Yield.csv")
