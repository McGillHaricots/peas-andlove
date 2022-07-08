## load required packages ##

library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)


library(caret)
library(ranger)
library(tidyverse)
library(e1071)

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

####### SIMULATION STARTS HERE #############


## establish founder population ##
founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)


##define simulation parameters##

SP <- SimParam$new(founderPop)
SP$addTraitA(5, mean=1350)
SP$setVarE(h2=0.1)
SP$addSnpChip(50)

## generate parents and cross to form F1 ##

Parents = newPop(founderPop)
F1 = randCross(Parents, 25) 

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 50) 

##Build GS model to get EBVs##

  set.seed(23489)
  pheno <- pheno(F1) #pull F1  phenotypes#
  geno <- pullSnpGeno(F1) #pull F1 genotypes#
  pop <- cbind(pheno, geno) #create data frame with pheno and geno data#
  colnames(pop) <- paste("ID",1:551, sep="") # name columns, note ID1 will be the phenotype, IDs 2-551 are genotypes##

  ### create a random training/testing partition ###
  train_index <- sample(1:nrow(pop), 0.9 * nrow(pop))
  SR_train <- pop[train_index, ]
  SR_test <- pop[-train_index, ]

  ### define cross validation strategy ###
  control <- trainControl(method='repeatedcv', number=3, repeats=3)

  ##build model##
  rf_fit = train(as.factor(ID1) ~ ., #predict phenotype (ID1) using all other factors# 
              data = SR_train, 
              method = "ranger",
              trControl=control)
  
  #make predictions on F2 ##
  
  pheno <- pheno(F2) #pull F2  phenotypes#
  geno <- pullSnpGeno(F2) #pull F2 genotypes#
  F2data <- cbind(pheno, geno) #create data frame with pheno and geno data#
  colnames(pop) <- paste("ID",1:551, sep="") 
  
  predictions <- predict(rf_fit, F2data)
  
  #pull ebvs##
  
  EBV = predictions
  
  cor1 = cor(EBV, gv(F2))
  
  #set ebvs#
  
  f2@ebv= as.matrix(EBV)

## select top individuals to form F3 ##

F3 = selectFam(F2, 10, use="ebv") 

##RUN THE RF MODEL##

set.seed(23489)
pheno <- pheno(F3)
geno <- pullSnpGeno(F3)
F3data <- cbind(pheno, geno)
colnames(pop) <- paste("ID",1:551, sep="")

#make predictions##

predictions <- predict(rf_fit, F3data)

#pull ebvs##

EBV = predictions

cor1 = cor(EBV, gv(F3))

#set ebvs#

f3@ebv= as.matrix(EBV)

##select top families from F3 to form F4 ##

F4 = selectFam(F3, 7, use="ebv") 

##RUN THE RF MODEL##

set.seed(23489)
pheno <- pheno(F4)
geno <- pullSnpGeno(F4)
F4data <- cbind(pheno, geno)
colnames(pop) <- paste("ID",1:551, sep="")

#make predictions##

predictions <- predict(rf_fit, F4data)

#pull ebvs##

EBV = predictions

cor1 = cor(EBV, gv(F4))

#set ebvs#

f4@ebv= as.matrix(EBV)


## select top families from F4 to form F5 ##

F5 = selectFam(F4, 5, use="ebv")


##RUN THE RF MODEL##

set.seed(23489)
pheno <- pheno(F5)
geno <- pullSnpGeno(F5)
F5data <- cbind(pheno, geno)
colnames(pop) <- paste("ID",1:551, sep="")

#make predictions##

predictions <- predict(rf_fit, F5data)

#pull ebvs##

EBV = predictions

cor1 = cor(EBV, gv(F5))

#set ebvs#

f5@ebv= as.matrix(EBV)


## select top families from F5 to form preliminary yield trial ##

PYT = selectInd(F5, 20, use="ebv") 


##RUN THE RF MODEL##

set.seed(23489)
pheno <- pheno(PYT)
geno <- pullSnpGeno(PYT)
PYTdata <- cbind(pheno, geno)
colnames(pop) <- paste("ID",1:551, sep="")

#make predictions##

predictions <- predict(rf_fit, PYTdata)

#pull ebvs##

EBV = predictions

cor1 = cor(EBV, gv(PYT))

#set ebvs#

PYT@ebv= as.matrix(EBV)

## select top plants from PYT to form advanced yield trial ##

AYT = selectInd(PYT,  20, use="ebv") 


##RUN THE RF MODEL##

set.seed(23489)
pheno <- pheno(AYT)
geno <- pullSnpGeno(AYT)
AYTdata <- cbind(pheno, geno)
colnames(pop) <- paste("ID",1:551, sep="")

#make predictions##

predictions <- predict(rf_fit, AYTdata)

#pull ebvs##

EBV = predictions

cor1 = cor(EBV, gv(AYT))

#set ebvs#

AYT@ebv= as.matrix(EBV)

## select top plants to form variety ##
Variety = selectInd(AYT, 1, use="ebv")

## pull genetic value for each generation ##

gv = list(Parents = gv(Parents),
          F1 = gv(F1),
          F2 = gv(F2),
          F3 = gv(F3),
          F4 = gv(F4),
          F5 = gv(F5),
          PYT = gv(PYT),
          AYT = gv(AYT),
          Variety = gv(Variety))

gv <- as.data.frame(gv)
write.csv(gv, "gv_sy_sr_rf.csv")

cor = list(cor1, cor2, cor3, cor4, cor5, cor6)
write.csv(cor, "cor_sy_sr_rf.csv")
