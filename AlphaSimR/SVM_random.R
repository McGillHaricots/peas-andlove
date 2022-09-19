### This script will carry out a GS breeding strategy on the population you provide in the genotype file. 
### The GS model is Support Vector Machine and the training set is determined randomly ###

## load required packages ##

library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)


library(e1071)

## read in genotype file, should have both chromosomes, 1 2 or 0 1 format##

genotypes <- as.data.frame(read_xlsx("SR_geno.xlsx"))
genotypes <- genotypes[1:1000,]


## must be in 0 1 coding. write and reload new excel file to avoid class incompatibility later ##

genotypes[genotypes==1] <- 0
genotypes[genotypes==2] <- 1
write_xlsx(genotypes, "SRNMAlphaGeno.xlsx")
genotypes <- as.data.frame(read_xlsx("SRNMAlphaGeno.xlsx"))
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


## establish founder population ##
founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)


##define simulation parameters##

SP <- SimParam$new(founderPop)
SP$addTraitA(5,mean=35)
SP$setVarE(h2=0.7)
SP$addSnpChip(40)

## generate parents and cross to form F1 ##

Parents = newPop(founderPop)
F1 = randCross(Parents, 100) 

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 5 )

##Build GS model using F2 as TRN to get EBVs##

set.seed(123)
y <- pheno(F2)
x <- pullSnpGeno(F2)
popF2 = as.data.frame(cbind(y,x))
colnames(popF2) <- paste("ID",1:(ncol(y) + ncol(x)), sep="")

train_index <- sample(1:nrow(popF2), 0.9 * nrow(popF2))
SR_train <- popF2[train_index, ]
SR_test <- popF2[-train_index, ]

##fit model, predict pheno on all markers
fit = svm(ID1 ~ ., data = SR_train, kernel = "radial", cost = 10, scale = FALSE)

##Use model to predict EBVs###

genoF2 <- pullSnpGeno(F2)
colnames(genoF2) <- paste("ID", 2:(ncol(genoF2) +1), sep="")

predictionsF2 = as.numeric(predict(fit,genoF2))

cor1 = cor(predictionsF2, gv(F2))

#set ebvs#

F2@ebv= as.matrix(predictionsF2)

## select top individuals to form F3 ##

F3 = selectFam(F2, 50, use="ebv", top=TRUE) 


##RUN THE SVM MODEL##

genoF3 <- pullSnpGeno(F3)
colnames(genoF3) <- paste("ID", 2:(ncol(genoF3) +1), sep="")
predictionsF3 = predict(fit,genoF3)


cor2 = cor(predictionsF3, gv(F3))

#set ebvs#

F3@ebv= as.matrix(predictionsF3)

##select top families from F3 to form F4 ##

F4 = selectFam(F3, 30, use="ebv", top=TRUE) 


##RUN THE SVM MODEL##

genoF4 <- pullSnpGeno(F4)
colnames(genoF4) <- paste("ID", 2:(ncol(genoF3) +1), sep="")

predictionsF4 = predict(fit,genoF4)

cor3 = cor(predictionsF4, gv(F4))

#set ebvs#

F4@ebv= as.matrix(predictionsF4)


## select top families from F4 to form F5 ##

F5 = selectFam(F4, 15, use="ebv", top=TRUE)


##RUN THE SVM MODEL##

genoF5 <- pullSnpGeno(F5)
colnames(genoF5) <- paste("ID", 2:(ncol(genoF3) +1), sep="")

predictionsF5 = predict(fit,genoF5)


cor4 = cor(predictionsF5, gv(F5))

#set ebvs#

F5@ebv= as.matrix(predictionsF5)


## select top families from F5 to form preliminary yield trial ##

PYT = selectWithinFam(F5, 4, use="ebv", top=TRUE)  


##RUN THE SVM MODEL##

genoPYT <- pullSnpGeno(PYT)
colnames(genoPYT) <- paste("ID", 2:(ncol(genoF3) +1), sep="")

predictionsPYT = predict(fit,genoPYT)

cor5 = cor(predictionsPYT, gv(PYT))

#set ebvs#

PYT@ebv= as.matrix(predictionsPYT)


## select top plants from PYT to form advanced yield trial ##

AYT = selectInd(PYT,  20, use="ebv", reps=5, top=TRUE) 


##RUN THE SVM MODEL##

genoAYT <- pullSnpGeno(AYT)
colnames(genoAYT) <- paste("ID", 2:2:(ncol(genoF3) +1), sep="")

predictionsAYT = predict(fit,genoAYT)


cor6 = cor(predictionsAYT, gv(AYT))

#set ebvs#

AYT@ebv= as.matrix(predictionsAYT)

## select top plants to form variety ##
Variety = selectInd(AYT, 1, use="ebv", top=TRUE)

## pull genetic value for each generation ##


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

###list correlations to view model performacne ##
corMat <- matrix(nrow=6, ncol=1)
corMat[1,] <- cor1
corMat[2,] <- cor2
corMat[3,] <- cor3
corMat[4,] <- cor4
corMat[5,] <- cor5
corMat[6,] <- cor6
