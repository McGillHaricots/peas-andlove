### This script will carry out a GS breeding strategy on the population you provide in the genotype file. 
### The GS model is rrBLUP and the training set is determined by CDMean ###

### This script will carry out a GS breeding strategy on the population you provide in the genotype file. 
### The GS model is rrBLUP and the training set is determined by CDMean ###

## load required packages ##

library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)

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

########### SIMULATION STARTS HERE #########################

## establish founder population ##

founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)


##define simulation parameters##

SP <- SimParam$new(founderPop)
SP$addTraitADE(10, mean=1350)
SP$setVarE(h2=0.25)
SP$addSnpChip(57)

## generate parents and cross to form F1 ##

Parents = newPop(founderPop)
F1 = randCross(Parents, 100) ##randomly cross 0 parents##

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 5) ##nProgeny = number of progeny per cross## 

##Build GS model using F2##

SampleOptimiz <- read.csv("TRN_SR.csv") ##This file was created by running the Rincent et al 2012 optimization algorithm##
SampleOptimiz <- SampleOptimiz[,2]

M <- as.data.frame(pullSnpGeno(F2))
genoTrain <- M[SampleOptimiz,]

y <- pheno(F2)
y <- as.data.frame(y)
phenoTrain <- as.matrix(y[SampleOptimiz,])

Optim_train <- cbind(phenoTrain, genoTrain)
colnames(Optim_train) <- paste("ID", 1:ncol(Optim_train), sep="")

EBVans <-mixed.solve(phenoTrain, Z=genoTrain, K=NULL, SE=FALSE, return.Hinv=FALSE)

markerEffects <- EBVans$u

M <- as.matrix(pullSnpGeno(F2))
EBV <- M %*% markerEffects ##multiply markers by estimated effects to getEBV


## Set EBVs using model##
F2@ebv <- as.matrix(EBV)

cor1 = cor(gv(F2), ebv(F2))

## select top families to form F3 ##

F3Sel = selectFam(F2, 50, use="ebv", top=TRUE) 
F3 = self(F3Sel)

##set EBV using BLUP model##
M_F3 <-pullSnpGeno(F3)
G_F3 = M_F3-1
EBVF3 <- G_F3 %*% markerEffects

F3@ebv <- as.matrix(EBVF3)

cor2 = cor(gv(F3),ebv(F3))

##select top families from F3 to form F4 ##

F4Sel = selectFam(F3, 30, use="ebv") 
F4 = self(F4Sel)

##set EBV using BLUP model##
M_F4 <-pullSnpGeno(F4)
G_F4 = M_F4-1
EBVF4 <- G_F4 %*% markerEffects

F4@ebv <- as.matrix(EBVF4)

cor3= cor(gv(F4), ebv(F4))

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 15, use="ebv")
F5 = self(F5Sel)

##set EBV using BLUP model##
M_F5 <-pullSnpGeno(F5)
G_F5 = M_F5-1
EBVF5 <- G_F5 %*% markerEffects

F5@ebv <- as.matrix(EBVF5)

cor4 = cor(gv(F5),ebv(F5))

## select top individual from F5 to form preliminary yield trial ##

PYTSel = selectWithinFam(F5, 4, use="ebv") 
PYT = self(PYTSel)

##set EBV using BLUP model##
M_PYT <-pullSnpGeno(PYT)
G_PYT = M_PYT-1
EBVPYT <- G_PYT %*% markerEffects

PYT@ebv<- as.matrix(EBVPYT)
cor5 = cor(gv(PYT),ebv(PYT))

## select top plants from PYT to form advanced yield trial ##

AYTSel = selectInd(PYT,  20, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTsel)


##set EBV using BLUP model##
M_AYT <-pullSnpGeno(AYT)
G_AYT = M_AYT-1
EBVAYT <- G_AYT %*% markerEffects

AYT@ebv <- as.matrix(EBVAYT)

cor6 = cor(gv(AYT),ebv(AYT))

## select top plants to form variety ##
VarietySel = selectInd(AYT, 1, use="ebv")
Variety = self(VarietySel)

## pull genetic value for each generation and write results ##

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
