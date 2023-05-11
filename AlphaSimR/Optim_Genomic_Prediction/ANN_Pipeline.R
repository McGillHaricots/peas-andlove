#Pedigree Breeding Method using GEBV Selections

#load packages
library(AlphaSimR)
library(rrBLUP)
library(tidyverse)

#establish simulation parameters
genMap <- readRDS("genMap.Rdata")
haplotypes <- readRDS("srAlphaHaplo.Rdata")

founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)

SP <- SimParam$new(founderPop)
SP$addTraitAEG(10, mean=900)
SP$setVarE(h2=0.25)

#build training pop

## randomly cross 200 parents 
Parents = newPop(founderPop)
F1 = randCross(Parents, 200)

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 10)
F2 = setPheno(F2)

## select top 100 individuals from F2 bulk and self to form F3
F3Sel = selectInd(F2, 100, use="pheno", top=TRUE) 
F3 = self(F3Sel, nProgeny=30)
F3 = setPheno(F3)

##select top individuals within F3 families to form F4 ##

F4Sel = selectWithinFam(F3, 10, use="pheno", top=TRUE) 
F4 = self(F4Sel)
F4 = setPheno(F4)

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 30, use="pheno", top=TRUE)
F5 = self(F5Sel)
F5 = setPheno(F5)


## select top families from F5 for PYTs ##

PYTSel = selectFam(F5, 16, use="pheno", top=TRUE) 
PYT = self(PYTSel, nProgeny = 2)
PYT = setPheno(PYT, reps=2)

## self PYT for sufficiently large TP ##

TP = self(PYT, nProgeny = 4)

TrainingGeno <- pullSegSiteGeno(TP)
TrainingPheno <- pheno(TP)


#load in GS prediction model and use it to select parents for next cycle

#source GS Prediction Model
source("ANN_RD.R")

PYTgeno <- as.matrix(pullSegSiteGeno(PYT))
prediction = model_Final %>% predict(PYTgeno)
PYT@ebv <- prediction

cor = cor(gv(PYT), ebv(PYT))
var = varG(PYT)

newParents = selectInd(PYT, 10, use="ebv", top=TRUE)
var0 = varG(newParents)

#start new cycle

##start with 200 random crosses

F1 = randCross(newParents, 200) 
var1 = varG(F1)

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 10) 
var2 = varG(F2)

##set EBV using BLUP model##

F2geno <- as.matrix(pullSegSiteGeno(F2))
prediction = model_Final %>% predict(F2geno)
F2@ebv <- prediction

cor1 = cor(gv(F2), ebv(F2))

## select top individuals from F2 bulk  to form F3 ##

F3Sel = selectWithinFam(F2,6 , use="ebv", top=TRUE) 
F3 = self(F3Sel, nProgeny=20)
var3 = varG(F3)

##set EBV using BLUP model##
F3geno <- as.matrix(pullSegSiteGeno(F3))
prediction = model_Final %>% predict(F3geno)
F3@ebv <- prediction

cor2 = cor(gv(F3),ebv(F3))

##select top within familiy from F3 to form F4 ##

F4Sel = selectWithinFam(F3, 10, use="ebv", top=TRUE) 
F4 = self(F4Sel)
var4 = varG(F4)

##set EBV using BLUP model##
F4geno <- as.matrix(pullSegSiteGeno(F4))
prediction = model_Final %>% predict(F4geno)
F4@ebv <- prediction

cor3 = cor(gv(F4),ebv(F4))

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 10, use="ebv")
F5 = self(F5Sel, nProgeny=3)
var5 = varG(F5)

#use F5 to retrain the model

source("ANN_RD_Retrain.R")


#continue pipeline

##set EBV using BLUP model##
F5geno <- as.matrix(pullSegSiteGeno(F5))
prediction = model_Final2 %>% predict(F5geno)
F5@ebv <- prediction

cor4 = cor(gv(F5),ebv(F5))

## select top F5 families for preliminary yield trial ##

PYTSel = selectFam(F5, 4, use="ebv") 
PYT = self(PYTSel)
var6 = varG(PYT)

##set EBV using BLUP model##
PYTgeno <- as.matrix(pullSegSiteGeno(PYT))
prediction = model_Final2 %>% predict(PYTgeno)
PYT@ebv <- prediction

cor5 = cor(gv(PYT),ebv(PYT))

## select top families from PYT for AYT ##

AYTSel = selectFam(PYT,  3, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTSel)
var7 = varG(AYT)

##set EBV using BLUP model##
AYTgeno <- as.matrix(pullSegSiteGeno(AYT))
prediction = model_Final2 %>% predict(AYTgeno)
AYT@ebv <- prediction

cor6 = cor(gv(AYT),ebv(AYT))

## select top plants to form variety ##
VarietySel = selectInd(AYT, 1, use="ebv")
Variety = self(VarietySel)

## pull genetic value for each generation and write results ##

parentsgv <- as.vector(mean(gv(newParents)))
F1gv <- as.vector(mean(gv(F1)))
F2gv <- as.vector(mean(gv(F2)))
F3gv <- as.vector(mean(gv(F3)))
F4gv <- as.vector(mean(gv(F4)))
F5gv <- as.vector(mean(gv(F5)))
PYTgv <- as.vector(mean(gv(PYT)))
AYTgv <- as.vector(mean(gv(AYT)))
Varietygv <- as.vector(mean(gv(Variety)))

###list correlations to view model performacne ##
corMat <- matrix(nrow=7, ncol=1)
corMat[1,] <- cor
corMat[2,] <- cor1
corMat[3,] <- cor2
corMat[4,] <- cor3
corMat[5,] <- cor4
corMat[6,] <- cor5
corMat[7,] <- cor6

varMat <- matrix(nrow=9, ncol=1)
varMat[1,] <- var
varMat[2,] <- var0
varMat[3,] <- var1
varMat[4,] <- var2
varMat[5,] <- var3
varMat[6,] <- var4
varMat[7,] <- var5
varMat[8,] <- var6
varMat[9,] <- var7
