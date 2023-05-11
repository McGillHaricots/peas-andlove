#load packages
library(AlphaSimR)
library(e1071)

#establish simulation parameters
genMap <- readRDS("geneticMap.Rdata")
haplotypes <- readRDS("srnmAlphaHaplo.Rdata")

founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)

SP <- SimParam$new(founderPop)
SP$addTraitA(5, mean=35)
SP$setVarE(h2=0.85)

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
source("SVM_RD.R")

genoPYT <- pullSegSiteGeno(PYT)
colnames(genoPYT) <- paste("ID", 2:(ncol(genoPYT) +1), sep="")
EBVPYT = as.numeric(predict(SVMfit,genoPYT))
PYT@ebv <- as.matrix(EBVPYT)


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
genoF2 <- pullSegSiteGeno(F2)
colnames(genoF2) <- paste("ID", 2:(ncol(genoF2) +1), sep="")
EBVF2 = as.numeric(predict(SVMfit,genoF2))
F2@ebv <- as.matrix(EBVF2)


cor1 = cor(gv(F2), ebv(F2))

## select top individuals from F2 bulk  to form F3 ##

F3Sel = selectWithinFam(F2,6 , use="ebv", top=TRUE) 
F3 = self(F3Sel, nProgeny=20)
var3 = varG(F3)

##set EBV using BLUP model##
genoF3 <- pullSegSiteGeno(F3)
colnames(genoF3) <- paste("ID", 2:(ncol(genoF3) +1), sep="")
EBVF3 = as.numeric(predict(SVMfit,genoF3))

F3@ebv <- as.matrix(EBVF3)

cor2 = cor(gv(F3),ebv(F3))

##select top within familiy from F3 to form F4 ##

F4Sel = selectWithinFam(F3, 10, use="ebv", top=TRUE) 
F4 = self(F4Sel)
var4 = varG(F4)

##set EBV using BLUP model##
genoF4 <- pullSegSiteGeno(F4)
colnames(genoF4) <- paste("ID", 2:(ncol(genoF4) +1), sep="")
EBVF4 = as.numeric(predict(SVMfit,genoF4))


F4@ebv <- as.matrix(EBVF4)

cor3 = cor(gv(F4),ebv(F4))

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 10, use="ebv")
F5 = self(F5Sel, nProgeny=3)
var5 = varG(F5)

#use F5 to retrain the model

rm(SVMfit)
source("SVM_RD_Retrain.R")


#continue pipeline

##set EBV using BLUP model##
genoF5 <- pullSegSiteGeno(F5)
colnames(genoF5) <- paste("ID", 2:(ncol(genoF5) +1), sep="")
EBVF5 = as.numeric(predict(SVMfit,genoF5))

F5@ebv <- as.matrix(EBVF5)

cor4 = cor(gv(F5),ebv(F5))

## select top F5 families for preliminary yield trial ##

PYTSel = selectFam(F5, 4, use="ebv") 
PYT = self(PYTSel)
var6 = varG(PYT)

##set EBV using BLUP model##
genoPYT <- pullSegSiteGeno(PYT)
colnames(genoPYT) <- paste("ID", 2:(ncol(genoPYT) +1), sep="")
EBVPYT = as.numeric(predict(SVMfit,genoPYT))

PYT@ebv <- as.matrix(EBVPYT)
cor5 = cor(gv(PYT),ebv(PYT))

## select top families from PYT for AYT ##

AYTSel = selectFam(PYT,  3, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTSel)
var7 = varG(AYT)

##set EBV using BLUP model##
genoAYT <- pullSegSiteGeno(AYT)
colnames(genoAYT) <- paste("ID", 2:(ncol(genoAYT) +1), sep="")
EBVAYT = as.numeric(predict(SVMfit,genoAYT))


AYT@ebv <- as.matrix(EBVAYT)

cor6 = cor(gv(AYT),ebv(AYT))

## select top plants to form variety ##
VarietySel = selectInd(AYT, 1, use="ebv")
Variety = self(VarietySel)
var8 = varG(Variety)

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

varMat <- matrix(nrow=10, ncol=1)
varMat[1,] <- var
varMat[2,] <- var0
varMat[3,] <- var1
varMat[4,] <- var2
varMat[5,] <- var3
varMat[6,] <- var4
varMat[7,] <- var5
varMat[8,] <- var6
varMat[9,] <- var7
varMat[10,] <- var8


#write files - naming convention: "model_trainingSet_descriptor_populationType_trait.csv"
