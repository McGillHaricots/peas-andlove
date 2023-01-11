genMap <- readRDS("genMap.Rdata")
haplotypes <- readRDS("srnAlphaHaplo.Rdata")

founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)

SP <- SimParam$new(founderPop)
SP$addTraitA(3, mean=35)
SP$setVarE(h2=0.8)

Parents = newPop(founderPop)
F1 = randCross(Parents, 200) ##randomly cross 100 parents##

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 5)##nProgeny = number of progeny per cross## 
F2 = setPheno(F2)

F3Sel = selectFam(F2, 100, use="pheno", top=TRUE) 
F3 = self(F3Sel)
F3 = setPheno(F3)

##select top families from F3 to form F4 ##

F4Sel = selectFam(F3, 60, use="pheno", top=TRUE) 
F4 = self(F4Sel)
F4 = setPheno(F4)

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 30, use="pheno", top=TRUE)
F5 = self(F5Sel)
F5 = setPheno(F5)

PYTSel = selectFam(F5, 16, use="pheno", top=TRUE) 
PYT = self(PYTSel, nProgeny = 2)
PYT = setPheno(PYT, reps=2)

TP = self(PYT, nProgeny = 4)

TrainingGeno <- pullSegSiteGeno(TP)
TrainingPheno <- pheno(TP)

####BUILD GS PREDICTION MODEL ###

#source GS Prediction Model
source("rrBLUP_random.R")

#Select parents from previous AYT

M_PYT1 = pullSegSiteGeno(PYT)
PYT1ebv <- M_PYT1*markerEffects
PYT@ebv <- as.matrix(PYT1ebv)

newParents = selectInd(PYT, 10, use="ebv", top=TRUE)
