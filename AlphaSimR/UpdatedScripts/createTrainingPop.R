genMap <- readRDS("genMap.Rdata")
haplotypes <- readRDS("srAlphaHaplo.Rdata")

founderPop = newMapPop(genMap, 
                       haplotypes, 
                       inbred = FALSE, 
                       ploidy = 2L)

SP <- SimParam$new(founderPop)
SP$addTraitADE(10, mean=1350)
SP$setVarE(h2=0.3)

Parents = newPop(founderPop)
F1 = randCross(Parents, 100) ##randomly cross 100 parents##

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 5)##nProgeny = number of progeny per cross## 
F2 = setPheno(F2)

F3Sel = selectFam(F2, 50, use="pheno", top=TRUE) 
F3 = self(F3Sel,nProgeny = 4)
F3 = setPheno(F3Sel)

##select top families from F3 to form F4 ##

F4Sel = selectFam(F3, 30, use="pheno", top=TRUE) 
F4 = self(F4Sel,nProgeny = 4)
F4 = setPheno(F4Sel)

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 15, use="pheno", top=TRUE)
F5 = self(F5Sel, nProgeny = 4)
F5 = setPheno(F5Sel)

PYTSel = selectFam(F5, 8, use="pheno", top=TRUE) 
PYT = self(PYTSel, nProgeny=4)
PYT = setPheno(PYTSel, reps=2)

AYTSel = selectWithinFam(PYT, 5, use="pheno", top=TRUE)
AYT = self(AYTSel, nProgeny=4)
AYT = setPheno(AYTSel, reps=4)

TrainingPop <- self(AYT, nProgeny=5)

