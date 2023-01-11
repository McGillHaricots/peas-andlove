i = 5

nIndF1 = nInd(F1)
nIndF2 = nInd(F2)
nIndF3 = nInd(F3)
nIndF4 = nInd(F4)
nIndF5 = nInd(F5)
nIndPYT = nInd(PYT)
nIndAYT = nInd(AYT)
nIndVariety = 1
nGen = 9
nModels = 6
nReps = 5

## establish empty matrices to hold outputs##
F1results <- matrix(nrow=nIndF1, ncol=nReps)
F2results <- matrix(nrow=nIndF2, ncol=nReps) 
F3results <- matrix(nrow=nIndF3, ncol=nReps) 
F4results <- matrix(nrow=nIndF4, ncol=nReps) 
F5results <- matrix(nrow=nIndF5, ncol=nReps) 
PYTresults <- matrix(nrow=nIndPYT, ncol=nReps) 
AYTresults <- matrix(nrow=nIndAYT, ncol=nReps)
Varietyresults <- matrix(nrow=nIndVariety, ncol=nReps) 
gvAveResults <- matrix(nrow=nGen, ncol=nReps)
corResults <- matrix(nrow=nModels, ncol=nReps)

repeat{

F1 = makeCross(newCycleSelections, crossPlan = cross, nProgeny = 5)

F2 = self(F1, nProgeny = 5) ##nProgeny = number of progeny per cross## 

source("SelectParentsF2.R")

source("rrBLUP_random.R")

##set EBV using BLUP model##
M_F2 <-pullSegSiteGeno(F2)
G_F2 = M_F2-1
EBVF2 <- G_F2 %*% markerEffects

F2@ebv <- as.matrix(EBVF2)

cor1 = cor(gv(F2), ebv(F2))

source("SelectParentsF2.R")

## select top families to form F3 ##

F3Sel = selectFam(F2, 40, use="ebv", top=TRUE) 
F3 = self(F3Sel)

##set EBV using BLUP model##
M_F3 <-pullSegSiteGeno(F3)
G_F3 = M_F3-1
EBVF3 <- G_F3 %*% markerEffects

F3@ebv <- as.matrix(EBVF3)

cor2 = cor(gv(F3),ebv(F3))

##select top families from F3 to form F4 ##

F4Sel = selectFam(F3, 20, use="ebv", top=TRUE) 
F4 = self(F4Sel)

##set EBV using BLUP model##
M_F4 <-pullSegSiteGeno(F4)
G_F4 = M_F4-1
EBVF4 <- G_F4 %*% markerEffects

F4@ebv <- as.matrix(EBVF4)

cor3 = cor(gv(F4),ebv(F4))
## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 10, use="ebv")
F5 = self(F5Sel, nProgeny=3)

source("rrblup_Random_Retrain.R")

##set EBV using BLUP model##
M_F5 <-pullSegSiteGeno(F5)
G_F5 = M_F5-1
EBVF5 <- G_F5 %*% markerEffects2

F5@ebv <- as.matrix(EBVF5)

cor4 = cor(gv(F5),ebv(F5))

## select top individual from F5 to form preliminary yield trial ##

PYTSel = selectWithinFam(F5, 4, use="ebv") 
PYT = self(PYTSel)

##set EBV using BLUP model##
M_PYT <-pullSegSiteGeno(PYT)
G_PYT = M_PYT-1
EBVPYT <- G_PYT %*% markerEffects2

PYT@ebv <- as.matrix(EBVPYT)
cor5 = cor(gv(PYT),ebv(PYT))

## select top plants from PYT to form advanced yield trial ##

AYTSel = selectWithinFam(PYT,  3, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTSel)

##set EBV using BLUP model##
M_AYT <-pullSegSiteGeno(AYT)
G_AYT = M_AYT-1
EBVAYT <- G_AYT %*% markerEffects2

AYT@ebv <- as.matrix(EBVAYT)

cor6 = cor(gv(AYT),ebv(AYT))

## select top plants to form variety ##
VarietySel = selectInd(AYT, 1, use="ebv")
Variety = self(VarietySel)


F1gv <- as.vector(mean(gv(F1)))
F2gv <- as.vector(mean(gv(F2)))
F3gv <- as.vector(mean(gv(F3)))
F4gv <- as.vector(mean(gv(F4)))
F5gv <- as.vector(mean(gv(F5)))
PYTgv <- as.vector(mean(gv(PYT)))
AYTgv <- as.vector(mean(gv(AYT)))
Varietygv <- as.vector(mean(gv(Variety)))

F1results[,i] = F1gv
F2results[,i] = F2gv
F3results[,i] = F3gv
F4results[,i] = F4gv
F5results[,i] = F5gv
PYTresults[,i] = PYTgv
AYTresults[,i] = AYTgv
Varietyresults[,i] = Varietygv

###list correlations to view model performacne ##
corMat <- matrix(nrow=6, ncol=1)
corMat[1,] <- cor1
corMat[2,] <- cor2
corMat[3,] <- cor3
corMat[4,] <- cor4
corMat[5,] <- cor5
corMat[6,] <- cor6

corResults[,i] <- corMat

}

if (i > 5){ ##break at number of desired reps##
  break
}
  
