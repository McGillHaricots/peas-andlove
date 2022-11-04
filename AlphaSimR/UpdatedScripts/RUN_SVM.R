library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)


library(e1071)

#source training population 
source("buildTrainingPop.R")

#source GS Prediction Model
source("SVM_randomTRN.R")


M = pullSegSiteGeno(AYT)
AYTebv <- as.numeric(predict(svm_fit, M))
AYT@ebv <- as.matrix(AYTebv)

newParents = selectInd(AYT, 10, use="ebv", top=TRUE)

F1 = randCross(newParents, 100) ##randomly cross 0 parents##

## self and bulk F1 to form F2 ##

F2 = self(F1, nProgeny = 5) ##nProgeny = number of progeny per cross## 

##set EBV using BLUP model##
M_F2 <-pullSegSiteGeno(F2)
G_F2 = M_F2-1

EBVF2 <- as.numeric(predict(svm_fit, G_F2))

F2@ebv <- as.matrix(EBVF2)

cor1 = cor(gv(F2), ebv(F2))

## select top families to form F3 ##

F3Sel = selectFam(F2, 45, use="ebv", top=TRUE) 
F3 = self(F3Sel)

##set EBV using BLUP model##
M_F3 <-pullSegSiteGeno(F3)
G_F3 = M_F3-1
EBVF3 <-as.numeric(predict(svm_fit, G_F3))

F3@ebv <- as.matrix(EBVF3)

cor2 = cor(gv(F3),ebv(F3))

##select top families from F3 to form F4 ##

F4Sel= selectFam(F3, 30, use="ebv") 
F4 = self(F4Sel)


##set EBV using BLUP model##
M_F4 <-pullSegSiteGeno(F4)
G_F4 = M_F4-1
EBVF4 <- <-as.numeric(predict(svm_fit, G_F4))

F4@ebv <- as.matrix(EBVF4)

cor3= cor(gv(F4), ebv(F4))

## select top families from F4 to form F5 ##

F5Sel = selectFam(F4, 15, use="ebv")
F5 = self(F5Sel)


##set EBV using BLUP model##
M_F5 <-pullSegSiteGeno(F5)
G_F5 = M_F5-1
EBVF5 <- <-as.numeric(predict(svm_fit, G_F5))

F5@ebv <- as.matrix(EBVF5)

cor4 = cor(gv(F5),ebv(F5))

## select top individual from F5 to form preliminary yield trial ##

PYTSel = selectWithinFam(F5, 4, use="ebv") 
PYT = self(PYTSel)

##set EBV using BLUP model##
M_PYT <-pullSegSiteGeno(PYT)
G_PYT = M_PYT-1
EBVPYT <- <-as.numeric(predict(svm_fit, G_PYT))

PYT@ebv <- as.matrix(EBVPYT)
cor5 = cor(gv(PYT),ebv(PYT))

## select top plants from PYT to form advanced yield trial ##

AYTSel = selectInd(PYT,  20, use="ebv", reps=5, top=TRUE) 
AYT = self(AYTSel)

##set EBV using BLUP model##
M_AYT <-pullSegSiteGeno(AYT)
G_AYT = M_AYT-1
EBVAYT <- <-as.numeric(predict(svm_fit, G_AYT))

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

###list correlations to view model peormacne ##
corMat <- matrix(nrow=6, ncol=1)
corMat[1,] <- cor1
corMat[2,] <- cor2
corMat[3,] <- cor3
corMat[4,] <- cor4
corMat[5,] <- cor5
corMat[6,] <- cor6

## write files - naming convention: "model_trainingSet_descriptor_populationType_trait.csv" ###
