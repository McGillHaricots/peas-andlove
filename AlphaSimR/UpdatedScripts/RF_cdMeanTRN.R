library(devtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(BMTME)

###############
#Functions used
###############

# This function creates the matrix of contrast between each of the individual not in the calibration set and the mean of the population
contrasteNonPheno=function(NotSampled_f,Nind_f,Nind_in_Sample_f)
{
  mat=matrix(-1/Nind_f,Nind_f,Nind_f-Nind_in_Sample_f)
  for (i in 1:ncol(mat)) {
    mat[NotSampled_f[i],i]=1-1/Nind_f
  }
  return(mat)
}

##############################
# Data required
##########################

M = as.data.frame(TrainingGeno)
MT <- t(M)
matA1 <- as.matrix(MT)
matA1 = cov(matA1)
y <- as.matrix(TrainingPheno)
y <- t(y)
y <- cov(y)

Nind=nrow(matA1) # total number of individuals
nindrep=0.8*(nrow(M)) # Choose a size for your calibration set


lambda=7.25 # lambda is needed to estimate the CDmean


invA1=solve(y,matA1) # Inverse of the covariance matrix


##############################
# Optimization algo
##############################

Nind_in_Sample=nindrep

#Design matrices
Ident<-diag(Nind_in_Sample)
X<-rep(1,Nind_in_Sample)
M<-Ident- (X%*%solve(t(X)%*%X) %*% t(X) )

Sample1<-sample(Nind,Nind_in_Sample) #Calibration set initialization
SaveSample=Sample1
NotSampled1<-seq(1:Nind)
NotSampled<-NotSampled1[-Sample1] # Initial validation set

Z=matrix(0,Nind_in_Sample,Nind)
for (i in 1:length(Sample1)) { Z[i,Sample1[i]]=1 } 

T<-contrasteNonPheno(NotSampled,Nind,Nind_in_Sample)   # T is the matrix of contrasts

# Calculate of CDmean of the initial set
matCD<-(t(T)%*%(matA1-lambda*solve(t(Z)%*%M%*%Z + lambda*invA1))%*%T)/(t(T)%*%matA1%*%T)
CD=diag(matCD)
CDmeanSave=mean(CD)

CDmeanMax1=rep(NA,800)

# Exchange algorithm (maximize CDmean)
cpt2=1
cpt=0
while (cpt2<800) {  # Make sure that 800 is enough in your case (that you reached a plateau), for this look at CDmeanMax1.
  NotSampled=NotSampled1[-Sample1] 
  cpt2=cpt2+1
  # Remove one individual (randomly choosen) from the sample :
  Sample2=sample(Sample1,1)
  # Select one individual (randomly choosen) from the individuals that are not in the Calibration set :
  Sample3=sample(NotSampled,1)
  # New calibration set :
  Sample4=c(Sample3,Sample1[Sample1!=Sample2])
  # Calculate the mean CD of the new calibration set :
  Z=matrix(0,Nind_in_Sample,Nind)
  for (i in 1:length(Sample4)) { Z[i,Sample4[i]]=1 } 
  NotSampled=NotSampled1[-Sample4] 
  T<-contrasteNonPheno(NotSampled,Nind,Nind_in_Sample)
  
  matCD<-(t(T)%*%(matA1-lambda*solve(t(Z)%*%M%*%Z + lambda*invA1))%*%T)/(t(T)%*%matA1%*%T)
  CD=diag(matCD)
  
  CDmeanMax1[cpt2-1]=CDmeanSave
}  #Fin du while

SampleOptimiz=Sample1 # SampleOptimiz is the optimized calibration set

# End

M <- as.data.frame(TrainingGeno)
rownames(M) <- c(1:nrow(M))
OptimGeno <- M[SampleOptimiz,]

y <- as.data.frame(TrainingPheno)
OptimPheno <- y[SampleOptimiz,]


Training <- cbind(OptimPheno, OptimGeno)
colnames(Training) <- paste("ID",1:ncol(Training), sep="") ##1-605 because the SNP chip has 605 SNPs + phenotypes may have to change if you have a different num. SNPS###
##note ID1 will be the phenotype, IDs 2-606 are genotypes##


## create cross validation strategy ##
control <- trainControl(method='repeatedcv', 
                        number=10, ##will test 10 different values for mtry (number of variables for splitting) ##
                        repeats=3,
                        search = "random")        

##build model##
na.omit(Training)
rf_fit = train(ID1 ~ ., 
               data = Training, 
               method = "rf",
               tuneLength= 10,
               trControl=control) ## search a random tuning grid ##


### This command takes about 90 minutes in an compute canada interactive session ###

## look at the parameters of the model ##
print(rf) 
