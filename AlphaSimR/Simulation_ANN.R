## load required packages ##

library(AlphaSimR)
library(readxl)
library(writexl)
library(rrBLUP)

## IMPORTATN : working in python venv so we can use keras and tensorflow ##

Sys.setenv(RETICULATE_PYTHON = ".venv/bin/python")

## requried packages ##

library(keras)
library(readr)
library(BMTME)

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

#BUILD ANN##

          ## create GRM ##

          geno <- pullSnpGeno(F2)
          geno <- as.matrix(geno)
          GM <- tcrossprod(geno)/dim(geno)
          LG <- cholesky(GM)

          Y <- pheno(F2)
          Y <- as.matrix(Y)
          X = LG
          
          ## create data frame with geno and pheno data ##

          phenotypes <- as.data.frame(Y)
          genotypes <- as.data.frame(geno)
          data <-cbind(phenotypes,genotypes)

          ## training testing partition using BMTME ##

          data <- data.frame(GID=data)
          CrossV <- CV.KFold(data, DataSetID='GID')

          ## one holdout CV ##

          tst_set <- CrossV$CrossValidation_list[[2]]
          No_Epoch = 1000
          N_Units = 33
          X_trn = X[-tst_set,]
          X_tst = X[tst_set,]
          Y_trn = Y[-tst_set,]
          Y_tst = Y[tst_set,]

          ## Build model using pipe operator ##

          build_model <- function() {
            model <- keras_model_sequential()
            model %>%
              layer_dense(units =N_Units, activation = "relu", input_shape = c(dim
                                                                               (X_trn)[2])) %>%
              layer_dropout(rate = 0.0) %>%
              layer_dense(units = 1, activation = "linear")
            model %>% compile(
              loss = "mse",
              optimizer = "rmsprop",
              metrics = c("mse"))
            model}

          ## build and view model ##

          model <- build_model()
          model %>% summary()

          ## fitting and plotting the model ##

          print_dot_callback <- callback_lambda(
            on_epoch_end = function(epoch, logs) {
              if (epoch %% 20 == 0) cat("\n")
              cat(".")
            })

          ## inner training and validation ##

          model_fit <- model %>% fit(
            X_trn, Y_trn,
            shuffle=F,
            epochs = No_Epoch, batch_size = 640,
            validation_split = 0.2,
            verbose = 0,
            callbacks = list(print_dot_callback))

          ## plot ## 

          plot(model_fit)

          ## Refit model using early stopping ##

          early_stop <- callback_early_stopping(monitor="val_loss", mode='min', patience =50)

          model_Final <- build_model()
          model_fit_Final <- model_Final%>%fit(
            X_trn, Y_trn,
            shuffle=F,
            epochs = No_Epoch, batch_size=640,
            validation_split = 0.2,
            verbose=0, callbacks = list(early_stop, print_dot_callback)
          )

          ## plot history of training process ##

          length(model_fit_Final$metrics$mean_squared_error)
          plot(model_fit_Final)

          ## predition to see how model performs##

          prediction = model_Final %>% predict(X_tst)
          Predicted = c(prediction)
          Observed = Y_tst
          plot(Observed, Predicted)
          MSE = mean((Observed-Predicted)^2)
          MSE
          Obs_Pred = cbind(Observed, Predicted)
          colnames(Obs_Pred) = c("Observed", "Predicted")
          Obs_Pred
          
###### Model is built, continue with breeding simulation #####

## Use model to predict F2 EBV ##

prediction = model_Final %>% predict(LG)
EBV= prediction
cor1=cor(EBV, gv(F2))
F2@ebv = as.matrix(EBV)

## select top individuals to form F3 ##

F3 = selectFam(F2, 10, use="ebv") 

## Use model to predict F3 EBV ##

geno <- pullSnpGeno(F3)
geno <- as.matrix(geno)
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)
prediction = model_Final %>% predict(LG)
EBV= prediction
cor2=cor(EBV, gv(F3))
F3@ebv = as.matrix(EBV)

##select top families from F3 to form F4 ##

F4 = selectFam(F3, 7, use="ebv") 

## Use model to predict F4 EBV ##

geno <- pullSnpGeno(F4)
geno <- as.matrix(geno)
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)
prediction = model_Final %>% predict(LG)
EBV= prediction
cor3=cor(EBV, gv(F4))
F4@ebv = as.matrix(EBV)

## select top families from F4 to form F5 ##

F5 = selectFam(F4, 5, use="ebv")


## Use model to predict F5 EBV ##

geno <- pullSnpGeno(F5)
geno <- as.matrix(geno)
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)
prediction = model_Final %>% predict(LG)
EBV= prediction
cor4=cor(EBV, gv(F5))
F5@ebv = as.matrix(EBV)

## select top families from F5 to form preliminary yield trial ##

PYT = selectInd(F5, 20, use="ebv") 


## Use model to predict PYT EBV ##

geno <- pullSnpGeno(PYT)
geno <- as.matrix(geno)
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)
prediction = model_Final %>% predict(LG)
EBV= prediction
cor5=cor(EBV, gv(PYT))
PYT@ebv = as.matrix(EBV)

## select top plants from PYT to form advanced yield trial ##

AYT = selectInd(PYT,  20, use="ebv") 


## Use model to predict AYT EBV ##

geno <- pullSnpGeno(AYT)
geno <- as.matrix(geno)
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)
prediction = model_Final %>% predict(LG)
EBV= prediction
cor6=cor(EBV, gv(AYT))
AYT@ebv = as.matrix(EBV)

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
write.csv(gv, "gv_sy_sr_rrblup.csv")

cor = list(cor1, cor2, cor3, cor4, cor5, cor6)
write.csv(cor, "cor_sy_sr_ssblup.csv")
