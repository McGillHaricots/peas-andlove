##### For calculating precision, accuracy, recall based on class #####

## pull phenotypes from simulation at desired generation ##

pheno = pheno(F2)

pheno <- as.data.frame(pheno)

## create an IDs column so individual IDs are not lost ##

IDs<- as.data.frame(1:nrow(pheno))
pheno <- cbind(IDs,pheno)
colnames(pheno) <- c("IDs", "SY")

## order phenotypes ##

pheno <- pheno[order(pheno$SY),]

## split into 10 equal categories, creates new column ##

pheno$SYcategory <- as.factor(ifelse(pheno$SY < pheno[125,2], "1",
                                     ifelse(pheno$SY < pheno[250,2],"2",
                                            ifelse(pheno$SY < pheno[375,2],"3",
                                                   ifelse(pheno$SY < pheno[500,2],"4",
                                                          ifelse(pheno$SY < pheno[625,2],"5",
                                                                 ifelse(pheno$SY < pheno[750,2],"6",
                                                                        ifelse(pheno$SY < pheno[875,2],"7",
                                                                               ifelse(pheno$SY < pheno[1000,2],"8",
                                                                                      ifelse(pheno$SY < pheno[1125,2],"9",
                                                                                             ifelse(pheno$SY < pheno[1250,2],"10", "11")))))))))))


## re-order individuals to original order ##

pheno <-pheno[order(as.numeric(pheno$IDs)),]



