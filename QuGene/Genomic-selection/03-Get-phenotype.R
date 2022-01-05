
strategy="/Volumes/Seagate drive/NEW/training15par"

library("data.table")
library("stringr")
library("readr")
library("dplyr")
library("stringr")
library("openxlsx")
library("tidyverse")



#####################
### GET THE PHENOTYPE
#####################

##############
### SEED YIELD

### Filter the POU files and write csv files

view <- fread(file="ztrain.pou", header=FALSE, nrows=6)
pou <- fread(file="ztrain.pou", header=FALSE, skip=2) %>% filter(V5 == "Field" & V4 == "0") %>% select(6,7)

### Note: go to the .ges file and find the residual variance for seed yield 

pheno <- rnorm(15, mean=0, sd=sqrt(26928656.00000/15))   #change this
pou.final <- cbind(pou, pheno)
colnames(pou.final) <- c("Individual", "Genotypic", "Rnorm")
pou.final$Phenotypic <- pou.final$Genotypic + pou.final$Rnorm
Pheno.final <- pou.final %>% select(1,4)
write.table(Pheno.final, sep="\t", file="SY-TRN-phenotype",row.names=FALSE)

##############
### WHITE MOLD

### Filter the POU files and write csv files

view <- fread(file="ztrain.pou", header=FALSE, nrows=6)
pou <- fread(file="ztrain.pou", header=FALSE, skip=2) %>% filter(V5 == "Field" & V4 == "0") %>% select(6,10)

### Note: go to the .ges file and find the residual variance for seed yield 

pheno <- rnorm(15, mean=0, sd=sqrt(4048.50098 /15))   #change this
pou.final <- cbind(pou, pheno)
colnames(pou.final) <- c("Individual", "Genotypic", "Rnorm")
pou.final$Phenotypic <- pou.final$Genotypic + pou.final$Rnorm
Pheno.final <- pou.final %>% select(1,4)
write.table(Pheno.final, sep="\t", file="WM-TRN-phenotype",row.names=FALSE)

#############
### FLOWERING

### Filter the POU files and write csv files

view <- fread(file="ztrain.pou", header=FALSE, nrows=6)
pou <- fread(file="ztrain.pou", header=FALSE, skip=2) %>% filter(V5 == "Field" & V4 == "0") %>% select(6,13)

### Note: go to the .ges file and find the residual variance for seed yield 

pheno <- rnorm(15, mean=0, sd=sqrt(42.47341 /15))   #change this
pou.final <- cbind(pou, pheno)
colnames(pou.final) <- c("Individual", "Genotypic", "Rnorm")
pou.final$Phenotypic <- pou.final$Genotypic + pou.final$Rnorm
Pheno.final <- pou.final %>% select(1,4)
write.table(Pheno.final, sep="\t", file="DF-TRN-phenotype",row.names=FALSE)
