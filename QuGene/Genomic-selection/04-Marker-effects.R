
############################
#PREPARE THE GENOTYPE MATRIX
############################

setwd("/Volumes/Seagate drive/NEW/training15par")

##############
### SEED YIELD

### Read in the sampled files

geno <- read.table(file="TRN-genotype", header=TRUE, sep="\t")
names(geno) <-NULL
GM <- as.matrix(geno)

pheno <- read.table(file="SY-TRN-phenotype", header=TRUE, sep="\t") %>% select(2)
names(pheno) <-NULL
PM <- as.matrix(pheno)

library("rrBLUP")

Meff <- mixed.solve(PM,Z=GM)
write.csv(Meff$u, file="SY-TRN-markereff.csv")

##############
### WHITE MOLD

### Read in the sampled files

geno <- read.table(file="TRN-genotype", header=TRUE, sep="\t")
names(geno) <-NULL
GM <- as.matrix(geno)

pheno <- read.table(file="WM-TRN-phenotype", header=TRUE, sep="\t") %>% select(2)
names(pheno) <-NULL
PM <- as.matrix(pheno)

library("rrBLUP")

Meff <- mixed.solve(PM,Z=GM)
write.csv(Meff$u, file="WM-TRN-markereff.csv")

#############
### FLOWERING

### Read in the sampled files

geno <- read.table(file="TRN-genotype", header=TRUE, sep="\t")
names(geno) <-NULL
GM <- as.matrix(geno)

pheno <- read.table(file="DF-TRN-phenotype", header=TRUE, sep="\t") %>% select(2)
names(pheno) <-NULL
PM <- as.matrix(pheno)

library("rrBLUP")

Meff <- mixed.solve(PM,Z=GM)
write.csv(Meff$u, file="DF-TRN-markereff.csv")

