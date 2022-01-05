### After creating the simuPOP populations, they must be converted to QU-GENE format. This code will allow for it

library(dplyr)

folder="/Users/jennylin/PycharmProjects/new"
setwd(folder)

mypop <- read.table(file="mypop.csv", header=FALSE, sep=",", skip=1)  ### changefile

strand1 <- mypop %>% dplyr::select(3:1054)
strand2 <- mypop %>% dplyr::select(1055:2106)

marker <- paste0("a", 1:1052)

### FOR FLOWERING
#strand1 <- mypop %>% dplyr::select(3:1029)
#strand2 <- mypop %>% dplyr::select(1030:2056)

#marker <- paste0("a", 1:1027)

### FOR FINAL
#strand1 <- mypop %>% dplyr::select(3:1049)
#strand2 <- mypop %>% dplyr::select(1050:2096)

#marker <- paste0("a", 1:1047)

colnames(strand1) <- marker
colnames(strand2) <- marker

indv <- 1:200

check <- rep("s1", times=200)
s1 <- cbind(check, indv, strand1)

check <- rep("s2", times=200)
s2 <- cbind(check, indv, strand2)

final <- rbind(s1, s2) %>% arrange(by_group=indv)

final.rm <- final %>% dplyr::select(-1)

### Convert to haplotype

hap <- rowsum(final.rm, group=final.rm$indv, reorder=FALSE) %>% dplyr::select(-1)

hap[hap == 2] <- 0
hap[hap == 3] <- 1
hap[hap == 4] <- 2


##############
### GET THE LD
##############

library(LDcorSV)
library(ggplot2)

ldtest <- LD.Measures(donnees=hap)

avgld <- mean(ldtest$r2)

avgld


### basic simulation, LD is 0.04594353

### with natural selection and 50 generations, LD is 0.1532205

### with natural selection and 75 generations, LD is 0.1677289

### with natural selection and 100 generations, LD is 0.2567022

### with natural selection and 150 generations, LD is 0.2478646

### with natural selection and 200 generations, LD is 0.2440913


### In the end, I will choose the file "mypop100.csv" as the population (LD=0.2384855)




### MORE DATA

### With mutation (100 gen)--> LD=0.1271302
### With mutation (50 gen)--> LD=0.1305323
### With mutation (20 gen)--> LD=0.05961483
### With mutation (70 gen)--> LD=0.1153101

#######################################################################
### RE-FORMAT THE SIMUPOP FILE TO MATCH THE PARENTS.POP FILE IN QU-GENE
#######################################################################

parents <- read.table(file="FYParents.pop", header=FALSE,skip=19, colClasses="character")


library("tidyr")

format <- final %>% dplyr::select(3:1054)

f1 <- unite(format, chr, sep="", a1:a99)
f2 <- unite(format, chr, sep="", a100:a253)
f3 <- unite(format, chr, sep="", a254:a365)
f4 <- unite(format, chr, sep="", a366:a462)
f5 <- unite(format, chr, sep="", a463:a534)
f6 <- unite(format, chr, sep="", a535:a622)
f7 <- unite(format, chr, sep="", a623:a711)
f8 <- unite(format, chr, sep="", a712:a840)
f9 <- unite(format, chr, sep="", a841:a909)
f10 <- unite(format, chr, sep="", a910:a982)
f11 <- unite(format, chr, sep="", a983:a1052)

###FLOWERING
#format <- final %>% dplyr::select(3:1029)

#f1 <- unite(format, chr, sep="", a1:a103)
#f2 <- unite(format, chr, sep="", a104:a255)
#f3 <- unite(format, chr, sep="", a256:a356)
#f4 <- unite(format, chr, sep="", a357:a454)
#f5 <- unite(format, chr, sep="", a455:a529)
#f6 <- unite(format, chr, sep="", a530:a617)
#f7 <- unite(format, chr, sep="", a618:a700)
#f8 <- unite(format, chr, sep="", a701:a817)
#f9 <- unite(format, chr, sep="", a818:a884)
#f10 <- unite(format, chr, sep="", a885:a954)
#f11 <- unite(format, chr, sep="", a955:a1027)

reformat <- as.data.frame(cbind(f1$chr, f2$chr, f3$chr, f4$chr, f5$chr, f6$chr, f7$chr, f8$chr, f9$chr, f10$chr, f11$chr)) 

write.table(reformat, file="testpopns.pop", quote=FALSE, row.names=FALSE, col.names=FALSE)


### Add spaces after every two rows (each individual)

library(berryFunctions)

spaces <- as.vector(unlist((1:200)*3))

testinsert <- insertRows(reformat, spaces, new=NA)

write.table(testinsert, file="flowerpop.pop", quote=FALSE, na=" ", row.names=FALSE, col.names=FALSE)

### As a note, QU-GENE is very picky with the format of the pop file
### Make sure each individual genotype has an empty line between them 
### At the end of the file, there should be two empty lines at the end (the first line should have a space, but the second line should not)


### Let's verify the LD of the original parent pop files


library("stringr")

pop <- read.table("FYParents.pop", blank.lines.skip=TRUE,colClasses="character", skip=15, fill=TRUE)
for (i in 1:dim(pop)[1]) {
    pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), nrow=1, byrow=T), stringsAsFactors=FALSE)       
    write.table(pop.data, file="FYParents.split.csv", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }

pop.read <- read.table("FYParents.split.csv", header=FALSE, sep=",")
indv <- 1:200
pop.add <- cbind(indv, pop.read)
hap <- rowsum(pop.add, group=pop.add$indv, reorder=FALSE) %>% dplyr::select(-1)

hap[hap == 2] <- 0
hap[hap == 3] <- 1
hap[hap == 4] <- 2

library(LDcorSV)
library(ggplot2)

ldtest <- LD.Measures(donnees=hap)

avgld <- mean(ldtest$r2)

avgld

### the LD was 0.01006738


########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

############### 30 PARENTS


library(dplyr)

folder="/Users/jennylin/PycharmProjects/new"
setwd(folder)

mypop <- read.table(file="training.csv", header=FALSE, sep=",", skip=1)  ### changefile

### FOR FINAL
strand1 <- mypop %>% dplyr::select(3:1049)
strand2 <- mypop %>% dplyr::select(1050:2096)

marker <- paste0("a", 1:1047)

colnames(strand1) <- marker
colnames(strand2) <- marker

indv <- 1:30

check <- rep("s1", times=30)
s1 <- cbind(check, indv, strand1)

check <- rep("s2", times=30)
s2 <- cbind(check, indv, strand2)

final <- rbind(s1, s2) %>% arrange(by_group=indv)

final.rm <- final %>% dplyr::select(-1)

### Convert to haplotype

hap <- rowsum(final.rm, group=final.rm$indv, reorder=FALSE) %>% dplyr::select(-1)

hap[hap == 2] <- 0
hap[hap == 3] <- 1
hap[hap == 4] <- 2


##############
### GET THE LD
##############

library(LDcorSV)
library(ggplot2)

ldtest <- LD.Measures(donnees=hap)

avgld <- mean(ldtest$r2)

avgld


###########
### RESULTS
###########

# 10 generations --> LD=0.2022657
# 20 generations --> LD=0.2308919
# 30 generations --> LD=0.254913           ### CHOOSE THIS FILE (A.K.A. trainingG30.csv)
# 40 generations --> LD=0.2437839
# 150 generations --> LD=.05094761


#######################################################################
### RE-FORMAT THE SIMUPOP FILE TO MATCH THE PARENTS.POP FILE IN QU-GENE
#######################################################################

parents <- read.table(file="FYParents.pop", header=FALSE,skip=19, colClasses="character")


library("tidyr")

format <- final %>% dplyr::select(3:1047)

f1 <- unite(format, chr, sep="", a1:a105)
f2 <- unite(format, chr, sep="", a106:a260)
f3 <- unite(format, chr, sep="", a261:a365)
f4 <- unite(format, chr, sep="", a366:a463)
f5 <- unite(format, chr, sep="", a464:a539)
f6 <- unite(format, chr, sep="", a540:a627)
f7 <- unite(format, chr, sep="", a628:a718)
f8 <- unite(format, chr, sep="", a719:a837)
f9 <- unite(format, chr, sep="", a838:a904)
f10 <- unite(format, chr, sep="", a905:a975)
f11 <- unite(format, chr, sep="", a976:a1047)



reformat <- as.data.frame(cbind(f1$chr, f2$chr, f3$chr, f4$chr, f5$chr, f6$chr, f7$chr, f8$chr, f9$chr, f10$chr, f11$chr)) 

write.table(reformat, file="trn.pop", quote=FALSE, row.names=FALSE, col.names=FALSE)


### Add spaces after every two rows (each individual)

library(berryFunctions)

spaces <- as.vector(unlist((1:30)*3))

testinsert <- insertRows(reformat, spaces, new=NA)

write.table(testinsert, file="trn-ns.pop", quote=FALSE, na=" ", row.names=FALSE, col.names=FALSE)

### As a note, QU-GENE is very picky with the format of the pop file
### Make sure each individual genotype has an empty line between them 
### At the end of the file, there should be two empty lines at the end (the first line should have a space, but the second line should not)


### Let's verify the LD of the original parent pop files


library("stringr")

pop <- read.table("FYParents.pop", blank.lines.skip=TRUE,colClasses="character", skip=15, fill=TRUE)
for (i in 1:dim(pop)[1]) {
    pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), nrow=1, byrow=T), stringsAsFactors=FALSE)       
    write.table(pop.data, file="FYParents.split.csv", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }

pop.read <- read.table("FYParents.split.csv", header=FALSE, sep=",")
indv <- 1:200
pop.add <- cbind(indv, pop.read)
hap <- rowsum(pop.add, group=pop.add$indv, reorder=FALSE) %>% dplyr::select(-1)

hap[hap == 2] <- 0
hap[hap == 3] <- 1
hap[hap == 4] <- 2

library(LDcorSV)
library(ggplot2)

ldtest <- LD.Measures(donnees=hap)

avgld <- mean(ldtest$r2)

avgld

### the LD was 0.01006738



