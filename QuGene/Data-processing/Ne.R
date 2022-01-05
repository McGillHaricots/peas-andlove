setwd("/Volumes/Seagate drive/NEW/15parents/50run-real")
library(dplyr)

### This is based on the moment estimator for effective population size reported in the paper by Siol et al. (doi:10.1111/j.1420-9101.2007.01409.x)

##########
### FLOWER
###################################################################################################

trait = "DF"
parents=15
freq <- read.table(file=paste0("z",trait,".fre.csv"), header= TRUE, sep=",")

###################################################################################################
### Cycle 1
mycycle = 1 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle1 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 2
mycycle = 2 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle2 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 3
mycycle = 3 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle3 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 4
mycycle = 4 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle4 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 5
mycycle = 5 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle5 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 6
mycycle = 6 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle6 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 7
mycycle = 7 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle7 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 8
mycycle = 8 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle8 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 9
mycycle = 9 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle9 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 10
mycycle = 10 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle10 <- cbind(Trait, Strategy, Run, Cycle, Neff)

final <- rbind(cycle1,cycle2,cycle3,cycle4,cycle5,cycle6,cycle7,cycle8,cycle9,cycle10)

sorted <- final %>% arrange(Trait, Strategy, Run, Cycle)
names(sorted) <-  c("Trait", "Strategy", "Run", "Cycle", "Neff")

dffull <- sorted

dfsum <- as.data.frame(sorted %>% group_by(Trait, Strategy, Cycle) %>% 
                      dplyr::summarize(mean= mean(Neff), sd= sd(Neff), n=n(), SE= sd(Neff)/sqrt(n()))) 
                      
##############
### WHITE MOLD
###################################################################################################

trait = "WM"

freq <- read.table(file=paste0("z",trait,".fre.csv"), header= TRUE, sep=",")

###################################################################################################
### Cycle 1
mycycle = 1 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle1 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 2
mycycle = 2 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle2 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 3
mycycle = 3 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle3 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 4
mycycle = 4 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle4 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 5
mycycle = 5 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle5 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 6
mycycle = 6 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle6 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 7
mycycle = 7 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle7 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 8
mycycle = 8 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle8 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 9
mycycle = 9 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle9 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 10
mycycle = 10 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle10 <- cbind(Trait, Strategy, Run, Cycle, Neff)

final <- rbind(cycle1,cycle2,cycle3,cycle4,cycle5,cycle6,cycle7,cycle8,cycle9,cycle10)

sorted <- final %>% arrange(Trait, Strategy, Run, Cycle)
names(sorted) <-  c("Trait", "Strategy", "Run", "Cycle", "Neff")

wmfull <- sorted

wmsum <- as.data.frame(sorted %>% group_by(Trait, Strategy, Cycle) %>% 
                      dplyr::summarize(mean= mean(Neff), sd= sd(Neff), n=n(), SE= sd(Neff)/sqrt(n()))) 

##############
### SEED YIELD
###################################################################################################

trait = "SY"

freq <- read.table(file=paste0("z",trait,".fre.csv"), header= TRUE, sep=",")

###################################################################################################
### Cycle 1
mycycle = 1 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle1 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 2
mycycle = 2 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle2 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 3
mycycle = 3 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle3 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 4
mycycle = 4 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle4 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 5
mycycle = 5 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle5 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 6
mycycle = 6 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle6 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 7
mycycle = 7 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle7 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 8
mycycle = 8 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle8 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 9
mycycle = 9 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle9 <- cbind(Trait, Strategy, Run, Cycle, Neff)

###################################################################################################
### Cycle 10
mycycle = 10 
mylist <- list()
uniq1 <- unique(unlist(freq$Strategy))
uniq2 <- unique(unlist(freq$Run))
for (i in 1:length(uniq1)){
for (j in 1:length(uniq2)){	
    bystrat <- subset(freq, Strategy == uniq1[i])
    byrun <- subset(bystrat, Run == uniq2[j])
    gen1 <- byrun %>% filter(Cycle == mycycle - 1) %>% select(5:7)
	gen2 <- byrun %>% filter(Cycle == mycycle) %>% select(5:7)
	gen1.edit <- reshape(gen1, idvar = "Locus", timevar = "Allele", direction = "wide")
	gen2.edit <- reshape(gen2, idvar = "Locus", timevar = "Allele", direction = "wide")
	df <- as.data.frame(cbind(gen1.edit$Locus, gen1.edit$Value.1, 
	                          gen2.edit$Value.1, gen1.edit$Value.2, gen2.edit$Value.2))
	names(df) <- c("Locus", "P_A", "P_A'", "P_a", "P_a'")
	df$V_A <- (df$"P_A" - df$"P_A'")^2/((df$"P_A'" + df$"P_A")/(2 - df$"P_A'" * df$"P_A"))
	df$V_a <- (df$"P_a" - df$"P_a'")^2/((df$"P_a'" + df$"P_a")/(2 - df$"P_a'" * df$"P_a"))
	df$Fc <- (df$V_A + df$V_a)/2
	mean.Fc <- mean(df$Fc, na.rm=TRUE)
	Ne <- 8/(2*(mean.Fc-(1/(parents*240))-(1/(parents*240))))
    mylist <- c(mylist, Ne) 
}
}
Neff <- as.data.frame(unlist(mylist))
Trait <- rep(trait, times=nrow(Neff))
Cycle <- rep(mycycle, times=nrow(Neff))
Strategy <- rep(1:10, each=(nrow(Neff)/10))
Run <- rep(1:50, times=10)
cycle10 <- cbind(Trait, Strategy, Run, Cycle, Neff)

final <- rbind(cycle1,cycle2,cycle3,cycle4,cycle5,cycle6,cycle7,cycle8,cycle9,cycle10)

sorted <- final %>% arrange(Trait, Strategy, Run, Cycle)
names(sorted) <-  c("Trait", "Strategy", "Run", "Cycle", "Neff")

syfull <- sorted

sysum <- as.data.frame(sorted %>% group_by(Trait, Strategy, Cycle) %>% 
                      dplyr::summarize(mean= mean(Neff), sd= sd(Neff), n=n(), SE= sd(Neff)/sqrt(n()))) 

###################################################################################################
### PUTTING IT TOGETHER

fullsum <- rbind(dfsum,wmsum,sysum)

#write.table(fullsum, file="Neff-sum.csv", quote=FALSE, sep=",", row.names=FALSE)

fullfull <- rbind(dffull, wmfull, syfull)

write.table(fullfull, file="Neff.csv", quote=FALSE, sep=",", row.names=FALSE)
