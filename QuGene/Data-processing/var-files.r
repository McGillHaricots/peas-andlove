setwd("/Volumes/Seagate drive/NEW/15parents/50run-real")

library("openxlsx")
library("plyr") ; library("dplyr")
library("ggplot2")
library("readr")
library("svglite")

###########################################################################################

Strategytitle <- c("Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

###########################################################################################

#######################################################################################################################################
### FLOWERING 

trait = "DF" # can be changed
#mycycle = 5
myfile <- read.table(file= "zDF.var", header=TRUE)  # read in the file

# please note that after 5 cycles, the genetic variance goes to 0, so analysis will only be done in the first 5 cycles 
conv <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(1,3,5,7,9) & !Cycle %in% c(6,7,8,9,10)) #filter environment, trait, strategy, cycle
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Application <- as.factor(conv$Application)
conv$Strategy <- as.factor(conv$Strategy)
conv$max <- rep(as.vector(unlist(conv %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
conv$percent <- conv$VarADD.2 / conv$max * 100 # convert to percentage 



spfile <- as.data.frame(read.table(file= "zDF.var", header=TRUE)  %>% 
                        filter(Trait == trait & Strategy %in% c(1,3,5,7,9)) %>% 
                        select(2,3,4,20) %>% group_by(Strategy)) # create a dataframe for speed breeding, filter and select columns as appropriate
                      
spfile$Cycle <- spfile$Cycle/1.6 # divide the cycle by 1.6
names(spfile) <- c("Strategy", "Run", "Cycle", "Variance") # re-name the column names

justvar <- spfile[4] # select the column with genetic variance
justvar[justvar == 0.000] <- "0.000001" # if 0, cannot get an estimate for the x. therefore, change 0 to 0.000001 

sp.edit <- cbind(spfile[1:3], justvar) # add this non-zero justvar column to the dataframe

### the following code will give an estimate for the variance at each cycle (from 0 to 5) pertaining to speed breeding
mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 0)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle0 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 1)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle1 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 2)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle2 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 3)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle3 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 4)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle4 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 5)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle5 <- mylist

# the following code will summarize the speed breeding cycle estimates 
allcycle <- as.data.frame(unlist(c(cycle0, cycle1, cycle2, cycle3, cycle4, cycle5)))
allcycle$Cycle <- rep(c(0:5), each = (nrow(allcycle))/6)
allcycle$Run <- rep(c(1:50), times = (nrow(allcycle))/50 )
allcycle$Strategy <- rep(rep(c(1,3,5,7,9), each = 50), times = 6)
allcycle$Trait <- rep(trait, times=nrow(allcycle))
allcycle$Application <- rep("Speed breeding", times=nrow(allcycle))
names(allcycle) <- c("VarADD.2", "Cycle", "Run", "Strategy", "Trait", "Application")
speed <- allcycle[, c(4,3,2,5,1,6)] %>% arrange(Strategy, Run, Cycle)
speed$max <- rep(as.vector(unlist(speed %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
speed$percent <- speed$VarADD.2 / speed$max * 100

### the following code is similar to conventional, but strategies 2,4,6,8,10 are selected for genomic selection
gs <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(2,4,6,8,10) & !Cycle %in% c(6,7,8,9,10))
gs$Cycle <- gs$Cycle 
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Application <- as.factor(gs$Application)
gs$Strategy <- as.factor(gs$Strategy)
gs$max <- rep(as.vector(unlist(gs %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
gs$percent <- gs$VarADD.2 / gs$max * 100

### combine the dataframes 
dfvar <- rbind(conv, speed, gs)

### WHITE MOLD 

### the following code is for white mold 
trait = "WM"
#mycycle = 5
myfile <- read.table(file= "zWM.var", header=TRUE)  

conv <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(1,3,5,7,9) & !Cycle %in% c(6,7,8,9,10))
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Application <- as.factor(conv$Application)
conv$Strategy <- as.factor(conv$Strategy)
conv$max <- rep(as.vector(unlist(conv %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
conv$percent <- conv$VarADD.2 / conv$max * 100



spfile <- as.data.frame(read.table(file= "zWM.var", header=TRUE)  %>% 
                        filter(Trait == trait & Strategy %in% c(1,3,5,7,9)) %>% 
                        select(2,3,4,20) %>% group_by(Strategy))
                      
spfile$Cycle <- spfile$Cycle/1.6
names(spfile) <- c("Strategy", "Run", "Cycle", "Variance")

justvar <- spfile[4]
justvar[justvar == 0.000] <- "0.000001"

sp.edit <- cbind(spfile[1:3], justvar)

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 0)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle0 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 1)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle1 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 2)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle2 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 3)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle3 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 4)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle4 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 5)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle5 <- mylist

allcycle <- as.data.frame(unlist(c(cycle0, cycle1, cycle2, cycle3, cycle4, cycle5)))
allcycle$Cycle <- rep(c(0:5), each = (nrow(allcycle))/6)
allcycle$Run <- rep(c(1:50), times = (nrow(allcycle))/50 )
allcycle$Strategy <- rep(rep(c(1,3,5,7,9), each = 50), times = 6)
allcycle$Trait <- rep(trait, times=nrow(allcycle))
allcycle$Application <- rep("Speed breeding", times=nrow(allcycle))
names(allcycle) <- c("VarADD.2", "Cycle", "Run", "Strategy", "Trait", "Application")
speed <- allcycle[, c(4,3,2,5,1,6)] %>% arrange(Strategy, Run, Cycle)
speed$max <- rep(as.vector(unlist(speed %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
speed$percent <- speed$VarADD.2 / speed$max * 100


gs <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(2,4,6,8,10) & !Cycle %in% c(6,7,8,9,10))
gs$Cycle <- gs$Cycle 
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Application <- as.factor(gs$Application)
gs$Strategy <- as.factor(gs$Strategy)
gs$max <- rep(as.vector(unlist(gs %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
gs$percent <- gs$VarADD.2 / gs$max * 100


wmvar <- rbind(conv, speed, gs)

### SEED YIELD 

trait = "SY"
#mycycle = 5
myfile <- read.table(file= "zSY.var", header=TRUE)  

conv <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(1,3,5,7,9) & !Cycle %in% c(6,7,8,9,10))
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Application <- as.factor(conv$Application)
conv$Strategy <- as.factor(conv$Strategy)
conv$max <- rep(as.vector(unlist(conv %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
conv$percent <- conv$VarADD.2 / conv$max * 100



spfile <- as.data.frame(read.table(file= "zSY.var", header=TRUE)  %>% 
                        filter(Trait == trait & Strategy %in% c(1,3,5,7,9)) %>% 
                        select(2,3,4,20) %>% group_by(Strategy))
                      
spfile$Cycle <- spfile$Cycle/1.6
names(spfile) <- c("Strategy", "Run", "Cycle", "Variance")

justvar <- spfile[4]
justvar[justvar == 0.000] <- "0.000001"

sp.edit <- cbind(spfile[1:3], justvar)

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 0)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle0 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 1)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle1 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 2)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle2 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 3)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle3 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 4)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle4 <- mylist

mylist <- list()
uniq1 <- unique(unlist(sp.edit$Strategy))
uniq2 <- unique(unlist(sp.edit$Run))
for (i in 1:length(uniq1)){
	for (j in 1:length(uniq2)){		
    	varfil <- subset(sp.edit, Strategy == uniq1[i] & Run == uniq2[j])
    	getx <- approx(x = varfil$Cycle, y = varfil$Variance, xout = 5)
    	xfinal <- getx$y
    	mylist <- c(mylist, xfinal)
	}
}
cycle5 <- mylist

allcycle <- as.data.frame(unlist(c(cycle0, cycle1, cycle2, cycle3, cycle4, cycle5)))
allcycle$Cycle <- rep(c(0:5), each = (nrow(allcycle))/6)
allcycle$Run <- rep(c(1:50), times = (nrow(allcycle))/50 )
allcycle$Strategy <- rep(rep(c(1,3,5,7,9), each = 50), times = 6)
allcycle$Trait <- rep(trait, times=nrow(allcycle))
allcycle$Application <- rep("Speed breeding", times=nrow(allcycle))
names(allcycle) <- c("VarADD.2", "Cycle", "Run", "Strategy", "Trait", "Application")
speed <- allcycle[, c(4,3,2,5,1,6)] %>% arrange(Strategy, Run, Cycle)
speed$max <- rep(as.vector(unlist(speed %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
speed$percent <- speed$VarADD.2 / speed$max * 100


gs <- myfile %>% select(2,3,4,5,20) %>% filter(Trait == trait & Strategy %in% c(2,4,6,8,10) & !Cycle %in% c(6,7,8,9,10))
gs$Cycle <- gs$Cycle 
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Application <- as.factor(gs$Application)
gs$Strategy <- as.factor(gs$Strategy)
gs$max <- rep(as.vector(unlist(gs %>% filter(Cycle == 0) %>% select("VarADD.2"))), each = 6)
gs$percent <- gs$VarADD.2 / gs$max * 100


syvar <- rbind(conv, speed, gs)



###########################################################################################
### FOR THE PLOT

fullsum <- as.data.frame(rbind(dfvar, wmvar, syvar))
fullsum$Run <- as.factor(fullsum$Run)
fullsum$Cycle <- as.factor(fullsum$Cycle)
fullsum$Trait <- as.factor(fullsum$Trait)
fullsum$VarADD.2 <- as.numeric(fullsum$VarADD.2)
fullsum$percent <- as.numeric(fullsum$percent)
fullsum <- fullsum %>% filter(!Cycle == 0)

varplot <- ggplot(fullsum, aes(x=Cycle, y=percent)) + 
     facet_grid(factor(Trait, levels=c("DF","WM","SY")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_boxplot(aes(fill=Strategy), lwd=0.2) + 
     scale_fill_discrete(labels=Strategytitle) +
     scale_y_continuous(breaks=c(25,50,75,100),limits = c(0,140)) +         
     ylab("Relative genetic variance (%)") +                        
     theme_bw() +
     theme(legend.position="top")
           
#ggsave(file="var-per.png", plot=varplot, width=8, height=8)                

### ANOTHER WAY 

## Run this code for each folder 

full15 <- fullsum

#full15$no.par <- rep("15", times = nrow(full15))
#full30$no.par <- rep("30", times = nrow(full30))
#full60$no.par <- rep("60", times = nrow(full60))
#full100$no.par <- rep("100", times = nrow(full100))

fullfull <- rbind(full15, full30, full60, full100)

dffull <- fullfull %>% filter(Trait == "DF")
wmfull <- fullfull %>% filter(Trait == "WM")
syfull <- fullfull %>% filter(Trait == "SY")


###plots
dfplot <- ggplot(dffull, aes(x=Cycle, y=percent)) + 
     facet_grid(factor(no.par, levels=c("15","30","60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_boxplot(aes(fill=Strategy), lwd=0.1, outlier.size=0.05) + 
     scale_fill_discrete(labels=Strategytitle) +
     scale_y_continuous(breaks=c(25,50,75,100),limits = c(0,140)) +         
     ylab("Relative genetic variance (%)") +                        
     theme_bw() +
     theme(legend.position="top")
     
wmplot <- ggplot(wmfull, aes(x=Cycle, y=percent)) + 
     facet_grid(factor(no.par, levels=c("15","30","60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_boxplot(aes(fill=Strategy), lwd=0.1, outlier.size=0.05) + 
     scale_fill_discrete(labels=Strategytitle) +
     scale_y_continuous(breaks=c(25,50,75,100),limits = c(0,140)) +         
     ylab("Relative genetic variance (%)") +                        
     theme_bw() +
     theme(legend.position="top")     

syplot <- ggplot(syfull, aes(x=Cycle, y=percent)) + 
     facet_grid(factor(no.par, levels=c("15","30","60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_boxplot(aes(fill=Strategy), lwd=0.1, outlier.size=0.05) + 
     scale_fill_discrete(labels=Strategytitle) +
     scale_y_continuous(breaks=c(25,50,75,100),limits = c(0,140)) +         
     ylab("Relative genetic variance (%)") +                        
     theme_bw() +
     theme(legend.position="top")     
     
setwd("/Volumes/Seagate drive/NEW")     

#ggsave(file="var-df.png", plot=dfplot, width=8, height=8)
#ggsave(file="var-wm.png", plot=wmplot, width=8, height=8)
#ggsave(file="var-sy.png", plot=syplot, width=8, height=8)




