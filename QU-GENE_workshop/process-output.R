
### INSTALL THE NEEDED PACKAGES 
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggpubr")

library("dplyr")
library("ggplot2")
library("ggpubr")

### SET YOUR WORKING DIRECTORY
folder = "C:/Users/vhvlo/OneDrive/Documents/JennyLin/QU-GENE-Workshop"
setwd(folder)

### INPUT YOUR TRAIT OF INTEREST
mytrait = "SY"

### INPUT YOUR ENVIRONMENT OF INTEREST
myenv = "Field"

fitfile = list.files(folder, pattern = ".fit")
fixfile = list.files(folder, pattern = ".fix")
hamfile = list.files(folder, pattern = ".ham")
varfile = list.files(folder, pattern = ".var")

############################################################################
### FIT FILE

fit <- read.table(file= fitfile, header=TRUE, sep=",")  %>% 
                  filter(Environment == myenv & 
                  Trait == mytrait) 

cols.num <- c("Model", "Strategy", "Environment", "Trait")
fit[cols.num] <- lapply(fit[cols.num],as.factor)
cols.num <- c("Value", "ValueAD", "Run", "Cycle")
fit[cols.num] <- lapply(fit[cols.num],as.numeric)

v1 <- fit$Value
v2 <- c(fit$Value[-1], 0)
v3 <- fit$ValueAD
v4 <- c(fit$ValueAD[-1], 0)
GenGain <- cbind(fit, v1, v2, v3, v4)
GenGain$Delta <- GenGain$v2 - GenGain$v1
GenGain$DeltaAD <- GenGain$v4 - GenGain$v3

fitsummary <- as.data.frame(GenGain %>% group_by(Strategy, Cycle) %>% 
                            dplyr::summarize(mean= mean(Delta), 
                                             sd= sd(Delta), 
                                             n=n(), 
                                             SE= sd(Delta)/sqrt(n())) %>% 
                            filter(Cycle < max(fit$Cycle)))
fitsummary$Cycle <- fitsummary$Cycle + 1

fitplot <- ggplot(data=fitsummary,aes(x=Cycle, y=mean, group=Strategy)) + 
     geom_line(aes(color=Strategy), size=0.4) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     ylab(paste0("Genetic gain ")) + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), 
                   color="grey35", size=0.2, width=0.3) + 
     scale_x_continuous(breaks = seq(0, max(fitsummary$Cycle), by = 1), 
                        limits = c(0, max(fitsummary$Cycle))) +
     scale_y_continuous(limits=c(0,max(fitsummary$mean)*2)) +  
     theme_bw() 

############################################################################
### FIX FILE

fix <- read.csv(fixfile, header=TRUE, stringsAsFactors=FALSE) %>% 
                filter(Traits == mytrait)
fix$Strategy <- as.factor(fix$Strategy)

fixsummary <- as.data.frame(fix %>% group_by(Strategy, Cycle) %>% 
                            dplyr::summarize(mean= mean(Favorable), 
                                             sd= sd(Favorable), 
                                             n=n(), 
                                             SE= sd(Favorable)/sqrt(n())))

fixplot <- ggplot(data=fixsummary,aes(x=Cycle, y=mean, group=Strategy)) + 
     geom_line(aes(color=Strategy), size=0.4) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     ylab("Percent of fixed genes") + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), 
                       color="grey35", size=0.2, width=0.2) +
     scale_x_continuous(breaks = seq(0, max(fitsummary$Cycle), by = 1))+
     theme_bw()

############################################################################
### HAM FILE

ham <- read.csv(hamfile, header=TRUE, stringsAsFactors=FALSE) %>% 
                filter(Environment == myenv & 
                       Trait == mytrait)

cols.num <- c("Model", "Strategy", "Environment", "Trait")
ham[cols.num] <- lapply(ham[cols.num],as.factor)

ham$Strategy <- as.factor(ham$Strategy)

hamsummary <- as.data.frame(ham %>% group_by(Strategy, Cycle) %>% 
                            dplyr::summarize(mean= mean(Value), 
                                             sd= sd(Value), 
                                             n=n(), 
                                             SE= sd(Value)/sqrt(n())))

hamplot <- ggplot(data=hamsummary,aes(x=Cycle, y=mean, group=Strategy)) + 
     geom_line(aes(color=Strategy), size=0.4) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     ylab("Hamming distance") +  
     scale_y_continuous(limits = c(0,max(hamsummary$mean))) +
     scale_x_continuous(breaks = seq(0, max(fixsummary$Cycle), by = 1)) +  
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), 
                       color="grey35", size=0.2, width=0.2) +
     theme_bw()
           
############################################################################
### VAR FILE

var <- read.table(file= varfile, header=TRUE) %>% 
                  select(contains(c("Model","Strategy", 
                                    "Run", "Cycle", "Trait", myenv)))

newcol <- as.vector(unlist(slice(var,1)))
var <- slice(var,-1) %>% filter(Trait == mytrait)
names(var) <- newcol

cols.num <- c("Model", "Strategy", "Trait")
var[cols.num] <- lapply(var[cols.num],as.factor)

cols.num <- c(newcol[-c(1:5)])
var[cols.num] <- lapply(var[cols.num],as.numeric)

varplot <- ggplot(var, aes(x = Cycle, y = VarADD, fill = Strategy)) + 
    geom_boxplot(lwd=0.2) + 
    ylab("Genetic variance") +                       
    theme_bw() 

############################################################################

ggarrange(fitplot, fixplot, hamplot, varplot, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

