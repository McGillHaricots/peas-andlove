# set working directory
setwd("/Volumes/Seagate drive/NEW/15parents/50run-real")

# load your libraries
library("openxlsx")
library("plyr") ; library("dplyr")
library("ggplot2")
library("readr")
library("svglite")

###########################################################################################

# constants (labels and colours for the strategies)
Strategytitle <- c("Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

###########################################################################################
### FLOWER

trait = "DF" # can be changed
myfile <- read.csv(file= "zDF.ham.csv", header=TRUE, stringsAsFactors=FALSE) 

conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9))
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Strategy <- as.factor(conv$Strategy)

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Application <- rep("Speed breeding", times=nrow(speed))
speed$Strategy <- as.factor(speed$Strategy)

gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10))
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Strategy <- as.factor(gs$Strategy)

comb <- rbind(conv, speed, gs)

dffull <- comb

dfsum <- as.data.frame(comb %>% group_by(Trait, Application, Strategy, Cycle) %>% 
                                dplyr::summarize(mean= mean(Value), sd= sd(Value), n=n(), SE= sd(Value)/sqrt(n())))


###########################################################################################
### WHITE MOLD

trait = "WM"
myfile <- read.csv(file= "zWM.ham.csv", header=TRUE, stringsAsFactors=FALSE) 

conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9))
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Strategy <- as.factor(conv$Strategy)

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Application <- rep("Speed breeding", times=nrow(speed))
speed$Strategy <- as.factor(speed$Strategy)

gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10))
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Strategy <- as.factor(gs$Strategy)

comb <- rbind(conv, speed, gs)
comb$Value <- 100 - comb$Value # subtract from 100 because the lower is better (note for white mold, we select on lowest disease incidence)

wmfull <- comb

wmsum <- as.data.frame(comb %>% group_by(Trait, Application, Strategy, Cycle) %>% 
                                dplyr::summarize(mean= mean(Value), sd= sd(Value), n=n(), SE= sd(Value)/sqrt(n())))

###########################################################################################
### SEED YIELD

trait = "SY"
myfile <- read.csv(file= "zSY.ham.csv", header=TRUE, stringsAsFactors=FALSE) 

conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9))
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Strategy <- as.factor(conv$Strategy)

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Application <- rep("Speed breeding", times=nrow(speed))
speed$Strategy <- as.factor(speed$Strategy)

gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10))
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Strategy <- as.factor(gs$Strategy)

comb <- rbind(conv, speed, gs)

syfull <- comb

sysum <- as.data.frame(comb %>% group_by(Trait, Application, Strategy, Cycle) %>% 
                                dplyr::summarize(mean= mean(Value), sd= sd(Value), n=n(), SE= sd(Value)/sqrt(n())))

###########################################################################################
### FOR THE PLOT

fullfull <- rbind(dffull, wmfull, syfull) %>% filter(!Cycle == 0)
fullsum <- rbind(dfsum, wmsum, sysum)  %>% filter(!Cycle == 0)
#fullsum <- rbind(dfsum, wmsum, sysum) %>% filter(Cycle %in% c(6.25, 10))

hamgraph <- ggplot(data=fullsum,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(Trait, levels=c("DF","WM","SY")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0.3,10.3)) +
     scale_y_continuous(limits = c(0,70)) +
     #ggtitle(trait, mymethod) +    
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))     
#hamgraph           

#ggsave(file="ham.png", plot=hamgraph, width=8, height=8)           

#write.table(fullsum, file="ham.csv", quote=FALSE, sep=",", row.names=FALSE)


############################################################################################
### Alternative 

# Run each as its own thing 

full15 <- fullsum

#full15$no.par <- rep("15", times = nrow(full15))
#full30$no.par <- rep("30", times = nrow(full30))
#full60$no.par <- rep("60", times = nrow(full60))
#full100$no.par <- rep("100", times = nrow(full100))

fullfull <- rbind(full15, full30, full60, full100)

dffull <- fullfull %>% filter(Trait == "DF")
wmfull <- fullfull %>% filter(Trait == "WM")
syfull <- fullfull %>% filter(Trait == "SY")


dfham <- ggplot(data=dffull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0.3,10.3)) +
     scale_y_continuous(limits = c(0,85)) +
     #ggtitle(trait, mymethod) +    
     ylab("Hamming distance") +
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))     

wmham <- ggplot(data=wmfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0.3,10.3)) +
     scale_y_continuous(limits = c(0,85)) +
     #ggtitle(trait, mymethod) +   
     ylab("Hamming distance") + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))     

syham <- ggplot(data=syfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0.3,10.3)) +
     scale_y_continuous(limits = c(0,85)) +
     ylab("Hamming distance") +       
     #ggtitle(trait, mymethod) + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))    

setwd("/Volumes/Seagate drive/NEW")            
           
#ggsave(file="ham-df.png", plot=dfham, width=8, height=8)      
#ggsave(file="ham-wm.png", plot=wmham, width=8, height=8) 
#ggsave(file="ham-sy.png", plot=syham, width=8, height=8)         
