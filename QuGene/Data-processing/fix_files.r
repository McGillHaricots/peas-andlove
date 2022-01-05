setwd("/Volumes/Seagate drive/NEW/15parents/50run-real") # set working directory

# load your libraries
library("openxlsx")
library("plyr") ; library("dplyr")
library("ggplot2")
library("readr")
library("svglite")

###########################################################################################

### constants (for strategies colours and labels)
Strategytitle <- c("Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

###########################################################################################
### FLOWER

trait = "DF" # can be changed for traits 
myfile <- read.csv(file= "zDF.fix.csv", header=TRUE, stringsAsFactors=FALSE) # read in file 

conv <- myfile %>% filter(Traits == trait & Strategy %in% c(1,3,5,7,9)) # filter the environment, trait, and strategy 
conv$Strategy <- as.factor(conv$Strategy) # convert to factor
conv$Application <- rep("Conventional", times=nrow(conv)) # create a new column with "Conventional"

speed <- conv # name as speed
speed$Cycle <- speed$Cycle/1.6 # divide the cycles by 1.6
speed$Strategy <- as.factor(speed$Strategy) # convert to factor
speed$Application <- rep("Speed breeding", times=nrow(speed)) # create a new column with "Speed breeding"

gs <- myfile %>% filter(Traits == trait & Strategy %in% c(2,4,6,8,10)) # filter the environment, trait, and strategy 
gs$Strategy <- gs$Strategy - 1 # change the strategy 2,4,6,8,10 to 1,3,5,7,9
gs$Strategy <- as.factor(gs$Strategy) # convert to factor
gs$Application <- rep("Genomic selection", times=nrow(gs)) # create a new column with "Genomic selection"

comb <- rbind(conv, speed, gs) # combine the three data frames

dffull <- comb # rename 

dfsum <- as.data.frame(comb %>% 
                       group_by(Traits, Application, Strategy, Cycle) %>% 
                       dplyr::summarize(mean= mean(Favorable), sd= sd(Favorable), n=n(), SE= sd(Favorable)/sqrt(n()))) # summarize the data 


###########################################################################################
### WHITE MOLD

trait = "WM"
myfile <- read.csv(file= "zWM.fix.csv", header=TRUE, stringsAsFactors=FALSE) 

conv <- myfile %>% filter(Traits == trait & Strategy %in% c(1,3,5,7,9))
conv$Strategy <- as.factor(conv$Strategy)
conv$Application <- rep("Conventional", times=nrow(conv))

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Strategy <- as.factor(speed$Strategy)
speed$Application <- rep("Speed breeding", times=nrow(speed))

gs <- myfile %>% filter(Traits == trait & Strategy %in% c(2,4,6,8,10))
gs$Strategy <- gs$Strategy - 1
gs$Strategy <- as.factor(gs$Strategy)
gs$Application <- rep("Genomic selection", times=nrow(gs))

comb <- rbind(conv, speed, gs)

wmfull <- comb

wmsum <- as.data.frame(comb %>% 
                       group_by(Traits, Application, Strategy, Cycle) %>% 
                       dplyr::summarize(mean= mean(NonFavorable), sd= sd(NonFavorable), n=n(), SE= sd(NonFavorable)/sqrt(n()))) 
                       # note pick "nonfavourable" because for white mold you select the lowest incidence 


###########################################################################################
### SEED YIELD

trait = "SY"
myfile <- read.csv(file= "zSY.fix.csv", header=TRUE, stringsAsFactors=FALSE) 

conv <- myfile %>% filter(Traits == trait & Strategy %in% c(1,3,5,7,9))
conv$Strategy <- as.factor(conv$Strategy)
conv$Application <- rep("Conventional", times=nrow(conv))

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Strategy <- as.factor(speed$Strategy)
speed$Application <- rep("Speed breeding", times=nrow(speed))

gs <- myfile %>% filter(Traits == trait & Strategy %in% c(2,4,6,8,10))
gs$Strategy <- gs$Strategy - 1
gs$Strategy <- as.factor(gs$Strategy)
gs$Application <- rep("Genomic selection", times=nrow(gs))

comb <- rbind(conv, speed, gs)

syfull <- comb

sysum <- as.data.frame(comb %>% 
                       group_by(Traits, Application, Strategy, Cycle) %>% 
                       dplyr::summarize(mean= mean(Favorable), sd= sd(Favorable), n=n(), SE= sd(Favorable)/sqrt(n())))

###########################################################################################
### FOR THE PLOT

fullsum <- rbind(dfsum, wmsum, sysum) # combine the data frames for three traits 
#fullsum <- rbind(dfsum, wmsum, sysum) %>% filter(Cycle %in% c(6.25, 10))

# please note, this graph is for all three traits. Below there is code with the traits separated
favgraph <- ggplot(data=fullsum,aes(x=Cycle, y=mean, group=Strategy)) + # make the plot
     facet_grid(factor(Traits, levels=c("DF","WM","SY")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") + # facet grid to create a grid of plots with trait by application
     geom_line(aes(color=Strategy), size=0.2) +  # create the line
     geom_point(aes(color=Strategy), size=0.8) +  # create the points 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) +  # specify the colour and label in the legend 
     ylab("Percentage of fixed genes") + # name the y axis 
     #ggtitle(trait, mymethod) +    
     scale_x_continuous(breaks=c(0:10), limits = c(-0.3,10.3)) + # define the breaks in the x axis 
     #scale_y_continuous(limits = c(0,100)) +
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) + # include the error bar
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8)) 
#favgraph         

#ggsave(file="fix.png", plot=favgraph, width=8, height=8)       

#write.table(fullsum, file="fix.csv", quote=FALSE, sep=",", row.names=FALSE)


##########################################################################################################
### ALTERNATIVE 

# Run them all and save each as its own thing

full15 <- fullsum

#full15$no.par <- rep("15", times = nrow(full15))
#full30$no.par <- rep("30", times = nrow(full30))
#full60$no.par <- rep("60", times = nrow(full60))
#full100$no.par <- rep("100", times = nrow(full100))

fullfull <- rbind(full15, full30, full60, full100)

dffull <- fullfull %>% filter(Traits == "DF")
wmfull <- fullfull %>% filter(Traits == "WM")
syfull <- fullfull %>% filter(Traits == "SY")

dffav <- ggplot(data=dffull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     ylab("Percentage of fixed genes") + 
     #ggtitle(trait, mymethod) +    
     scale_x_continuous(breaks=c(0:10), limits = c(-0.3,10.3)) +
     scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) +
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8)) 

wmfav <- ggplot(data=wmfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     ylab("Percentage of fixed genes") + 
     #ggtitle(trait, mymethod) +    
     scale_x_continuous(breaks=c(0:10), limits = c(-0.3,10.3)) +
     scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) +
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))       
           
syfav <- ggplot(data=syfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color=Strategy), size=0.2) + 
     geom_point(aes(color=Strategy), size=0.8) + 
     scale_colour_discrete(name = "Strategy", labels = Strategytitle) + 
     ylab("Percentage of fixed genes") + 
     #ggtitle(trait, mymethod) +    
     scale_x_continuous(breaks=c(0:10), limits = c(-0.3,10.3)) +
     scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) +
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.2) +
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))    
              
setwd("/Volumes/Seagate drive/NEW")           
ggsave(file="fix-df.png", plot=dffav, width=8, height=8)   # save the plots
ggsave(file="fix-wm.png", plot=wmfav, width=8, height=8)  
ggsave(file="fix-sy.png", plot=syfav, width=8, height=8)                        

