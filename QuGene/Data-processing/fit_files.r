### set your working directory
setwd("/Volumes/Seagate drive/NEW/15parents/50run-real")

### call up your libraries
library("openxlsx")
library("plyr") ; library("dplyr")
library("ggplot2")
library("readr")
library("svglite")

###########################################################################################

### constants (names and colours)
Strategytitle <- c("Cumulative gain","Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#000000", "#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

###########################################################################################
### FLOWER

trait = "DF" # this can be changed to the trait you want. DF = days to flowering
myfile <- read.table(file= "zDF.fit.csv", header=TRUE, sep=",")  # read in your file

### for conventional, the strategies are 1,3,5,7,9 (corresponding to mass, bulk, ssd, ped, mod ped)
conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9)) # filter the environment, traits, and strategies
conv$Cycle <- conv$Cycle + 1 
conv$Application <- rep("Conventional", times=nrow(conv)) #create new column called "Conventional"
conv$Application <- as.factor(conv$Application) #convert to factor
conv$Strategy <- as.factor(conv$Strategy) #convert to factor

### for speed breeding, same as conventional but divide the cycles by 1.6
speed <- conv # name as speed
speed$Cycle <- speed$Cycle/1.6 # divide cycles by 1.6
speed$Application <- rep("Speed breeding", times=nrow(speed)) #create new column called "Speed breeding"
speed$Application <- as.factor(speed$Application) #convert to factor
speed$Strategy <- as.factor(speed$Strategy) #convert to factor

### for genomic selection, the strategies are 2,4,6,8,10 (corresponding to mass, bulk, ssd, ped, and mod ped)
gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10)) # filter the environment, traits, and strategies
gs$Cycle <- gs$Cycle + 1
gs$Strategy <- gs$Strategy - 1 # changes strategies from 2,4,6,8,10 to 1,3,5,7,9
gs$Application <- rep("Genomic selection", times=nrow(gs))  #create new column called "Genomic selection"
gs$Application <- as.factor(gs$Application) #convert to factor
gs$Strategy <- as.factor(gs$Strategy) #convert to factor


fit <- rbind(conv, speed, gs) # combine the fit conventional, speed, and genomic selection 

v1 <- fit$Value # create a duplicate column for fitness value called v1
v2 <- c(fit$Value[-1], 0) # create a duplicate column called v2 with the column shifted down
v3 <- fit$ValueAD # create a duplicate column for the adjusted fitness value called v3
v4 <- c(fit$ValueAD[-1], 0) # create a duplicate column called v4 with the column shifted down
GenGain <- cbind(fit, v1, v2, v3, v4) # create a new dataframe with these new columns
GenGain$Delta <- GenGain$v2 - GenGain$v1 # obtain the genetic gain by subtracting column v1 from v2
GenGain$DeltaAD <- GenGain$v4 - GenGain$v3 # obtain the adjusted genetic gain by subtracting column v3 from v4

GenGain$Delta[GenGain$Delta < 0] <- 0   # convert negative values to 0
GenGain$DeltaAD[GenGain$DeltaAD < 0] <- 0   # convert negative values to 0

dffull <- GenGain # rename as dffull

fitsummary <- as.data.frame(GenGain %>% group_by(Application, Trait, Strategy, Cycle) %>% # group the data
                      dplyr::summarize(mean= mean(DeltaAD), sd= sd(DeltaAD), n=n(), SE= sd(DeltaAD)/sqrt(n())) %>% # summarize the data to get the mean, standard deviation, and standard error
                      filter(!Cycle %in% c(6.875,11))) # remove the last column since it was added 

cumulative <- as.data.frame(GenGain %>% group_by(Application) %>% filter(!Cycle %in% c(6.875, 11)) %>% dplyr::select(2,3,4,6,9,15)) # create new dataframe and select the columns
cumulative$csum <- ave(cumulative$DeltaAD, cumulative$Application, cumulative$Strategy, cumulative$Run, FUN=cumsum) # get the cumulative sum for the genetic gain as a column called csum
cumulative$Run <- as.factor(cumulative$Run) # convert to factor                     
fitaverage <- as.data.frame(cumulative %>% group_by(Application, Trait, Cycle) %>% # group the data
                      dplyr::summarize(mean= mean(csum)) %>% # get the average
                      filter(!Cycle %in% c(6.875,11)))  # remove the last column since it was added
fitaverage$Strategy <- rep("-1", times = nrow(fitaverage))    # call the strategy average "-1" in the strategy column
fitaverage <- fitaverage[,c(1,2,5,3,4)]       # rearrage the columns 
fitaverage$sd <- rep(0, times = nrow(fitaverage)) # arbitrarily put "0" in the sd column
fitaverage$n <- rep(50, times = nrow(fitaverage)) # arbitrarily put "50" in the n column
fitaverage$SE <- rep(0, times = nrow(fitaverage)) # arbitrarily put "0" in the SE column                    

dfsum <- rbind(fitaverage, fitsummary) %>% arrange(Application, Strategy) # combine the data
               

###########################################################################################
### WHITE MOLD

### the following data is a repeat of the above, but for WM
trait = "WM"
myfile <- read.table(file= "zWM.fit.csv", header=TRUE, sep=",")  

conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9)) 
conv$Cycle <- conv$Cycle + 1
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Application <- as.factor(conv$Application)
conv$Strategy <- as.factor(conv$Strategy)

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Application <- rep("Speed breeding", times=nrow(speed))
speed$Application <- as.factor(speed$Application)
speed$Strategy <- as.factor(speed$Strategy)

gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10)) 
gs$Cycle <- gs$Cycle + 1
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Application <- as.factor(gs$Application)
gs$Strategy <- as.factor(gs$Strategy)


fit <- rbind(conv, speed, gs)

v1 <- fit$Value
v2 <- c(fit$Value[-1], 0)
v3 <- fit$ValueAD
v4 <- c(fit$ValueAD[-1], 0)
GenGain <- cbind(fit, v1, v2, v3, v4)
GenGain$Delta <- GenGain$v1 - GenGain$v2
GenGain$DeltaAD <- GenGain$v3 - GenGain$v4

GenGain$Delta[GenGain$Delta < 0] <- 0   
GenGain$DeltaAD[GenGain$DeltaAD < 0] <- 0   

wmfull <- GenGain

fitsummary <- as.data.frame(GenGain %>% group_by(Application, Trait, Strategy, Cycle) %>% 
                      dplyr::summarize(mean= mean(DeltaAD), sd= sd(DeltaAD), n=n(), SE= sd(DeltaAD)/sqrt(n())) %>% 
                      filter(!Cycle %in% c(6.875,11)))

cumulative <- as.data.frame(GenGain %>% group_by(Application) %>% filter(!Cycle %in% c(6.875, 11)) %>% dplyr::select(2,3,4,6,9,15))
cumulative$csum <- ave(cumulative$DeltaAD, cumulative$Application, cumulative$Strategy, cumulative$Run, FUN=cumsum)
cumulative$Run <- as.factor(cumulative$Run) 
fitaverage <- as.data.frame(cumulative %>% group_by(Application, Trait, Cycle) %>% 
                      dplyr::summarize(mean= mean(csum)) %>%
                      filter(!Cycle %in% c(6.875,11)))         
fitaverage$Strategy <- rep("-1", times = nrow(fitaverage))    
fitaverage <- fitaverage[,c(1,2,5,3,4)]       
fitaverage$sd <- rep(0, times = nrow(fitaverage))
fitaverage$n <- rep(50, times = nrow(fitaverage))
fitaverage$SE <- rep(0, times = nrow(fitaverage))                             
              
wmsum <- rbind(fitaverage, fitsummary) %>% arrange(Application, Strategy)


###########################################################################################
### SEED YIELD

### the following data is a repeat of the above, but for SY
trait = "SY"
myfile <- read.table(file= "zSY.fit.csv", header=TRUE, sep=",")  

conv <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(1,3,5,7,9)) 
conv$Cycle <- conv$Cycle + 1
conv$Application <- rep("Conventional", times=nrow(conv))
conv$Application <- as.factor(conv$Application)
conv$Strategy <- as.factor(conv$Strategy)

speed <- conv
speed$Cycle <- speed$Cycle/1.6
speed$Application <- rep("Speed breeding", times=nrow(speed))
speed$Application <- as.factor(speed$Application)
speed$Strategy <- as.factor(speed$Strategy)

gs <- myfile %>% filter(Environment == "Field" & Trait == trait & Strategy %in% c(2,4,6,8,10)) 
gs$Cycle <- gs$Cycle + 1
gs$Strategy <- gs$Strategy - 1
gs$Application <- rep("Genomic selection", times=nrow(gs))
gs$Application <- as.factor(gs$Application)
gs$Strategy <- as.factor(gs$Strategy)


fit <- rbind(conv, speed, gs)

v1 <- fit$Value
v2 <- c(fit$Value[-1], 0)
v3 <- fit$ValueAD
v4 <- c(fit$ValueAD[-1], 0)
GenGain <- cbind(fit, v1, v2, v3, v4)
GenGain$Delta <- GenGain$v2 - GenGain$v1
GenGain$DeltaAD <- GenGain$v4 - GenGain$v3

GenGain$Delta[GenGain$Delta < 0] <- 0   
GenGain$DeltaAD[GenGain$DeltaAD < 0] <- 0   

syfull <- GenGain

fitsummary <- as.data.frame(GenGain %>% group_by(Application, Trait, Strategy, Cycle) %>% 
                      dplyr::summarize(mean= mean(DeltaAD), sd= sd(DeltaAD), n=n(), SE= sd(DeltaAD)/sqrt(n())) %>% 
                      filter(!Cycle %in% c(6.875,11)))

cumulative <- as.data.frame(GenGain %>% group_by(Application) %>% filter(!Cycle %in% c(6.875, 11)) %>% dplyr::select(2,3,4,6,9,15))
cumulative$csum <- ave(cumulative$DeltaAD, cumulative$Application, cumulative$Strategy, cumulative$Run, FUN=cumsum)
cumulative$Run <- as.factor(cumulative$Run) 
fitaverage <- as.data.frame(cumulative %>% group_by(Application, Trait, Cycle) %>% 
                      dplyr::summarize(mean= mean(csum)) %>%
                      filter(!Cycle %in% c(6.875,11)))         
fitaverage$Strategy <- rep("-1", times = nrow(fitaverage))    
fitaverage <- fitaverage[,c(1,2,5,3,4)]       
fitaverage$sd <- rep(0, times = nrow(fitaverage))
fitaverage$n <- rep(50, times = nrow(fitaverage))
fitaverage$SE <- rep(0, times = nrow(fitaverage))                           
              
sysum <- rbind(fitaverage, fitsummary) %>% arrange(Application, Strategy)

###########################################################################################
### FOR THE PLOT

### combine the data 
fullsum <- rbind(dfsum, wmsum, sysum)

fullfull <- rbind(dffull, wmfull, syfull)

### note this is to create a graph with each trait
gain <- ggplot(data=fullsum,aes(x=Cycle, y=mean, group=Strategy)) + # plot the graph 
     facet_grid(factor(Trait, levels=c("DF","WM","SY")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") + # the facet grid 
     geom_line(aes(color = Strategy), size=0.4) + # create the linegraph
     geom_point(aes(color = Strategy), size=0.8) + # create the points 
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) + # label the legend 
     ylab("\u0394G%") + # label the y axis
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.3) + # include error bars
     #geom_vline(xintercept = cyclemean, 
     #           linetype="dotted", color = colours, size=0.6) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0,10.3)) + # define the breaks in the x axis 
     #scale_y_continuous(limits=c(-3.5,maxvalue)) +
     #ggtitle(trait, mymethod) +           
     theme_bw() + # make the theme
     theme(legend.position = "top", # place the legend at the top
           legend.text=element_text(size=8))  # make the legend smaller      

#gain

#ggsave(file="fit.png", plot=gain, width=8, height=8)             

###########################################################################################
### FOR THE PLOT
### ALTERNATIVE

full15 <- fullsum

#full15$no.par <- rep("15", times = nrow(full15))
#full30$no.par <- rep("30", times = nrow(full30))
#full60$no.par <- rep("60", times = nrow(full60))
#full100$no.par <- rep("100", times = nrow(full100))

fullfull <- rbind(full15, full30, full60, full100)

dffull <- fullfull %>% filter(Trait == "DF")
wmfull <- fullfull %>% filter(Trait == "WM")
syfull <- fullfull %>% filter(Trait == "SY")

### this plots the the parental population size by the application
dfgain <- ggplot(data=dffull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color = Strategy), size=0.4) + 
     geom_point(aes(color = Strategy), size=0.8) + 
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) +
     ylab("\u0394G%") + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.3) + 
     #geom_vline(xintercept = cyclemean, 
     #           linetype="dotted", color = colours, size=0.6) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0,10.3)) +
     scale_y_continuous(breaks=c(10,20,30,40,50), limits=c(0, 55)) +
     #ggtitle(trait, mymethod) +           
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))   

           
wmgain <- ggplot(data=wmfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color = Strategy), size=0.4) + 
     geom_point(aes(color = Strategy), size=0.8) + 
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) +
     ylab("\u0394G%") + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.3) + 
     #geom_vline(xintercept = cyclemean, 
     #           linetype="dotted", color = colours, size=0.6) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0,10.3)) +
     scale_y_continuous(breaks=c(10,20,30,40,50), limits=c(0, 55)) +
     #ggtitle(trait, mymethod) +           
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))              
           
sygain <- ggplot(data=syfull,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(factor(no.par, levels=c("15", "30", "60", "100")) ~ 
                factor(Application, levels=c("Conventional","Speed breeding","Genomic selection")), 
                scales = "free") +
     geom_line(aes(color = Strategy), size=0.4) + 
     geom_point(aes(color = Strategy), size=0.8) + 
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) +
     ylab("\u0394G%") + 
     geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), color="grey35", size=0.2, width=0.3) + 
     #geom_vline(xintercept = cyclemean, 
     #           linetype="dotted", color = colours, size=0.6) + 
     scale_x_continuous(breaks=c(0:10), limits = c(0,10.3)) +
     scale_y_continuous(breaks=c(10,20,30,40,50), limits=c(0, 55)) +
     #ggtitle(trait, mymethod) +           
     theme_bw() +
     theme(legend.position = "top",
           legend.text=element_text(size=8))              
           
setwd("/Volumes/Seagate drive/NEW")           
ggsave(file="fit-df.png", plot=dfgain, width=8, height=8)   # save the plots
ggsave(file="fit-wm.png", plot=wmgain, width=8, height=8)  
ggsave(file="fit-sy.png", plot=sygain, width=8, height=8)                   


