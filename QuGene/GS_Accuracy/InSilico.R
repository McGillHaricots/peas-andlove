#setwd("/Volumes/Seagate drive/NEW/15parents/50run-real")
setwd("/Volumes/Seagate drive/NEW/30parents/50run-real")
#setwd("/Volumes/Seagate drive/NEW/60parents/50run-real")
#setwd("/Volumes/Seagate drive/NEW/100parents/50run-real")

mytrait="DF"
myfile <- read.csv(file=paste0("pou-",mytrait), sep=",") %>% filter(!Cycle == "0")

#myfile$family <- c(rep(c(1:1000), each=48), rep(1000+c(1:60000), each=240))   #15 parents
myfile$family <- c(rep(c(1:1000), each=48), rep(1000+c(1:120000), each=240))  #30 parents
#myfile$family <- c(rep(c(1:1000), each=48), rep(1000+c(1:240000), each=240))   #60parents
#myfile$family <- c(rep(c(1:1000), each=48), rep(1000+c(1:400000), each=240))   #100parents

summary <- as.data.frame(myfile %>% group_by(Strategy, Run, Cycle, family) %>% summarize(Geno = mean(Genotypic)))

###################################################################################################

mystrat = 1   #change

mylist <- list()
uniq1 <- unique(unlist(summary$Cycle)) 

for (i in 1:length(uniq1)){
	CV <- summary %>% filter(Strategy == mystrat & Cycle == uniq1[i]) 
	GS <- summary %>% filter(Strategy == mystrat + 1 & Cycle == uniq1[i]) 
    mycor <- cor(CV$Geno, GS$Geno, method = c("pearson"))
    mylist <- c(mylist, mycor)
}

strat01 <- mylist

mystrat = 3   #change

mylist <- list()
uniq1 <- unique(unlist(summary$Cycle)) 

for (i in 1:length(uniq1)){
	CV <- summary %>% filter(Strategy == mystrat & Cycle == uniq1[i]) 
	GS <- summary %>% filter(Strategy == mystrat + 1 & Cycle == uniq1[i]) 
    mycor <- cor(CV$Geno, GS$Geno, method = c("pearson"))
    mylist <- c(mylist, mycor)
}

strat03 <- mylist

mystrat = 5   #change

mylist <- list()
uniq1 <- unique(unlist(summary$Cycle)) 

for (i in 1:length(uniq1)){
	CV <- summary %>% filter(Strategy == mystrat & Cycle == uniq1[i]) 
	GS <- summary %>% filter(Strategy == mystrat + 1 & Cycle == uniq1[i]) 
    mycor <- cor(CV$Geno, GS$Geno, method = c("pearson"))
    mylist <- c(mylist, mycor)
}

strat05 <- mylist

mystrat = 7   #change

mylist <- list()
uniq1 <- unique(unlist(summary$Cycle)) 

for (i in 1:length(uniq1)){
	CV <- summary %>% filter(Strategy == mystrat & Cycle == uniq1[i]) 
	GS <- summary %>% filter(Strategy == mystrat + 1 & Cycle == uniq1[i]) 
    mycor <- cor(CV$Geno, GS$Geno, method = c("pearson"))
    mylist <- c(mylist, mycor)
}

strat07 <- mylist

mystrat = 9   #change

mylist <- list()
uniq1 <- unique(unlist(summary$Cycle)) 

for (i in 1:length(uniq1)){
	CV <- summary %>% filter(Strategy == mystrat & Cycle == uniq1[i]) 
	GS <- summary %>% filter(Strategy == mystrat + 1 & Cycle == uniq1[i]) 
    mycor <- cor(CV$Geno, GS$Geno, method = c("pearson"))
    mylist <- c(mylist, mycor)
}

strat09 <- mylist


###################################################################################################

##run code above and save each strategy

Accuracy <- c(unlist(strat01),unlist(strat03),unlist(strat05),unlist(strat07),unlist(strat09))
Cycle <- rep(c(1:10), times = 5)
mytable <- as.data.frame(cbind(Cycle, Accuracy))

df.table <- mytable

#write.table(df.table, file="temp-df.csv", quote=FALSE, sep=",", row.names=FALSE)
#df.table <- read.table("temp-df.csv", sep=",", header=TRUE)
#wm.table <- read.table("temp-wm.csv", sep=",", header=TRUE)

##run all code with each trait

df.table$Trait <- rep("DF", times = nrow(df.table))
wm.table$Trait <- rep("WM", times = nrow(wm.table))
sy.table$Trait <- rep("SY", times = nrow(sy.table))

comb <- rbind(df.table, wm.table, sy.table)
comb$Strategy <- rep(rep(c(1:5), each = 10), times = 3)
comb$Strategy <- as.factor(comb$Strategy)
comb$Trait <- as.factor(comb$Trait)

#setwd("/Volumes/Seagate drive/NEW")
#write.table(comb, file="gs-acc-60.csv", quote=FALSE, sep=",", row.names=FALSE)

########################################################################################################
### SHORTCUT (READ IN DATA RIGHT AWAY)


comb15 <- read.table(file="gs-acc-15.csv", header=TRUE, sep=",")
comb30 <- read.table(file="gs-acc-30.csv", header=TRUE, sep=",")
comb60 <- read.table(file="gs-acc-60.csv", header=TRUE, sep=",")
comb100 <- read.table(file="gs-acc-100.csv", header=TRUE, sep=",")

comb15$Parents <- rep("15", times= nrow(comb15))
comb30$Parents <- rep("30", times= nrow(comb30))
comb60$Parents <- rep("60", times= nrow(comb60))
comb100$Parents <- rep("100", times= nrow(comb100))

comb <- rbind(comb15, comb30, comb60, comb100)
comb$Parents <- as.factor(comb$Parents)
comb$Trait <- as.factor(comb$Trait)
comb$Strategy <- as.factor(comb$Strategy)

#write.table(comb, file="GSaccuracy-table.csv", quote=FALSE, sep=",", row.names=FALSE)

Strategytitle <- c("Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")


                       
myplot <- ggplot(data=comb,aes(x=Cycle, y=Accuracy, group=Strategy)) + 
     facet_grid( factor(Trait, levels=c("DF","WM","SY")) ~            
                factor(Parents, levels=c("15","30","60","100")),
                scales = "free") +
     geom_line(aes(color = Strategy), size=0.4) + 
     geom_point(aes(color = Strategy), size=0.8) + 
     scale_x_continuous(breaks=c(1:10), limits = c(0.7,10.3)) +     
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) +
     theme(legend.position="top")                                             
                         
#ggsave(file="gs-accuracy-all.png", plot=myplot, width=8, height=8)  


