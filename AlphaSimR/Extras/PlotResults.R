library(ggplot2)

###PLOT GENETIC GAIN###

F1 <- read.csv("BLUP_Random_F1gv_SR_Yield.csv")
F2 <- read.csv("BLUP_Random_F2gv_SR_Yield.csv")
F3 <- read.csv("BLUP_Random_F3gv_SR_Yield.csv")
F4 <- read.csv("BLUP_Random_F4gv_SR_Yield.csv")
F5 <- read.csv("BLUP_Random_F5gv_SR_Yield.csv")
PYT <- read.csv("BLUP_Random_PYT_SR_Yield.csv")
TAdv <- read.csv("BLUP_Random_AYT_SR_Yield.csv")


F1$gen <- rep("F1", times=nrow(F1))
F2$gen <- rep("F2", times=nrow(F2))
F3$gen <- rep("F3", times=nrow(F3))
F4$gen <- rep("F4", times=nrow(F4))
F5$gen <- rep("F5", times=nrow(F5))
PYT$gen <- rep("PYT", times=nrow(PYT))
TAdv$gen <- rep("TAdv", times=nrow(TAdv))
Variety$gen <- rep("Variety", times=nrow(Variety))

geneticgain <- rbind(F1,F2,F3,F4,F5,PYT,TAdv,Variety)

ggplot(geneticgain, aes(x=as.factor(gen), y=Trait1, fill=gen)) + 
  geom_boxplot(alpha=0.4) + 
  ggtitle("Yield - RRBLUP") +
  xlab("Generation") +
  ylab("Genetic Value") +
  theme(legend.position="none")


####PLOT PREDICTION PERFORMANCE####

correlation <- read.csv("BLUP_Random_COR_SR_Yield.csv")
correlation <- correlation[,-1]
colnames(correlation) <- c("F3", "F4", "F5", "PYT", "TAdv", "Variety")
rownames(correlation) <- c("correlation")
correlation <- as.data.frame(t(correlation))
correlation$gen <- c("3","4","5","6", "7", "8")

ggplot(data=correlation, aes(x=gen, y=correlation, group=1)) +
  geom_line(linetype="dashed") +
  geom_point() +
  ggtitle("Prediction Performance - RRBLUP") +
  xlab("Generation") +
  ylab("Prediction Performance")
