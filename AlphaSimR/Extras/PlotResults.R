library(ggplot2)

##Average runs and plot genetic gain##

yielddata <- read.csv("RRBLUP_random_Allgvs_SRNM_Yield.csv")
generations <- as.data.frame(yielddata[,27])
yielddata <- yielddata[,-c(1,26,27)]
repMeans <- as.data.frame(rowMeans(yielddata))
yieldAve <- cbind(repMeans, generations)

colnames(yieldAve) <- c("repMeans", "generation")
yieldAve$generation[yieldAve$generation=="AYT"] <- "rAYT"

##Calculate Genetic Gain##
F1 <- yieldAve[yieldAve$generation=="F1",]
F1 <- F1[,1]
F1 <- mean(F1)
Variety <- (yieldAve[yieldAve$generation=="Variety",])
Variety <- Variety[,1]
Variety <- as.numeric(Variety)
GeneticGain <- Variety-F1
GeneticGain

##Plot Genetic Gain by Generation
ggplot(yieldAve, aes(x=as.factor(generation), y=repMeans, fill=generation)) + 
  geom_boxplot(alpha=0.4) + 
  ggtitle("Yield - RRBLUP with random TRN (SR pop)") +
  xlab("Generation") +
  ylab("Genetic Value") +
  theme(legend.position="none")

##plot model performance##
correlation <- read.csv("SVM_stratClusters_correlation_SR_Flowering.csv")
correlation <- correlation[,-c(1,26)]
corMeans <- as.data.frame(rowMeans(correlation))

corMeans$generation <- c("3","4","5","6", "7", "8")
colnames(corMeans) <- c("correlation","generation")

ggplot(data=corMeans, aes(x=generation, y=correlation, group=1)) +
  geom_line(linetype="dashed") +
  geom_point() +
  ggtitle("Prediction PerformanceSVM_random_correlation_SR_Flowering") +
  xlab("Generation") +
  ylab("Prediction Performance")
