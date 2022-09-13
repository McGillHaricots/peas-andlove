library(ggplot2)

##Average runs and plot genetic gain##

yielddata <- read.csv("BLUP_Random_Allgvs_SR_Yield.csv")
generations <- as.data.frame(yielddata[,27])
yielddata <- yielddata[,-c(1,26,27)]
repMeans <- as.data.frame(rowMeans(yielddata))
yieldAve <- cbind(repMeans, generations)
yieldAve$generation[yieldAve$generation=="pAYT"] <- "rAYT"

colnames(yieldAve) <- c("repMeans", "generation")

ggplot(yieldAve, aes(x=as.factor(generation), y=repMeans, fill=generation)) + 
  geom_boxplot(alpha=0.4) + 
  ggtitle("Yield - RRBLUP with Random TRN") +
  xlab("Generation") +
  ylab("Genetic Value") +
  theme(legend.position="none")

##plot model performance##
correlation <- read.csv("BLUP_Random_Correlation_SR_Yield.csv")
correlation <- correlation[,-c(1,26)]
corMeans <- as.data.frame(rowMeans(correlation))

corMeans$generation <- c("3","4","5","6", "7", "8")

ggplot(data=corMeans, aes(x=generation, y=correlation, group=1)) +
  geom_line(linetype="dashed") +
  geom_point() +
  ggtitle("Prediction Performance - RRBLUP") +
  xlab("Generation") +
  ylab("Prediction Performance")
