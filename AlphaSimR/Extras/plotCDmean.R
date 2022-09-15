F1gv <- as.data.frame(F1gv)
F1<- rep("F1", times=nrow(F1gv))
F1results <- cbind(F1gv,F1)
colnames(F1results) <- c("gv","generation")

F2gv <- as.data.frame(F2gv)
F2<- rep("F2", times=nrow(F1gv))
F2results <- cbind(F2gv,F2)
colnames(F2results) <- c("gv","generation")

F3gv <- as.data.frame(F3gv)
F3<- rep("F3", times=nrow(F3gv))
F3results <- cbind(F3gv,F3)
colnames(F3results) <- c("gv","generation")

F4gv <- as.data.frame(F4gv)
F4<- rep("F4", times=nrow(F4gv))
F4results <- cbind(F4gv,F4)
colnames(F4results) <- c("gv","generation")

F5gv <- as.data.frame(F5gv)
F5<- rep("F5", times=nrow(F5gv))
F5results <- cbind(F5gv,F5)
colnames(F5results) <- c("gv","generation")


PYTgv <- as.data.frame(PYTgv)
PYT<- rep("PYT", times=nrow(PYTgv))
PYTresults <- cbind(PYTgv,PYT)
colnames(PYTresults) <- c("gv","generation")


AYTgv <- as.data.frame(AYTgv)
AYT<- rep("rAYT", times=nrow(AYTgv))
AYTresults <- cbind(AYTgv,AYT)
colnames(AYTresults) <- c("gv","generation")


Varietygv <- as.data.frame(Varietygv)
Variety<- rep("Variety", times=nrow(Varietygv))
Varietyresults<- cbind(Varietygv,Variety)
colnames(Varietyresults) <- c("gv","generation")

AllResults <- rbind(F1results,F2results,F3results,F4results,F5results,PYTresults,AYTresults,Varietyresults)

write.csv(AllResults, "BLUP_cdmean_Allgvs_SR_Yield.csv")

ggplot(AllResults, aes(x=as.factor(generation), y=gv, fill=generation)) + 
  geom_boxplot(alpha=0.4) + 
  ggtitle("Yield - RRBLUP with CDMean TRN (SR pop)") +
  xlab("Generation") +
  ylab("Genetic Value") +
  theme(legend.position="none")

###list correlations to view model performacne ##
corMat <- matrix(nrow=6, ncol=1)
corMat[1,] <- cor1
corMat[2,] <- cor2
corMat[3,] <- cor3
corMat[4,] <- cor4
corMat[5,] <- cor5
corMat[6,] <- cor6

corMeans <- as.data.frame(corMat)
corMeans$generation <- c("3","4","5","6", "7", "8")
colnames(corMeans) <- c("generation", "correlation")

write.csv(corMeans, "rrblup_cdmean_correlation_SRN_Yield.csv")


ggplot(data=corMeans, aes(x=generation, y=correlation, group=1)) +
  geom_line(linetype="dashed") +
  geom_point() +
  ggtitle("Yield Prediction Performance - RRBLUP with CDMean TRN (SR pop)") +
  xlab("Generation") +
  ylab("Prediction Performance")
