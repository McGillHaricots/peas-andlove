X <- Parents
Y <- F2

AlleleFreqX <- as.data.frame(pullSegSiteHaplo(X))
AlleleFreqY <- as.data.frame(pullSegSiteHaplo(Y)) 

AlleleFreqX <- ((as.matrix(colSums(AlleleFreqX)))) 
AlleleFreqX <- (AlleleFreqX) / (2*nInd(X))
AlleleFreqY <- ((as.matrix(colSums(AlleleFreqY)))) 
AlleleFreqY <- (AlleleFreqY) /(2*nInd(Y))

AlleleFreqX <- (as.data.frame(AlleleFreqX))
AlleleFreqY <- (as.data.frame(AlleleFreqY))


nLoci <- 1047
Fc <- matrix(nrow = 1047, ncol=1)

for (i in 1:nLoci){
  
  Fc[i,] <- ((AlleleFreqX[i,]-AlleleFreqY[i,])^2)/((AlleleFreqX[i,]+AlleleFreqY[i,])/2 - (AlleleFreqX[i,]*AlleleFreqY[i,]))
  
  
}

Fc <- Fc/2
Fc <- as.data.frame(Fc)
Fc <- Fc[!sapply(Fc, is.nan)]

t = 2
S0 = 1/(2*(nInd(X)))
S1 = 1/(2*(nInd(Y)))
Z = S0+S1

Ne = abs(as.data.frame(t / (2*(Fc - Z))))
Ne = colSums(Ne) / nrow(Ne)
Ne
            
             
