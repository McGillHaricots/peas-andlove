genotypes <- readRDS("8C1rrblup_rd_alleles_snp_yield.rds")

nreps=14

genList=c("NP","F1","F2","F3","F4","F5","PYT","AYT","Variety")

freqList <-list()

datalist <- list()


for (i in 1:nreps) {
  for(gen in genList){
    repMat <- genotypes[[i]]
    genMat1 <- repMat[repMat$Gen==gen,]
    genMat2 <- genMat1[,-1]
    alleleSum <- as.matrix(colSums(genMat2))
    alleleFreq <- as.data.frame(alleleSum/nrow(genMat2))
    
    MAJMIN <- alleleSum
    MAJMIN[MAJMIN < (nrow(genMat2)/2)] <- 0
    MAJMIN[MAJMIN > (nrow(genMat2)/2)] <- 1
    
    majAllele <- cbind(MAJMIN, alleleFreq)
    x <- majAllele[,2]
    
    MajAlleleFreq <- as.matrix(ifelse(x<=0.5, x+1,x*(1)))
    
    freqList[[gen]] <- MajAlleleFreq
    
  }
  freqDF <- do.call(cbind, freqList)
  freqDF <- freqDF[,-1]
  datalist[[i]] <- freqList
  
}
 
