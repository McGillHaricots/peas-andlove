genotypes <- readRDS("8C1rrblup_rd_alleles_snp_yield.rds")

nreps=14
genList = c("NP","F1","F2", "F3", "F4", "F5","PYT","AYT","Variety")
AveFreqMat <- as.data.frame(matrix(nrow=3547,ncol=9))
colnames(AveFreqMat) <-  c("NP","F1","F2", "F3", "F4", "F5","PYT","AYT","Variety")

C1Freq = vector("list", length = nreps)


for (i in 1:nreps) {
  repMat <- genotypes[[i]]
  
  for (gen in genList) {
  genMat <- paste0("gen",gen)
  genMat2 <- repMat[repMat$Gen=="gen",] 
  genMat3 <- genMat2[,-1]
  alleleSum <- as.matrix(colSums(genMat3))
  alleleFreq <- as.data.frame(alleleSum/nrow(genMat3))
  
  MAJMIN <- alleleSum
  MAJMIN[MAJMIN < (nrow(genMat)/2)] <- 0
  MAJMIN[MAJMIN > (nrow(genMat)/2)] <- 1
  
  majAllele <- cbind(MAJMIN, alleleFreq)
  x <- as.vector(majAllele[,2])
  
  MajAlleleFreq <-- as.matrix(ifelse(x>=0.5, x-1,x*(-1)))
  AveFreqMat$gen <- MajAlleleFreq 
}
C1Freq[[i]] <- AveFreqMat
}
}
