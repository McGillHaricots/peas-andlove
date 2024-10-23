library(heritability)
library(AGHmatrix)
library(lme4)

data = read.csv("fullDatasetSW.csv")

locs = as.list(unique(data$X1))
h2s = list()  # To store h2 values
va = list()
ve = list()
pv = list()
x = 1 # To add to h2s list 

for (loc in locs){
  Location = data[data$X1 == loc, ]
  pheno = as.numeric(Location$X3)
  pv[[x]] = var(pheno)
  labels = as.character(Location$X2)
  markers = Location[, -c(1:5)]
  M = as.matrix(markers)
  
  GM <- Gmatrix(SNPmatrix=M, maf=0.01, method="Yang")
  rownames(GM) = labels
  colnames(GM) = labels

  
  # Use tryCatch to handle errors
  result = tryCatch({
    marker_h2(pheno, labels, covariates = NULL, GM, alpha = 1, eps = .0001, 
                    max.iter = 100,fix.h2 = FALSE, h2 = 0.5)
  }, error = function(e) {
    return(NA)  # Return NA if there's an error
  })
  
  # allows loop to continue even if one loc can't achieve convergence 
  if (is.na(result)) {
    h2s[[x]] = NA
    va[[x]] = NA
    ve[[x]] = NA
  } else {
    h2s[[x]] = result$h2
    va[[x]] = result$va
    ve[[x]] = result$ve
  }
  x = x + 1
}

locationsDF = t(as.data.frame(locs))
h2DF = t(as.data.frame(h2s))

results = as.data.frame(cbind(locationsDF,pv,h2DF,va,ve))
rownames(results) = NULL
colnames(results) = c("location","pv","h2","va","ve")
results = data.frame(results)
results <- apply(results,2,as.character)
write.csv(results,"popValsSW.csv")
