library(data.table)
library(BGLR)
library(tidyverse)
library(AGHmatrix)
library(ASRgenomics)
setDTthreads(80)

# Load full marker dataset
markers <- fread("prunedGenotypes/CDBN_Filter_150Ct_MAF5per_CDBN_001_pruned_PLUS.SNPsScore.Numeric.raw")
markers <- as.data.frame(markers)
markers <- markers[, -c(1,3:6)]  # Remove unnecessary columns
colnames(markers) <- gsub("_[A-Z]$", "", colnames(markers))

# Load phenotypic data
pheno <- fread("SY.MTSI.EBLUPS.txt")
pheno$Taxa<-as.factor(pheno$Taxa)
n.pheno <- pheno$Taxa

# Filter genotypes with phenotypes
markers <- markers[markers$IID %in% n.pheno,]
names <- markers$IID
markers <- markers[, -1]
rownames(markers) <- names
markers <- as.matrix(markers)
mode(markers) <- "numeric"

#### When implamenting another kind of cross-validation based on population structure

# Load PCA's
pca_data <- fread("../AlphaSimulations/LDothers/CDBN_PCA.txt")  # should contain at least PC1, PC2, PC3
rownames(pca_data) <- pca_data$Taxa  # assume it has a column 'Taxa'
pca_data <- pca_data[match(rownames(markers), pca_data$Taxa), ]  # align to marker matrix

# Perform k-means clustering on PCA scores

# Ensure alignment of phenotypes and PCA
pheno <- pheno[match(pca_data$Taxa, pheno$Taxa), ]
pca_data <- pca_data[match(pheno$Taxa, pca_data$Taxa), ]
stopifnot(all(pheno$Taxa == pca_data$Taxa))  # Confirm matching

# Assign cluster to phenotype
k_clusters <- 3
set.seed(123)
clusters <- kmeans(pca_data[, c("PC1", "PC2", "PC3")], centers = k_clusters)$cluster
pheno$cluster <- clusters

# Define parameters
num_markers <- 7000
n <- nrow(markers)
nt <- floor(n * 0.3)  # 30% test set
runs <- 3

corr.val <- matrix(NA, nrow = runs * k_clusters, ncol = 10)
rownames(corr.val) <- paste0("C", rep(1:k_clusters, each = runs), "_R", rep(1:runs, times = k_clusters))

for (i in 1:10) {  # Loop over 10 marker subsets
  set.seed(i)
  selected_markers <- sample(ncol(markers), num_markers)
  M.subset <- markers[, selected_markers]
  
  # Consider clusters for training and testing
  for (c in 1:k_clusters) {
    trn_cluster <- which(pheno$cluster == c)
    tst_clusters <- which(pheno$cluster != c)
    
    # Loop for prediction
    for (j in 1:runs) {
      set.seed(j + as.integer(Sys.time()))
      
      trn_idx <- sample(trn_cluster, size = floor(0.7 * length(trn_cluster)))
      tst_idx <- sample(tst_clusters, size = floor(0.3 * length(tst_clusters)))
      
      yNA <- pheno$SY_BLUP
      yNA[tst_idx] <- NA
      
      
      # Fit Bayesian Ridge Regression using BGLR
      model <- BGLR(y = yNA, ETA = list(list(X = M.subset, model = "BRR")), nIter = 12000, burnIn = 2000, verbose = FALSE)
      
      
      # Predictions
      preds <- model$yHat[tst_idx]
      row_idx <- (c - 1) * runs + j
      corr.val[row_idx, i] <- cor(pheno$SY_BLUP[tst_idx], preds, use = "complete.obs")
    }
  }
}

# Compute mean correlation and standard errors
mean_corr <- colMeans(corr.val, na.rm = TRUE)
SE_corr <- apply(corr.val, 2, function(x) sd(x, na.rm = TRUE) / sqrt(runs))

# Create summary table
summary_table <- data.frame(Set = 1:10, mean_corr = mean_corr, SE_corr = SE_corr)

# Print results
table_output <- capture.output(print(summary_table, row.names = FALSE))
writeLines(table_output)

View(summary_table)
mean(summary_table$mean_corr)
