library(data.table)
library(ggplot2)
library(ASRgenomics)
library(AGHmatrix)
library(asreml)
library(tidyverse)
library(rrBLUP)
library(BGLR)
setDTthreads(80)

# Load full marker dataset
markers <- fread("CDBN_Filter_150Ct_MAF5per_CDBN_001_pruned_PLUS.SNPsScore.Numeric.raw")
markers <- as.data.frame(markers)
markers <- markers[, -c(1,3:6)]  # Remove unnecessary columns
colnames(markers) <- gsub("_[A-Z]$", "", colnames(markers))

# Load phenotypic data
pheno <- fread("phenotypes/DF.WYPO.EBLUPS.txt")
pheno$name<-as.factor(pheno$name)
n.pheno <- pheno$name
# ----
pheno <- pheno %>% select(c(name,DF_BLUP))

# Filter genotypes with phenotypes
markers <- markers[markers$IID %in% n.pheno,]
names <- markers$IID
markers <- markers[, -1]
rownames(markers) <- names
markers <- as.matrix(markers)
mode(markers) <- "numeric"

# Load conservation scores
conservation_scores <- fread("CDBN_prediction_weights.tsv")  # Columns: SNP, Score
conservation_scores <- conservation_scores[, c(1, 5)]

# Merge SNP conservation scores with genotype matrix
snp_info <- data.frame(SNP = colnames(markers))
snp_info <- merge(snp_info, conservation_scores, by = "SNP")

# Split SNPs into two groups
score_snps <- snp_info$SNP[snp_info$weight > 0]
NoScore_snps <- setdiff(colnames(markers), score_snps)

# Define loop parameters
num_markers <- 7000  # Subset size
n <- nrow(markers)
nt <- n * 0.3  # 30% test set
runs <- 10

# Storage for results

corr.val <- matrix(NA, nrow = runs, ncol = 10)

for (i in 1:10) {  # Loop over 10 different marker subsets
  set.seed(i)
  score_snps <- sample(score_snps, size = num_markers/2 )
  NoScore_snps <- sample(NoScore_snps, size = length(score_snps)) # subset markers with no score
  
  # Subset genotype matrix
  M_score <- markers[, score_snps, drop = FALSE]
  M_NoScore <- markers[, NoScore_snps, drop = FALSE]
  
  # Loop for prediction
  for (j in 1:runs) {
    set.seed(j + as.integer(Sys.time()))
    tst <- sample(1:n, size = nt, replace = FALSE)
    yNA <- pheno$DF_BLUP
    names(yNA) <- pheno$name
    yNA[tst] <- NA
    
    model <- BGLR(y = yNA, ETA = list(
      list(X = M_score, model = "BRR"),
      list(X = M_NoScore, model = "BRR")), nIter = 12000, burnIn = 2000, verbose = FALSE)
    
    
    # Predictions
    preds <- predict(model, classify = "name")$pvals
    preds <- preds[preds$name %in% pheno$name[tst], ]
    merged <- merge(preds, pheno[tst, c("name", "DF_BLUE")], by = "name")
    corr.val[j, i] <- cor(merged$DF_BLUE, merged$predicted.value, use = "complete.obs")    
  }
}

# Compute mean and standard errors
mean_corr <- colMeans(corr.val, na.rm = TRUE)
SE_corr <- apply(corr.val, 2, function(x) sd(x, na.rm = TRUE) / sqrt(runs))

# Create summary table
summary_table <- data.frame(
  Set = 1:10,
  mean_corr = mean_corr,
  SE_corr = SE_corr
)

# Print results
table_output <- capture.output(print(summary_table, row.names = FALSE))
writeLines(table_output)

View(summary_table)
mean(summary_table$mean_corr)


