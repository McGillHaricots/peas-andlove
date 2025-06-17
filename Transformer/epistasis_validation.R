
# Transform genotypes using PLINK

geno <- read.csv("BLB_19_geno.csv", row.names = 1, check.names = FALSE)
ids <- paste0("ID", seq_len(nrow(geno)))
major_allele <- "A"
minor_allele <- "G"

expand_genotype <- function(x) {
  if (is.na(x)) return(c("0", "0"))
  switch(as.character(x),
         "0" = c(major_allele, major_allele),
         "1" = c(major_allele, minor_allele),
         "2" = c(minor_allele, minor_allele),
         c("0", "0"))
}

geno_alleles <- t(apply(geno, 1, function(row) unlist(sapply(row, expand_genotype))))       
ped <- data.frame(
  FID = ids,
  IID = ids,
  PID = 0,
  MID = 0,
  SEX = 1,
  PHENO = -9
)
ped <- cbind(ped, geno_alleles)
write.table(ped, file = "output.ped", quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

snp_names <- colnames(geno)
chr_pos <- do.call(rbind, lapply(snp_names, function(snp) {
  parts <- unlist(strsplit(snp, "[_.]"))  # split on . and _
  chr <- gsub("S", "", parts[2])
  pos <- parts[3]
  return(c(chr, snp, 0, pos))
}))

map <- as.data.frame(chr_pos, stringsAsFactors = FALSE)
colnames(map) <- c("CHR", "SNP", "CM", "POS")
write.table(map, file = "output.map", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)

quit()

#do in terminal command line                        
plink --file output --make-bed --out BLB_19_geno

#back to R
R                        
                        
install.packages(c("lars","RcppEigen","Rcpp","doParallel","data.table","MASS","openxlsx",
"BEDMatrix","bigmemory","stringr","biglasso","progress","ncvreg","coin","sampling","sbl")) 


install.packages("IIIVmrMLM/IIIVmrMLM", repos = NULL, type = "source")

library(IIIVmrMLM)
                          
epistasis <- IIIVmrMLM(
  fileGen = "/lustre06/project/6044053/ich/CDBN/BLB_19_geno",  # no extension here!
  filePhe = "/lustre06/project/6044053/ich/CDBN/BLB_19_pheno_IDreset.csv",
  method = "Epistasis",
  trait = 1
)
