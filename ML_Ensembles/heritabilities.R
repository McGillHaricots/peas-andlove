data = read.csv("fullDatasetDM.csv")
#Vg = [4.68, 12.29, 2339.91,53173.10] #DF, DM, SW, SY

Vg = 53173.10
locs = as.list(unique(data$X1))
h2s = list()  # To store h2 values
x = 1 # To add to h2s list 

for (loc in locs){
  Location = data[data$X1 == loc, ]
  pheno = as.numeric(Location$X3)
  pv = var(pheno)
  labels = as.character(Location$X2)
  h2 = Vg / (Vg + (pv/2))
  h2s[x] = h2
  x = x+1
}
locationsDF = t(as.data.frame(locs))
h2DF = t(as.data.frame(h2s))

results = as.data.frame(cbind(locationsDF,h2DF))
rownames(results) = NULL
colnames(results) = c("location","h2")
results = data.frame(results)
write.csv(results,"phenoH2DM.csv")
