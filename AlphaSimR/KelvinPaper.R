
data = read.csv("KelvinData.csv")

##name all 7 environments by location and year

env = list()
for (x in 1:nrow(data)){

  newcol = paste(data[x,2],data[x,3],sep = "")
  env[[x]] = newcol
  
}

env = as.data.frame(env)
env = t(env)

data = cbind(data,env)
data = data[,-c(2,3)]


# find mean of each genotype at each location

means = list() # mean yield across reps at for each geno
genotypes = list() # will hold geno id at hand
env = list() # will hold environment id at hand 

data$Adjusted.Yield <- as.numeric(as.character(data$Adjusted.Yield))  # yield was character, must be numeric

i = 1
x = 1 
while (i < nrow(data)){
  yields = data[(i:(i+2)),3] # yield is 3th column
  yieldMean = mean(yields) # mean yield for each rep 
  means[[x]] = yieldMean # assign mean
  genotypes[[x]] = data[i,1] # take down genotype id
  env[[x]] = data[i,4] # take down env id
  i = i + 3 # move on to next set of 3 obs 
  x = x + 1 #fill in next spot in list
}

means = as.data.frame(t(means))
genotypes = as.data.frame(t(genotypes))
env = as.data.frame(t(env))

dataUpdated = as.data.frame(t(rbind(genotypes, means, env)))
colnames(dataUpdated) = c("geno","yield","env") #new dataUpdated gives genotype, mean yield, and location


envlist = c("GART2021","KABWE2021","MPIKA2021","MPIKA2018","MPIKA2020","UNZA2017","UNZA2019")
datalist = list()
x = 1

for (i in envlist){
  df = dataUpdated[dataUpdated$env==i,] #pull out one env
  rownames = df[,1] #must keep genotype names 
  yield = t(as.data.frame(df[,2])) #we just need yield
  rownames(yield) = rownames #assign genotypes to yields via rownames 
  colnames(yield) = i #colname is the env
  yield = as.data.frame(yield) #must be DF
  assign(paste0(i),yield) #write to global env
}

merge = merge(GART2021,KABWE2021, by='row.names') #look at genotypes present in both environments 
rownames = merge[,1]
merge = merge[,-1]
rownames(merge) = rownames

newEnvList = c("MPIKA2021","MPIKA2018","MPIKA2020","UNZA2017","UNZA2019")

for env in envlist {
  merge = merge(merge,env ,by='row.names') #look at genotypes that are present in all environments 
  rownames = merge[,1]
  merge = merge[,-1]
  rownames(merge) = rownames
}

GGEmat = merge #GGEModel genotypes in rows, envts in columns. contains means

GGEmodel = GGEModel(GGEmat, centering = "tester", scaling ="none",SVP="symmetrical") #GGEmodel function frompackage GGEBiplots

GGEPlot(GGEmodel, type=1) #Plot the GGEmodel using GGEPlot function
