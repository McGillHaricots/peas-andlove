
library(GGEBiplots)
library(metan)

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
dataUpdated <- as.data.frame(lapply(dataUpdated, unlist))

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

newEnvList = c(MPIKA2021,MPIKA2018,MPIKA2020,UNZA2017,UNZA2019)

for (env in newEnvList) {
  merge = merge(merge,UNZA2019 ,by='row.names') #look at genotypes that are present in all environments 
  rownames = merge[,1]
  merge = merge[,-1]
  rownames(merge) = rownames
}

GGEmat = merge #GGEModel genotypes in rows, envts in columns. contains means

# DATA TABLE IS READY BIPLOT ANALYSIS BELOW 

##BASICBIPLOT
gge_model = gge(dataUpdated,env,geno,yield, centering = "2", scaling ="sd", svp="2") 
b <- plot(gge_model,
          col.gen = "orange",
          size.text.env = 1,
          plot_theme = theme_metan(grid =  "both"))
arrange_ggplot(b)


#MEAN PERFORMANCE VS STABILITY
gge_model <- gge(dataUpdated, env, geno, yield, svp = "genotype")
d <- plot(gge_model,
          type = 2,
          col.gen = "black",
          col.env = "red",
          axis_expand = 1.5,
          plot_theme = theme_metan_minimal())
arrange_ggplot(d)

#WHICH WON WHERE
gge_model <- gge(dataUpdated, env, geno, yield, svp = "symmetrical")
f <- plot(gge_model,
          type = 3,
          size.shape.win = 5,
          large_label = 6,
          col.gen = "black",
          col.env = "gray")
arrange_ggplot(f)

#DISCRIMINITAVENESS VS REPRESENTATIVENESS
h <- plot(gge_model,
          type = 4,
          plot_theme = theme_metan_minimal())
arrange_ggplot(h)

#EXAMINE AN ENVIRONMENT
gge_model <- gge(dataUpdated, env, geno, yield, svp = "symmetrical")
j <- plot(gge_model,
          type = 5,
          sel_env = "UNZA2019",
          col.gen = "black",
          col.env = "black",
          size.text.env = 5,
          axis_expand = 0.5)
arrange_ggplot(j)


#RANK ENVIRONMENTS
gge_model <- gge(dataUpdated, env, geno, yield)
l <- plot(gge_model,
          type = 6,
          col.gen = "black",
          col.env = "black",
          col.circle = "red",
          col.alpha.circle = 0.5,
          size.text.env = 4,
          axis_expand = 3,
          plot_theme = theme_metan(color.background = "white"))
arrange_ggplot(l)

#EXAMINE GENOTYPE
gge_model <- gge(dataUpdated, env, geno, yield, svp = "genotype")
n <- plot(gge_model,
          type = 7,
          sel_gen = "SB160",
          col.gen = "black",
          col.env = "black",
          size.text.env = 10,
          axis_expand = 1.5,
          plot_theme = theme_metan(grid = "both"))
arrange_ggplot(n)


#RANK GENOTYPES
p <- plot(gge_model,
          type = 8,
          col.gen = "orange",
          col.env = "blue",
          size.text.gen = 1,
          axis_expand = 1.5,
          plot_theme = theme_metan(grid="both"))
arrange_ggplot(p)

#RELATIONSHIP AMONG ENV
gge_model <- gge(dataUpdated, env, geno, yield)
t <- plot(gge_model,
          type = 10,
          col.gen = "black")
arrange_ggplot(t)


##REF https://tiagoolivoto.github.io/metan/articles/vignettes_gge.html
