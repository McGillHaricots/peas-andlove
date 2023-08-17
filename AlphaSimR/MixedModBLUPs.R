
library(metan)
library(dplyr)

data = read.csv("KelvinData.csv")

env = list()
for (x in 1:nrow(data)){
  
  newcol = paste(data[x,2],data[x,3],sep = "")
  env[[x]] = newcol
  
}

env = as.data.frame(env)
env = t(env)

data = cbind(data,env)
data = data[,-c(2,3)]



data$Adjusted.Yield <- as.numeric(as.character(data$Adjusted.Yield))  # yield was character, must be numeric
data$Rep <- as.factor(as.factor(data$Rep)) 
data$env <- as.factor(as.factor(data$env)) 
data$Genotype <- as.factor(as.factor(data$Genotype)) 

inspect(data)


mixed_mod <- gamem_met(data,
            env = env,
            gen = Genotype,
            rep = Rep,
            resp = Adjusted.Yield,
            random = "gen", #Default
            verbose = TRUE) #Default

plot(mixed_mod) #plotresiduals 

plot(mixed_mod, type = "re") #plot random effects of genotype and interaction effects 

LRT <- get_model_data(mixed_mod, "lrt") #likelihood ratio tests 
print(LRT)

CV <- get_model_data(mixed_mod) # coefficient of variation
print(CV)

print(mixed_mod$Adjusted.Yield$BLUPgen) #blup for genotypes 

get_model_data(mixed_mod, what = "blupg") #get data from fitted model - return predicted mean of each genotype for each variable


#plot blups 
library(ggplot2)
a <- plot_blup(mixed_mod)
b <- plot_blup(mixed_mod, 
               col.shape  =  c("gray20", "gray80"),
               plot_theme = theme_metan(grid = "y")) +
  coord_flip()
arrange_ggplot(a, b, tag_levels = "a")

print(mixed_mod$Adjusted.Yield$BLUPint) #blup for GXE
