
# *****************************
### Installing requirements ###
# *****************************

# requirements, install only once

install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("C:/Users/alexa/ALEXANDER/1. McGill/1. Thesis/2. GWAS/asreml_4.2.0.302.zip", 
                 repos=NULL, type="win.binary")
install.packages("msm")
library(remotes); install_github("matthewwolak/nadiv", ref = "devel")
install.packages("foreach")
install.packages("iterators")
install.packages("doMC", repos="http://R-Forge.R-project.org")

# ********************
### Load Libraries ###
# ********************

# load libraries and other requirements *functions from source
### Workflow running the MTMM with the full sequencing data from Arabidopsis 1001 Genomes project 
### updated version 16.08.19
### This version uses ASREML-R versiion 4 

## ASREML library needs a valid license 
library(lattice)
library(asreml) #needs an active license
library(msm)
library(nadiv)
## libraries for single GWAS
library(foreach)
library(iterators)
library(parallel)
# libraries for plotting
library(ggplot2)
library(dplyr)  
library(tidyverse)
#scripts to source. All scripts can be found in the github folder 
source('scripts/emma.r')
source('scripts/mtmm_estimates_as4.r')
source('scripts/plots_gwas.r')
source('scripts/plot_mtmm.r')
source('scripts/mtmm_cluster.r')
source('scripts/mtmm_part2.r')
source('scripts/gwas.r')
source('scripts/plots_gwas_FDR.r')
source('scripts/plot_mtmm_FDR.r')

## load your Phenotype  Y: a n by m matrix, where n=number of individuals and the first column contains the individual names (colname = 'ecotype_id') , the second the phenotypic values for the first trait and the third column the phenotypic values of the second trait
## load the Kinship matrix
## to test the script load the samle data 
##load('data/MTMM_SAMPLE_DATA.Rdata')

# ------


# ****************************
### Preparing input filess ###
# ****************************

# 1. Genotypic data.

#I used GAPIT to convert the hapmap into numerican 0, 1, 2
#Load data
library(data.table)
setDTthreads(80)
gen=fread("data/GAPIT.Genotype.Numerical.txt", header = T)

# we change the col names to the one specified by the scripts in the way "1- 12458"

names(gen)<- gsub("S0","", names(gen))
names(gen)<- gsub("S","", names(gen))
names(gen)<- gsub("_","- ", names(gen))

# The row names in the X should be the same as row names in K. IN my case same as ecotype_id, K also has colomn and rows with the same name
# The row names need to be changed to numbers first so when turned into a matrix, keeps them
gen$number=1:nrow(gen)
gen <- gen %>% remove_rownames %>% column_to_rownames(var = "number")
# remove the Taxa column, keep only numbers
gen=gen[,-1]
gen = as.matrix(gen)
gen=X
save(X,file= "data/MDP_genMatrix_format.for.MTMM.RData")
load("data/MDP_genMatrix_format.for.MTMM.RData")
head(X[,1:5],3)

# 2. Kinship
# Estimating kinship IBS using the emma function. This function provided by the MTMM scripts doesn't work for Heterozygous files, 
# even when modifying the script to read 0, 1, 2 instead of 0, 0.5, and 1.
# I calculated the kinship using EMMA within GAPIT

K=read.delim("data/GAPIT.Genotype.Kin_EMMA.csv", sep = ",", header = F)

# Used to correct col and row names before re-saving
K$number=1:nrow(K)
K <- K %>% remove_rownames %>% column_to_rownames(var = "number")
K=K[,-1]
colnames(K)=1:nrow(K)
head(K[,1:5],3)
save(K, file= "kinship_285MDP_Combined.RData")
load("data/kinship_285MDP_Combined.RData")


# 2. Phenotype
Y=read.table("data/Phenotypes/DTF_yield21_23.txt", header = T)


# 3. SNP_names file. Important for any step if the names where not adjusted

#SNP_names=read.table("data/SNP_name.txt")

##################################
##run the mtmm_estimate script ###
##################################

mtmm_estimates(Y,k=2,l=3,K,method='default',only.vca=FALSE) 


## this script will generate all the needed data for the GWAS using the estimates from the asreml call
## it will save all data as an .rda file named [Y1]_[Y2]_mtmm_estimates.rda
## you need to load this data before you can continue.

k=2
l=3
mydata<-paste(colnames(Y)[k],colnames(Y)[l],'mtmm_estimates.rda',sep='_')
load(mydata)

## create the name of your output file 
out.name=paste(colnames(Y)[k],'_',colnames(Y)[l],'_mtmm_results',sep='')

results_DTF_yield21_23<-mtmm_part2(X, incl.singleGWAS = T)
save(results_DTF_yield21_23,file="results/results_DTF_yield21_23")

load("results/results_DTF_yield3years")


## Manhattan plots for mtmm

#This is F because I did not run the single GWAS before
#If I run the single GWAS, incl.singleGWAS must be T
# I chenged the maf filter to 0.02 to be shown in the plot

plot_mtmm(results_DTF_yield21_23,correlation,name2 = "results/MDP_results_DTF_yield21_23_mtmm_with_Single_maf0.02.pdf" ,incl.singleGWAS=T)
plot_mtmm_FDR(results_DTF_yield21_23,correlation,name2 = "results/FDR_results_DTF_yield21_23_mtmm_with_Single_maf0.02.pdf" ,incl.singleGWAS=T)

results=results_yield3yr_DTF

#qq plots
par(mfrow=c(1,1),mar=c(3, 4, 1, 4))
qq_plot(results,h=9, maf=0.02)
qq_plot(results,h=12, maf=0.02)
qq_plot(results,h=15, maf=0.02)
qq_plot(results,h=16, maf=0.02)
qq_plot(results,h=17, maf=0.02)
#
