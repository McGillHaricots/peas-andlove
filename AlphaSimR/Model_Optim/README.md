This folder contains the files required to test genomic prediction models RRBLUP, SVM, RF and ANN.

Each runUpdated file contains the steps to simulate one cycle a genomic selection pipeline using the desired model.

The script for each model has a RD(random) version - slecting a random subset of the training population for use in model training - 
The script for each model has a SC(stratified clustering) version - using stratified clustering to select a subset of the training population.

Genotype data are contained in the haplotypesSNPs.Rdata file.
The corresponding map is contained in the genMapSNPs.Rdata file. 

TO RUN THE SIMULATION AND WRITE RESULTS FILES:
-  Navigate to the RunReplications file
-  Specify which scenario you'd like to run, and how many replications
-  Be sure to modify the text in the output file to match the scenario you have run
