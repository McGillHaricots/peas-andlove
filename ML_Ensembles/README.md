This repo includes code and data for conducting the experiments in:
"Multi-Environment Ensemble Models for Genomic Prediction in Common Bean (Phaseolus vulgaris, L.)"

Data can be found in the data folder, with one file containing adjusted means, location, and genotype
data for each trait. 

The data folder also includes the data used to create the biplots in the biplots folder, with one .csv for each trait.

Raw data can be found at https://github.com/McGillHaricots/CDBN-GeneticGain and at https://github.com/Alice-MacQueen/CDBNgenomics 

There is one .py file for each model we implemented - Linear Regression (LinearRegressoin.py), 
Neural Network(NeuralNetwork.py) and Ridge Regression (RidgeRegression.py).

There is also a NeuralNetork.sh file for submitting the neural network experiment to remote clusters
due to increased computational requirement. The results from the neural net experiment 
can be accessed in the "NeuralNetworkOutput.py" file when run in the cluster where the experiment
was conducted.

When conduting ridge regression, we used a validation set to determine the best value
for the ridge penatly. this is the bestAlphaRR.py file. 
