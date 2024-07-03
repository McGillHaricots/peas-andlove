library(tidyverse)
library(keras)
library(tensorflow)
library(readr)

Y = data.frame(t(Y))
c1 = data.frame(Y[,1])
colnames(c1) = "value"
c2 = data.frame(Y[,2])
colnames(c2) = "value"
Y = rbind(c1,c2)

## create GRM ##
geno <- as.matrix(M)
GM <- tcrossprod(geno)/dim(geno)
LG <- GM

Y <- as.matrix(Y)
X = LG

X_train <- as.matrix(M)
Y_train <- as.matrix(Y)


## define hyperparameters##

cutoff =200

nEpoch = 1000

if (nrow(geno) < cutoff){
  batchSize = 20
}else{
batchSize=100
}

valSplit = 0.2


# add layers

inputs = layer_input(shape=(ncol(X_train))) 

predictions <- inputs %>% 
  layer_dense(units = ncol(X_train), activation = 'relu') %>% 
  layer_batch_normalization(momentum=0.1,epsilon=0.001,center=TRUE,scale=TRUE) %>%
  layer_dense(units = ncol(X_train), activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = ncol(X_train), activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1)

# create and compile model 

model <- keras_model(inputs = inputs, outputs = predictions) 

earlyStopping <- callback_early_stopping(monitor = "mean_squared_error", min_delta = 0.1, patience = 10,
                                         mode = "auto")

model %>% compile( 
  optimizer = 'Adam', 
  loss = 'mean_squared_error',
  metrics = c('mean_squared_error')
) 

fit(
  model,
  x = X_train,
  y = Y_train,
  batch_size = batchSize,
  epochs = nEpoch,
  verbose = 0,
  validation_split = valSplit,
  callbacks = list(earlyStopping)
)

cli_alert_success("Fit Neural Net at {args$trainGen} using {args$trainingData} data")









