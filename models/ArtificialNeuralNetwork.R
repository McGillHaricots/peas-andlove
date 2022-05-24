## working in python venv so we can use keras and tensorflow ##

Sys.setenv(RETICULATE_PYTHON = ".venv/bin/python")

## requried packages ##

library(keras)
library(readr)
library(BMTME)

## load in data ##

SR_SY_categorical <- read_csv("~/Desktop/tensorflow/SR_SY_categorical.csv")

## create GRM ##

geno <- as.matrix(SR_SY_categorical[,-1])
GM <- tcrossprod(geno)/dim(geno)
LG <- cholesky(GM)

## set respone variable ##

Y <- as.matrix(SR_SY_categorical[,2])
X = LG

## training testing partition using BMTME ##

pheno <- data.frame(GID=SR_SY_categorical[,2])
CrossV <- CV.KFold(pheno, DataSetID='GID')

## one holdout CV ##

tst_set <- CrossV$CrossValidation_list[[2]]
No_Epoch = 1000
N_Units = 33
X_trn = X[-tst_set,]
X_tst = X[tst_set,]
Y_trn = Y[-tst_set,]
Y_tst = Y[tst_set,]

## Build model using pipe operator ##

build_model <- function() {
  model <- keras_model_sequential()
  model %>%
    layer_dense(units =N_Units, activation = "relu", input_shape = c(dim
                                                                     (X_trn)[2])) %>%
    layer_dropout(rate = 0.0) %>%
    layer_dense(units = 1, activation = "linear")
  model %>% compile(
    loss = "mse",
    optimizer = "rmsprop",
    metrics = c("mse"))
  model}

## build and view model ##

model <- build_model()
model %>% summary()

## fitting and plotting the model ##

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 20 == 0) cat("\n")
    cat(".")
  })

## inner training and validation ##

model_fit <- model %>% fit(
  X_trn, Y_trn,
  shuffle=F,
  epochs = No_Epoch, batch_size = 640,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback))

## plot ## 

plot(model_fit)

## predictions and MSE of testing ##

model_fit$metrics$val_mse[No_Epoch]
pred=model%>%predict(X_tst)
Pred=c(pred)
Obs = y_tst
MSE_First=mean((Obs-Pred)^2)
MSE_First

## Refit model using early stopping ##

early_stop <- callback_early_stopping(monitor="val_loss", mode='min', patience =50)

model_Final <- build_model()
model_fit_Final <- model_Final%>%fit(
  X_trn, Y_trn,
  shuffle=F,
  epochs = No_Epoch, batch_size=640,
  validation_split = 0.2,
  verbose=0, callbacks = list(early_stop, print_dot_callback)
)

## plot history of training process ##

length(model_fit_Final$metrics$mean_squared_error)
plot(model_fit_Final)

## predition ##

prediction = model_Final %>% predict(X_tst)
Predicted = c(prediction)
Observed = Y_tst
plot(Observed, Predicted)
MSE = mean((Observed-Predicted)^2)
MSE
Obs_Pred = cbind(Observed, Predicted)
colnames(Obs_Pred) = c("Observed", "Predicted")
Obs_Pred


## results with and without early stoppage ##

MSE_First
MSE

## Source Statistical Machine Learning, Montesinos-Lopez 11.4 ##
