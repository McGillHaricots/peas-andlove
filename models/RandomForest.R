nstall.packages("caret")
install.packages("ranger")
install.packages("tidyverse")
install.packages("e1071")

library(caret)
library(ranger)
library(tidyverse)
library(e1071)


set.seed(23489)
train_index <- sample(1:nrow(data), 0.9 * nrow(data))
SR_train <- data[train_index, ]
SR_test <- data[-train_index, ]

SR_train[1:5,1:5]

control <- trainControl(method='repeatedcv', number=3, repeats=3)

rf_fit <- train(as.factor(SY_GV) ~ ., 
                data = SR_train, 
                method = "ranger",
                trControl=control)

predictions <- predict(rf_fit, SR_test)
write.csv(predictions, "RF_Pred_SY.csv")

###find pearson correlation coefficient###

actual <- as.numeric(SR_test$x)
predictions <- as.numeric(predictions)
accuracy <- as.data.frame(cbind(actual, predictions))
cor(accuracy$actual, accuracy$predictions, method="pearson")

##source : https://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/ ##
