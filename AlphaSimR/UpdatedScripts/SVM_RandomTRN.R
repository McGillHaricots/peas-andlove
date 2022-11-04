y <- TrainingPheno
x <- TrainingGeno
trainingpop= as.data.frame(cbind(y,x))
colnames(trainingpop) <- paste("ID",1:(ncol(y) + ncol(x)), sep="")

train_index <- sample(1:nrow(trainingpop), 0.9 * nrow(trainingpop))
trainingset <- trainingpop[train_index, ]
testingset <- trainingpop[-train_index, ]

##fit model, predict pheno on all markers
svm_fit = svm(ID1 ~ ., data = trainingset, kernel = "radial", cost = 10, scale = FALSE)
