library(rrBLUP)
training = read.csv("Training2021.csv")
y = training$responseVar
x <- training[ , !(names(training) %in% c("X", "index","responseVar")) ]
x[x==0] <- -1
x[x==1] <- 0
x[x==2] <- 1

testing = read.csv("TestingSSD2025.csv")
xTest <- testing[ , !(names(testing) %in% c("X", "index")) ]
xTest[xTest==0] <- -1
xTest[xTest==1] <- 0
xTest[xTest==2] <- 1
xTest <- xTest[ , colnames(x)]

testIDs = testing$index

rrblup <- mixed.solve(y = y, Z = x)

marker_effects <- rrblup$u  

xTest = as.matrix(xTest)
GEBVs_test <- as.data.frame(xTest %*% marker_effects)

predictions = cbind(testIDs, GEBVs_test)
colnames(predictions) <- c("ID", "GEBV")


write.csv(predictions,"GEBVsSSD2025.csv",row.names = FALSE)
