rm(list=ls())
graphics.off()

library(caret)
library(randomForest)
library(rpart)
library(neuralnet)

set.seed(33)

load('cleveland.RData')
cleve <- cleveland[,-15]
dummy <- dummyVars("~.", fullRank = TRUE, data = cleve)
data <- data.frame(predict(dummy,newdata=cleve))

test_indis <- sample(1:nrow(cleve), nrow(cleve)/3)
test <- cleve[test_indis,]
train <- cleve[-test_indis,]
y_true <- as.numeric(test$diag1) - 1

test_indisx <- sample(1:nrow(data), nrow(data)/3)
testx <- data[test_indisx,]
trainx <- data[-test_indisx,]

# Neural Network
test_err_store <- c()
train_err_store <- c()
for (i in 1:4){
  nn <- neuralnet(diag1.sick~., data = trainx, hidden = i, stepmax = 10^9, err.fct = 'ce', linear.output = FALSE)
  
  #x11()
  #plot(nn)
  
  predxt <- predict(nn, newdata = trainx)
  yt_hat <- round(predxt)
  train_err <- length(which(trainx$diag1.sick != yt_hat))/length(yt_hat)
  train_err_store <- c(train_err_store, train_err)

  predx <-predict(nn, newdata = testx)
  y_hat <- round(predx)
  test_err <- length(which(testx$diag1.sick != y_hat))/length(y_hat)
  test_err_store <- c(test_err_store, test_err)
}
train_err_store
test_err_store

# CART
model.control <- rpart.control(xval = 10, cp = 0)
fit <- rpart(diag1~., data = train, method = "class", control = model.control)

x11()
plot(fit)
text(fit, use.n = TRUE, cex = .5)

min_cp = which.min(fit$cptable[,4])
x11()
plot(fit$cptable[,4], main = "Cp for model selection", ylab = "cv error")

pruned_fit <- prune(fit, cp = fit$cptable[min_cp,1])
x11()
plot(pruned_fit)
text(pruned_fit, use.n = TRUE, cex = .5)

pred <- predict(pruned_fit, newdata = test, "class")
pred <- as.numeric(pred) - 1
misclass <- sum(abs(pred - y_true))/length(pred)
misclass

summary(pruned_fit)

# Random Forest
rf.fit <- randomForest(diag1~., data = train, n.tree = 5000)

x11()
varImpPlot(rf.fit)
importance(rf.fit)

rf.pred <-predict(rf.fit, newdata = test, type = "response")
rf.yhat <- as.numeric(rf.pred) - 1
rf.misclass <- sum(abs(rf.yhat - y_true))/length(rf.yhat)
rf.misclass