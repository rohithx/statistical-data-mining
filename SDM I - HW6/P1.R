rm(list=ls())
graphics.off()

library(rpart)
library(gbm)
library(randomForest)
library(glmnet)

load('Pima.RData')
diab <- pima[,-9]

# Putting aside test set
set.seed(123456)
test_indis <- sample(1:nrow(diab), nrow(diab)/3)
test <- diab[test_indis,]
train <- diab[-test_indis,]

y_true <- as.numeric(test$classdigit) - 1
# Random Forest
rf.fit <- randomForest(classdigit~., data = train, n.tree = 5000)

x11()
varImpPlot(rf.fit)
importance(rf.fit)

rf.pred <-predict(rf.fit, newdata = test, type = "response")
rf.yhat <- as.numeric(rf.pred) - 1
rf.misclass <- sum(abs(rf.yhat - y_true))/length(rf.yhat)

# Bagging
bag.fit <- randomForest(classdigit~., data = train, n.tree = 5000, mtry = 7)

x11()
varImpPlot(bag.fit)
importance(bag.fit)

bag.pred <-predict(bag.fit, newdata = test, type = "response")
bag.yhat <- as.numeric(bag.pred) - 1
bag.misclass <- sum(abs(bag.yhat - y_true))/length(bag.yhat)

# Boosting
boost.train <- train
boost.train$classdigit <- as.numeric(train$classdigit)-1
boost.test <- test
boost.test$classdigit <- as.numeric(test$classdigit)-1

shrink <- c(.1, .3, .5, .7)
max_iter <- 1000
store_error <- c()
for (i in 1:length(shrink)){
  boost.fit <- gbm(classdigit~., data = boost.train, n.trees = max_iter, shrinkage = shrink[i], interaction.depth = 3, distribution = 'adaboost')
  temp <- c()
  for (j in 1:max_iter){
    boost.yhat <- predict(boost.fit, newdata = boost.test, n.trees = j, type = "response")
    misclass_boost <- sum(abs(boost.yhat - y_true))/length(boost.yhat)
    temp <- c(temp, misclass_boost)
  }
  store_error <- cbind(store_error, temp)
}
colnames(store_error) <- paste("shrinkage", shrink, sep = ":")
min(store_error)
x11()
plot(store_error[,1], main = "Error Profiles", ylab = "error", xlab = "iterations", ylim = c(min(store_error), 0.5))
lines(store_error[,2], col = "red")
lines(store_error[,3], col = "blue")
lines(store_error[,4], col = "green")

# Logistic Regression
log_train <- train
log_train$classdigit <- factor(log_train$classdigit)
log_fit <- glm(classdigit~., data = train, family = "binomial")
summary(log_fit)
log_pred <- predict(log_fit, newdata = test, type = "response")
log_pred <- ifelse(log_pred > 0.5, 1, 0)
misclass_log <- sum(abs(log_pred - y_true))/length(y_true)
misclass_log
