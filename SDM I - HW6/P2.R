rm(list=ls())
graphics.off()

library(rpart)
library(gbm)
library(randomForest)
library(pdp)
library(ggplot2)

load('Pima.RData')
diab <- pima[,-9]

# Putting aside test set
set.seed(12)
test_indis <- sample(1:nrow(diab), nrow(diab)/3)
test <- diab[test_indis,]
train <- diab[-test_indis,]
y_true <- as.numeric(test$classdigit) - 1

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

boost.fit <- gbm(classdigit~., data = boost.train, n.trees = max_iter, shrinkage = 0.1, interaction.depth = 3, distribution = 'adaboost')
summary(boost.fit)

x11()
p1 <- partial(boost.fit, pred.var = "glucose", plot = TRUE, n.trees = 1000)
plot(p1)

# Random Forest
rf.fit <- randomForest(classdigit~., data = train, n.tree = 10000)

x11()
varImpPlot(rf.fit)
importance(rf.fit)

rf.pred <-predict(rf.fit, newdata = test, type = "response")
rf.yhat <- as.numeric(rf.pred) - 1
rf.misclass <- sum(abs(rf.yhat - y_true))/length(rf.yhat)
rf.misclass

x11()
partialPlot(rf.fit, pred.data = test, x.var = "glucose")  

x11()
partialPlot(rf.fit, pred.data = test, x.var = "pedigree")  

# Grow a single tree
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(classdigit~., data = train, method = "class", control = model.control)

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

x11()
p2 <- partial(pruned_fit, pred.var = "glucose", plot = TRUE)
plot(p2)
p3 <- partial(pruned_fit, pred.var = "age", plot = TRUE)
plot(p3)

