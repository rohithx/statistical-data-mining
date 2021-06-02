# Written by Rohith
rm(list = ls())
library(class)
library(gmodels)

setwd('C:/Users/X/Desktop')
load('zip.train.RData')
load('zip.test.RData')

test <- as.data.frame((zip.test))
train <- as.data.frame((zip.train))

# Considering only for 2 and 3
train_data <- subset(train, V1 == 2 | V1 == 3)
test_data <- subset(test, V1 == 2 | V1 ==  3)

# Performing linear regression
fit = lm(V1~., data = train_data)
summ <- summary(fit)
summ

# Errors
train_MSE = mean(fit$residuals^2)
test_MSE = mean((test_data$V1 -predict.lm(fit, test_data))^2)
pred <- round(predict.lm(fit, test_data))

# Removing Outlier in pred
pred[360] = 3

# Setting k values
k_val <- c(1,3,5,7,9,11,13,15)

# Training and testing

knntab <- function(training, testing, k){
  KNN <- knn(training[,-1], testing[,-1], training$V1, k)
  predict <- KNN
  CrossTable(x = testing$V1, y = predict)
}

for (i in k_val){
  knntab(train_data, test_data, i)
  print(paste("KNN Test predictions for k = ", i))
  
  knntab(train_data, train_data, i)
  print(paste("KNN Training predictions for k = ", i))
}

CrossTable(x = test_data$V1, y = pred)
