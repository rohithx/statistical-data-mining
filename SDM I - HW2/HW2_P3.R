# Written by Rohith
rm(list = ls())
library(ISLR)
library(glmnet)

?College
college_train <- College[1:700,]
college_test <- College[701:777,]

fit <- lm(Apps~., data = college_train[,-1])
summ <- summary(fit)

fit.train.mse = mean(summ$residuals^2)
fit.test.mse = mean((college_test$Apps - predict.lm(fit, college_test[,-1]))^2)

# Fitting a Ridge Regression Model
ridge.mod = glmnet(as.matrix(college_train[,3:18]), college_train[,2], alpha = 0)

# Model Selection
set.seed(123)
cv.out <- cv.glmnet(as.matrix(college_train[,3:18]), college_train[,2], alpha = 0)
plot(cv.out)
best_lamda <- cv.out$lambda.min

# Test Error with best lambda
ridge.pred <- predict(ridge.mod, as.matrix(college_test[,3:18]), s = best_lamda, type = "response")
test_err = mean((college_test[,2] - ridge.pred)^2)

# Fitting a Lasso Model
lasso.mod <- glmnet(as.matrix(college_train[,3:18]), college_train[,2], alpha = 1)

# Model Selection
set.seed(123)
cv2.out <- cv.glmnet(as.matrix(college_train[,3:18]), college_train[,2], alpha = 1)
plot(cv2.out)
best_lamda_2 <- cv2.out$lambda.min
coefs <- predict(lasso.mod, s = best_lamda_2, type = "coefficients")

# Test Error with best lambda
lasso.pred <- predict(lasso.mod, as.matrix(college_test[,3:18]), s = best_lamda_2, type = "response")
test_err_2 = mean((college_test[,2] - lasso.pred)^2)

