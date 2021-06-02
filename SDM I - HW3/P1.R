# Written by Rohith
rm(list =ls())
library(glmnet)
library(leaps)

# Import Training Data
train_data <- read.table("ticdata2000.txt")

# Import Testing Data
test_data <- read.table("ticeval2000.txt")
test_target <- read.table("tictgts2000.txt")

# Splitting Training Data
train_a <- train_data[-86]
train_t <- train_data[86]

# Fitting a liner model
fit <- lm(V86~., data = train_data)
fit.summary <- summary(fit)

fit.train.mse <- mean(fit.summary$residuals^2)
fit.train.mse

test_pred = (test_target$V1 - predict.lm(fit, test_data))^2
fit.test.mse <- mean(test_pred)

# Forward Selection

insurance_fss <- regsubsets(as.matrix(train_data[-86]), as.matrix(train_data[86]), nvmax = 85, method = "forward")
fss.summary <- summary(insurance_fss)

x11()
par(mfrow = c(1,2))
plot(fss.summary$cp, xlab = "Subset Size", ylab = "Cp", type = "b", xlim=c(1,85))
plot(fss.summary$bic, xlab = "Subset Size", ylab = "BIC", type = "b", xlim=c(1,85))

test <- cbind(rep(1,length(test_data[,1])), test_data)
colnames(test) <- c("(Intercept)", colnames(test[-1]))
test.fss.err = rep(NA, 85)

for (i in 1:85){
  coeff_fss = coef(insurance_fss, id = i)
  fss_pred = as.matrix(test[,names(coeff_fss)])%*%coeff_fss
  err_fss = (test_target$V1-fss_pred)^2
  test.fss.err[i] = mean(err_fss)
}

x11()
plot((fss.summary$rss)/length(train_data[,1]), main = "Forward Subset Selection Training Error", xlab = "Subset Size", ylab = "MSE", type = "b", xlim=c(1,85), col = "blue")
x11()
plot(test.fss.err, col = "red", type = "b", xlab  = "Subset Size", ylab = "MSE", main = "Forward Subset Selection Testing Error")
which(test.fss.err == min(test.fss.err))

# Backward Selection

insurance_bss <- regsubsets(as.matrix(train_data[-86]), as.matrix(train_data[86]), nvmax = 85, method = "backward")
bss.summary <- summary(insurance_bss)

x11()
par(mfrow = c(1,2))
plot(bss.summary$cp, xlab = "Subset Size", ylab = "Cp", type = "b", xlim=c(1,85))
plot(bss.summary$bic, xlab = "Subset Size", ylab = "BIC", type = "b", xlim=c(1,85))

test <- cbind(rep(1,length(test_data[,1])), test_data)
colnames(test) <- c("(Intercept)", colnames(test[-1]))
test.bss.err = rep(NA, 85)

for (i in 1:85){
  coeff_bss = coef(insurance_bss, id = i)
  bss_pred = as.matrix(test[,names(coeff_bss)])%*%coeff_bss
  err_bss = (test_target$V1-bss_pred)^2
  test.bss.err[i] = mean(err_bss)
}

x11()
plot((bss.summary$rss)/length(train_data[,1]), main = "Backward Subset Selection Training Error", xlab = "Subset Size", ylab = "MSE", type = "b", xlim=c(1,85), col = "blue")
x11()
plot(test.bss.err, col = "red", type = "b", xlab  = "Subset Size", ylab = "MSE", main = "Backward Subset Selection Testing Error")
which(test.bss.err == min(test.bss.err))

# LASSO Regression
set.seed(123)
insurance_lasso <- glmnet(as.matrix(train_a), as.matrix(train_t), alpha = 1)

cv.out <- cv.glmnet(as.matrix(train_a), as.matrix(train_t), alpha = 1)
x11()
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

lasso_pred <- predict(insurance_lasso, s= bestlam, type = "coefficients")

lasso_pred_train <- predict(insurance_lasso, s= bestlam, newx = as.matrix(train_a), type = "response")
lasso_pred_test <- predict(insurance_lasso, s= bestlam, newx = as.matrix(test_data), type = "response")

train_error <- mean((lasso_pred_train - as.matrix(train_t))^2)
test_error <- mean((lasso_pred_test - as.matrix(test_target))^2)


# Ridge Regression
set.seed(123)
insurance_rr <- glmnet(as.matrix(train_a), as.matrix(train_t), alpha = 0)

cv.out <- cv.glmnet(as.matrix(train_a), as.matrix(train_t), alpha = 0)
x11()
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

ridge_pred <- predict(insurance_rr, s= bestlam, type = "coefficients")

ridge_pred_train <- predict(insurance_rr, s= bestlam, newx = as.matrix(train_a), type = "response")
ridge_pred_test <- predict(insurance_rr, s= bestlam, newx = as.matrix(test_data), type = "response")

train_error <- mean((ridge_pred_train - as.matrix(train_t))^2)
test_error <- mean((ridge_pred_test - as.matrix(test_target))^2)
