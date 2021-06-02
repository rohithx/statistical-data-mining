# Written by Rohith Reddy
library(leaps)

cereal <- read.csv("C:\\Users\\X\\Desktop\\cereal.csv", header = TRUE)

# a)
# Creating Training and Test subsets

cereal_train <- cereal[1:66,]
cereal_test <- cereal[67:77,]

# Fitting a linear model 
fit <- lm(rating~., data = cereal_train[,4:16])

fit.summary <- summary(fit)
fit.summary

fit.train.mse <- mean(fit.summary$residuals^2)
fit.train.mse

test_pred = (cereal_test[,4:16]$rating - predict.lm(fit, cereal_test[,4:16]))^2
fit.test.mse <- mean(test_pred)

# Backward subset selection
cereal_bss <- regsubsets(rating~.-name -mfr -type, data = cereal_train, nvmax = 12, method = "backward")

bss.summary <- summary(cereal_bss)
bss.summary$outmat
coef(cereal_bss, id = 12)

# Exhaustive subset selection
cereal_ess <- regsubsets(rating~.-name -mfr -type, data = cereal_train, nvmax = 12, method = "exhaustive")

ess.summary <- summary(cereal_ess)
ess.summary$outmat
coef(cereal_ess, id = 12)

#  Testing
test <- cbind(rep(1,length(cereal_test[,1])), cereal_test[,4:16])
colnames(test) <- c("(Intercept)", colnames(test[-1]))
test.bss.err = rep(NA, 12)
test.ess.err = rep(NA, 12)

for (i in 1:12){
  coeff_bss = coef(cereal_bss, id = i)
  bss_pred = as.matrix(test[,names(coeff_bss)])%*%coeff_bss
  err_bss = (test$rating-bss_pred)^2
  test.bss.err[i] = mean(err_bss)
  
  coeff_ess = coef(cereal_ess, id = i)
  ess_pred = as.matrix(test[,names(coeff_ess)])%*%coeff_ess
  err_ess = (test$rating-ess_pred)^2
  test.ess.err[i] = mean(err_ess)
}

plot(test.ess.err, col = "red", type = "b", xlab  = "subset size", ylab = "MSE")
lines(test.bss.err, col = "blue", type = "b")
legend("topright", c("ESS","BSS"),lty=c(1,1),lwd=c(2.5,2.5),col=c("red","blue"))
