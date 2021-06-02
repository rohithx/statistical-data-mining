rm(list=ls())
graphics.off()

library(leaps)
library(ElemStatLearn)
data(prostate)
head(prostate)

train <- subset(prostate, train == "TRUE")[,-10]
test <- subset(prostate, train == "FALSE")[,-10]

fit <- regsubsets(lpsa~., data = train, nvmax= 8, method = 'exhaustive')
fit_sum <- summary(fit)
select <- fit_sum$outmat

train.error.store <- c()
test.error.store <- c()

aic.store <- c()
bic.store <- c()

for(i in 1:8){
  temp<-which(select[i,]=="*")
  red.train<-train[,c(9,temp)]
  red.test<-test[,c(9,temp)]
  red.fit<-lm(lpsa~.,data=red.train)
  
  pred.train = predict(red.fit, newdata = red.train)
  pred.test = predict(red.fit, newdata = red.test)
  
  train.error <- sum((pred.train-train$lpsa)^2)/length(train$lpsa)
  test.error <- sum((pred.test-test$lpsa)^2)/length(test$lpsa)
  
  train.error.store <- c(train.error.store, train.error)
  test.error.store <- c(test.error.store, test.error)
  
  aic<-AIC(red.fit)
  bic<-BIC(red.fit)
  
  aic.store <-c(aic.store, aic)
  bic.store <-c(bic.store, bic)
}

# Bootstrap.632 error
library(bootstrap)
library(boot)

beta.fit <- function(X,Y){
  lsfit(X,Y)
}

beta.predict <- function(fit,X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

x <- prostate[,1:8]
y <- prostate[,9]

bootstrap.error.store <- c()
for (i in 1:8){
  temp <- which(select[i,] == "*")
  res <- bootpred(x[,temp],y,nboot = 50, beta.fit, beta.predict, err.meas = sq.error)
  bootstrap.error.store<-c(bootstrap.error.store,res[[3]])
}

# cross val fivefold tenfold

set.seed(123)

fivefold.error.store <- c()
tenfold.error.store <- c()

for (i in 1:8){
  temp<-which(select[i,]=="*")
  x.data <- prostate[,c(9,temp)]
  x.fit <- glm(lpsa~., data = x.data)
  fivefold.error <- cv.glm(x.data, x.fit, K=5)$delta[2]
  tenfold.error <- cv.glm(x.data, x.fit, K=10)$delta[2]
  fivefold.error.store <- c(fivefold.error.store, fivefold.error)
  tenfold.error.store <- c(tenfold.error.store, tenfold.error)
}

# plotting

upper= max(aic.store,bic.store)
lower= min(aic.store,bic.store)

x11()
plot(bic.store, type = "o",lty = 2, col = "red", ylim = c(lower-1,upper +1), xlab = "k", ylab = "val", main = "AIC, BIC")
lines(aic.store, type = "o", lty = 1, col = "blue")
legend("topright", c("BIC", "AIC"), lty=c(2,1), col = c("red", "blue"))


upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)

x11()
plot(fivefold.error.store, type = "o", lty = 2, col = "red",ylim = c(lower-.1,upper+.1), xlab = "k", ylab = "error", main = "Errors")
lines(tenfold.error.store, type = "o", lty = 2, col = "blue")
lines(test.error.store, type = "o", lty = 1, col = "black")
lines(train.error.store, type = "o", lty = 1, col = "orange")
lines(bootstrap.error.store, type = "o", lty = 1, col = "green")

legend("topright", c("fivefold error", "tenfold error", "test error", "train error", "bootstrap.632 error"), lty = c(2,2,1,1,1), col = c("red", "blue", "black", "orange", "green"))
