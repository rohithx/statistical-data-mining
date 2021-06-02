rm(list=ls())
graphics.off()

library("rpart")
library('caret')

load("covertype.RData")
names(covertype)

set.seed(1234)
test_index <- sample(1:nrow(covertype), 200000)
test <- covertype[test_index,]
train <- covertype[-test_index,]

model_control <- rpart.control(minsplit = 7000 ,xval=10, cp = 0)

# fitting the data
fit <- rpart(V55~., data = train, method = 'class', control = model_control)

x11()
plot(fit$cptable[,4], main = "Cp fo model selection", ylab = "cv error")

x11()
plot(fit, branch = 0.4, compress = T, main = 'Full Tree')
text(fit, cex = 0.6)

pred.test <- predict(fit, newdata = test, type = 'class')
confusionMatrix(pred.test, factor(test$V55))

pred.train <- predict(fit, newdata = train, type = 'class')
confusionMatrix(pred.train, factor(train$V55))
