rm(list=ls())
graphics.off()

library('rpart')
library('caret')
library('randomForest')
library('MASS')

wine <- read.table("wine.data", header = F, sep=',')

# splitting the data
set.seed(1234)
test_index <- sample(1:nrow(wine), 40)
test <- wine[test_index,]
train <- wine[-test_index,]

model_control <- rpart.control(minsplit = 3,xval=10, cp = 0)

# fitting the data
fit <- rpart(V1~., data = train, method = 'class', control = model_control)

x11()
plot(fit$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit$cptable[,4])
pruned_fit <- prune(fit, cp = fit$cptable[min_cp,1])

x11()
plot(fit, branch = 0.4, compress = T, main = 'Full Tree')
text(fit, cex = 0.6)

x11()
plot(pruned_fit, branch = 0.4, compress = T, main = 'Pruned Tree')
text(pruned_fit, cex = 0.6)

# checking accuracy

pred.test <- predict(fit, newdata = test, type = 'class')
confusionMatrix(pred.test, factor(test$V1))

pred.prune.test <- predict(pruned_fit, newdata = test, type = 'class')
confusionMatrix(pred.prune.test, factor(test$V1))

# random forest

rf_fit <- randomForest(as.factor(V1)~., data = train, n.tree = 10000)

x11()
varImpPlot(rf_fit)

pred.rf.test = predict(rf_fit, newdata = test, type = "class")
confusionMatrix(pred.rf.test, factor(test$V1))

# b) LDA

lda_fit <- lda(V1~., data = train)

x11()
plot(lda_fit)

pred.lda.test <- predict(lda_fit, newdata = test, type = "class")

mean(pred.lda.test$class == test$V1)
confusionMatrix(pred.lda.test$class, factor(test$V1))
