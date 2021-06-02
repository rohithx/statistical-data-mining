# Written by Rohith
rm(list =ls())
library(ISLR)
library(class)

#a)
summary(Weekly)

plot(Weekly$Year, Weekly$Volume)
pairs(Weekly[,1:8])

#b)
set.seed(123)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = 'binomial', data = Weekly[,2:9])
summary(glm.fit)

#c)
pred <- predict(glm.fit, newdata =  Weekly[,2:9], type = "response")
p <- round(pred)
t <- table(Weekly$Direction, p)
t

#d)
train_data <- Weekly[Weekly$Year<=2008,2:9]
glm.fit2 <- glm(Direction~Lag2, family = "binomial", data = train_data)
test_data <- Weekly[Weekly$Year>=2009,2:9]

pred2 <- predict(glm.fit2, newdata = test_data, type = "response")
p2 <- round(pred2)
t2 <- table(test_data$Direction, p2)
t2

#e)
lda.fit = lda(Direction~Lag2, data = train_data)
lda.fit
pred3 = predict(lda.fit, newdata = test_data, type = "response")
p3 = pred3$class
t3 = table(test_data$Direction,p3)
t3


#f)
set.seed(123)
trx = cbind(train_data$Lag2)
tx = cbind(test_data$Lag2)
try = cbind(train_data$Direction)
pred4 = knn(trx, tx, try, k=1)
t4 = table(test_data$Direction, pred4)
t4
