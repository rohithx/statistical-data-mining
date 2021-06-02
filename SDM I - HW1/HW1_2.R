# Written by Rohith Reddy

# Loading from RData file
pr_auto <- get(load("data.RData"))

# fitting the data with mpg as the response variable
fit <- lm(mpg~., data = pr_auto)
summary(fit)

# refitting the model excluding acceleration
fit2 <- lm(mpg~.-acceleration, data = pr_auto)
summary(fit2)

# refitting the model further excluding cylinders and displacement
fit3 <- lm(mpg~. -acceleration -displacement -cylinders, data = pr_auto)
summary(fit3)

# refitting the model further excluding horsepower
fit4 <- lm(mpg~. -acceleration -displacement -cylinders -horsepower, data = pr_auto)
summary(fit4)
hist(fit4$residuals)

# fitting models with interaction
fit5 <- lm(mpg~ horsepower:weight + year, data = pr_auto)
summary(fit5)

fit6 <- lm(mpg~ horsepower*weight + year, data = pr_auto)
summary(fit6)

fit7 <- lm(mpg~ displacement*weight + year, data = pr_auto)
summary(fit7)
