rm(list=ls())
graphics.off()

library("rpart")

load("vehicle.RData")
names(vehicle)
vehiclex <- vehicle[-19]

set.seed(1234)

model_control <- rpart.control(minsplit = 20, cp = 0)

fit <- rpart(class~., data = vehiclex, method = 'class', control = model_control)

x11()
plot(fit$cptable[,4], main = "Cp fo model selection", ylab = "cv error")

min_cp = which.min(fit$cptable[,4])
pruned_fit <- prune(fit, cp = fit$cptable[min_cp,1])

x11()
plot(fit, branch = 0.4, compress = T, main = 'Full Tree')
text(fit, cex = 0.6)

x11()
plot(pruned_fit, branch = 0.4, compress = T, main = 'Pruned Tree')
text(pruned_fit, cex = 0.6)
