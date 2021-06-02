rm(list=ls())
graphics.off()

load('pendigits.RData')
data <- pendigits[,-36]
YY <- pendigits[,36]

# Variables variance
var_var <- apply(pendigits, 2, var)
var_var
plot(var_var)

# Scaling the data
sdata <- scale(data)

# PC
pc <- prcomp(sdata, center = FALSE, scale = FALSE)
plot(pc)

pc_var <- (pc$sdev)^2
per_var_exp<- (pc_var/(sum(pc_var)))*100
barplot(per_var_exp, main = "PC variance explained", ylab = "% variation explained", xlab = "PCs")

# Adding color
mycol <-rep("pink", length(data[,1]))
unique(YY)
c1 <- which(YY == 1)
mycol[c1] <- "red"
c2 <- which(YY == 2)
mycol[c2] <- "blue"
c3 <- which(YY == 3)
mycol[c3] <- "brown"
c4 <- which(YY == 4)
mycol[c4] <- "orange"
c5 <- which(YY == 5)
mycol[c5] <- "yellow"
c6 <- which(YY == 6)
mycol[c6] <- "cyan"
c7 <- which(YY == 7)
mycol[c7] <- "black"
c8 <- which(YY == 8)
mycol[c8] <- "purple"
c9 <- which(YY == 9)
mycol[c9] <- "grey"

x11()
plot(pc$x[,1], pc$x[,2], xlab = "PC1 scores", ylab = "PC2 scores", col = mycol, main = 'colored score plot')

x11()
biplot(pc)

library(plotly)
x11()
plot_ly(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], type = "scatter3d", mode = "markers", color = YY, size = 20)

##############
test_indis <- sample(1:nrow(data), nrow(data)/3)
test <- sdata[test_indis,]
test_y <- YY[test_indis]
train <- sdata[-test_indis,]
train_y <- YY[-test_indis]
trainpc <- pc$x[-test_indis, 1:6]
testpc <- pc$x[test_indis, 1:6]
library(class)
knn1 <- knn(train=train, test=test, cl = train_y, k = 15)
acc1 <- 100*sum(test_y == knn1)/NROW(test_y)

knn2 <- knn(train=trainpc, test=testpc, cl = train_y, k = 15)
acc2 <- 100*sum(test_y == knn2)/NROW(test_y)