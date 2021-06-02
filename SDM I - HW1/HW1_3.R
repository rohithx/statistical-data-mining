# Written by Rohith Reddy

rm(list = ls())
library(MASS)

?Boston
dim(Boston)
head(Boston)

# scatterplots of lstat and other variables
par(mfrow=c(3,3))
plot(Boston$lstat, Boston$medv)
plot(Boston$lstat, Boston$tax)
plot(Boston$lstat, Boston$age)
plot(Boston$dis, Boston$lstat)
plot(Boston$dis, Boston$indus)
plot(Boston$zn, Boston$indus)
plot(Boston$zn, Boston$age)
plot(Boston$zn, Boston$lstat)
plot(Boston$crim, Boston$tax)

# scatterplots of crime and other variables
par(mfrow=c(3,3))
plot(Boston$crim, Boston$dis)
plot(Boston$crim, Boston$ptratio)
plot(Boston$crim, Boston$tax)
plot(Boston$crim, Boston$black)
plot(Boston$crim, Boston$lstat)
plot(Boston$crim, Boston$rad)
plot(Boston$crim, Boston$age)
plot(Boston$crim, Boston$rm)
plot(Boston$crim, Boston$zn)

# Subetting suburbs averaging more than 7 and 8 dwellings.
s7 <- Boston[which(Boston$rm > 7),]
dim(s7)

s8 <- Boston[which(Boston$rm > 8),]
dim(s8)

# histogram for s8
hist(s8$crim, breaks = 20)
hist(s8$tax, breaks = 20)
hist(s8$dis, breaks =20) 
