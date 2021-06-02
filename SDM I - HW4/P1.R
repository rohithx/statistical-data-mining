# Written by Rohith
rm(list =ls())

library(MVN)
library(MASS)

load("C:/Users/X/Desktop/Diabetes.RData")

#a
x11()
cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(Diabetes[,1:5], pch = 19, cex= 0.9, col = cols[Diabetes$group])

mvresult1 <- mvn(data = Diabetes[,1:5], mvnTest = "mardia")
mvresult1
mvresult2 <- mvn(data = Diabetes[,1:5], mvnTest = "hz")
mvresult2

#b

ldax = lda(group~., data = Diabetes)
ldap = predict(ldax, Diabetes)
mean(ldap$class == Diabetes$group)

qdax = qda(group~., data = Diabetes)
qdap = predict(qdax, Diabetes)
mean(qdap$class == Diabetes$group)

cfml <- table(Diabetes$group,ldap$class)
cfml
cfmq <- table(Diabetes$group,qdap$class)
cfmq

#c

relwt = 1.86
glufast = 184
glutest = 68
instest = 122
sspg = 544
indiv <- data.frame(relwt,glufast,glutest,instest,sspg)
ldaip <- predict(ldax, indiv)
ldaip$class

qdaip <- predict(qdax, indiv)
qdaip$class
