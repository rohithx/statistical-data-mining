# Written by Rohith
rm(list =ls())
library(class)
library(caret)

set.seed(14)

data(iris)
dim(iris)

rand_index <- sample(1:nrow(iris), 110, replace = FALSE)
iris_train <- iris[rand_index,]
iris_test <- iris[-rand_index,]

train_species <- iris_train[,5]
test_species <- iris_test[,5]

train_meas <- iris_train[,-5]
test_meas <- iris_test[,-5]

#a
accuracy <- c(1:10)
for (i in 1:10){
  iris_pred = knn(train = train_meas, test = test_meas, cl = train_species,k = i)
  #print(table(iris_pred, test_species))
  accuracy[i] <- mean(iris_pred == test_species)
  print(paste('k = ', i, 'accuracy = ', accuracy[i]))
  print(confusionMatrix(iris_pred, test_species))
}
x11()
plot(c(1:10), 1 - accuracy, xlab = 'k value', ylab = 'error')

#b
set.seed(14)

iris_pca <- princomp(iris[,-5])
iris_pca$scores
pc = cbind(-1*iris_pca$scores[,1], -1*iris_pca$scores[,2])

pc_accuracy <- c(1:10)
for (i in 1:10){
  pca_knn_pred <- knn(train = pc[rand_index,], test = pc[-rand_index,], cl = train_species, k = i)
  pc_accuracy[i] <- mean(pca_knn_pred == test_species) 
  print(paste('k = ', i, 'accuracy = ', pc_accuracy[i]))
  print(confusionMatrix(pca_knn_pred, test_species))}
  
x11()
plot(c(1:10), 1 - pc_accuracy, xlab = 'k value', ylab = 'error')

x11()
plot(-1*iris_pca$scores[,1][-rand_index], -1*iris_pca$scores[,2][-rand_index], col = test_species, xlab = 'PC1', ylab = 'PC2')
