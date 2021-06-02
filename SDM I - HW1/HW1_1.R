# written by Rohith Reddy

# Loading the ISLR Package
library(ISLR)

# Finding details about the Auto dataset
dim(Auto)
head(Auto)

# Scatterplots ahead

# Scatterplot of mpg vs weight
plot(Auto$weight, Auto$mpg, col = Auto$origin, xlab = 'Car Weight', ylab = 'Miles per Gallon', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of mpg vs horsepower
plot(Auto$horsepower, Auto$mpg, col = Auto$origin, xlab = 'Engine Horspower', ylab = 'Miles per Gallon', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of weight vs horsepower
plot(Auto$horsepower, Auto$weight, col = Auto$origin, xlab = 'Engine Horspower', ylab = 'Car Weight', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of year vs mpg
plot(Auto$year, Auto$mpg, col = Auto$origin, xlab = 'Year', ylab = 'Miles per Gallon', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of year vs weight
plot(Auto$year, Auto$weight, col = Auto$origin, xlab = 'Year', ylab = 'Car Weight', pch = 19)
legend('bottomright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of displacement vs mpg
plot(Auto$displacement, Auto$mpg, col = Auto$origin, xlab = 'displacement', ylab = 'mpg', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of cylinders vs mpg
plot(Auto$cylinders, Auto$mpg, col = Auto$origin, xlab = 'cylinders', ylab = 'mpg', pch = 19)
legend('topright', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Scatterplot of accelaration vs mpg
plot(Auto$acceleration, Auto$mpg, col = Auto$origin, xlab = 'acceleration', ylab = 'mpg', pch = 19)
legend('topleft', legend = c('American', 'European', 'Asian'), col = 1:3, pch = 19)

# Histogram of weight
hist(Auto$weight, main = 'Histogram of vehicle weight')

# Boxplots for outliers
par(mfrow=c(2,2))
boxplot(Auto$displacement, horizontal=TRUE, main="Engine Displacement")
boxplot(Auto$horsepower, horizontal=TRUE, main="Engine Horsepower")
boxplot(Auto$weight, horizontal=TRUE, main="Vehicle Weight")
boxplot(Auto$acceleration, horizontal=TRUE, main="Vehicle Acceleration")

# Creating a subset to export
# Removing origin and name 
out_data <- Auto[c(-8,-9)]
