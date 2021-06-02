# Writte by Rohith

rm(list =ls())
set.seed(1234)
library(tidyverse)
library(leaps)

gen_data <- data.frame(replicate(20, rnorm(n = 1000)))

gen_data %>% reduce(function(y, x) y + ifelse(runif(1) < 0.4, rnorm(1, mean = 2, sd = 1), 0)*x + rnorm(1000)) -> gen_data.Y

train_gen <- gen_data[1:800,]
test_gen <- gen_data[801:1000,]
train_resp <- as.data.frame(gen_data.Y)[1:800,]
test_resp <- as.data.frame(gen_data.Y)[801:1000,]

bss_data <- regsubsets(x = train_gen, y = train_resp, nvmax = 20)
bss_summary <- summary(bss_data)

test <- cbind(rep(1,length(test_gen[,1])), test_gen)
colnames(test) <- c("(Intercept)", colnames(test[-1]))
test.bss.err = rep(NA, 20)
for (i in 1:20){
  coeff_bss = coef(bss_data, id = i)
  bss_pred = as.matrix(test[,names(coeff_bss)])%*%coeff_bss
  err_bss = (test_resp-bss_pred)^2
  test.bss.err[i] = mean(err_bss)
}

x11()
plot((bss_summary$rss)/length(train_resp), col = "blue", type = "b", xlab = "Subset Size", ylab = "MSE")
lines(test.bss.err, col = 'red')
legend("topright", c("Training","Testing"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))