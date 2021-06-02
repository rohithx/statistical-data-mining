rm(list = ls())
graphics.off()

library(igraphdata)
library(igraph)

data(karate)
?karate
data(kite)
?kite

#################
#a)
set.seed(1244)

karate_edges <- E(karate)
length(karate_edges)
#5% of 78 is approximately 4 
random_edges_1 <- sample(karate_edges, size=4, replace = FALSE)
removed_karate <- delete_edges(karate, random_edges_1)
hrg_1 <- fit_hrg(removed_karate)
pred_edges_1 <- predict_edges(removed_karate)
random_edges_1
pred_edges_1

E(karate)$color <- "gray"
E(karate)[random_edges_1]$color <- "red"
x11()
plot(karate)

x11()
plot(removed_karate)

x11()
plot_dendrogram(hrg_1)

col_1 <- rep('green',100)
col_1[c(2,26,20,8)] <- 'red' 

x11()
plot(pred_edges_1$prob[1:100], col = col_1, ylab = "Probability")

E(removed_karate)$color <- 'gray'
graph_1 <- add_edges(removed_karate, t(pred_edges_1$edges[1:4, ]), color = 'red')
x11()
plot(graph_1)

#################
#b)
set.seed(3123)
kite_edges <- E(kite)
length(kite_edges)
#5% of 18 is approximately 1 
random_edges_2 <- sample(kite_edges, size=1, replace = FALSE)
removed_kite <- delete_edges(kite, random_edges_2)
hrg_2 <- fit_hrg(removed_kite)
pred_edges_2 <- predict_edges(removed_kite)
random_edges_2
pred_edges_2
removed_kite

E(kite)$color <- 'gray'
E(kite)[random_edges_2]$color <- 'red'
x11()
plot(kite)

x11()
plot(removed_kite)

x11()
plot_dendrogram(hrg_2)

col_2 <- rep('green',28)
col_2[2] <- 'red' 

x11()
plot(pred_edges_2$prob, col = col_2, ylab = 'Probability')

E(removed_kite)$color <- 'gray'
graph_2 <- add_edges(removed_kite, t(pred_edges_2$edges[1:3, ]), color = 'red')
x11()
plot(graph_2)
##################
#c)
set.seed(1244)

#Repeating a) with 15%
#15% of 78 is approximately 12 
random_edges_3 <- sample(karate_edges, size=12, replace = FALSE)
removed_karate_2 <- delete_edges(karate, random_edges_3)
hrg_3 <- fit_hrg(removed_karate_2)
pred_edges_3 <- predict_edges(removed_karate_2)
random_edges_3
pred_edges_3

E(karate)$color <- "gray"
E(karate)[random_edges_3]$color <- "red"
x11()
plot(karate)

x11()
plot(removed_karate_2)

x11()
plot_dendrogram(hrg_3)

col_3 <- rep('green',100)
col_3[c(1,3,4,12,14,17,22,27,28,85)] <- 'red' 

pred_edges_3$edges[c(1,3,4,12,14,17,22,27,28,85),]

x11()
plot(pred_edges_3$prob[1:100], col = col_3, ylab = 'Probability')

E(removed_karate_2)$color <- 'gray'
graph_3 <- add_edges(removed_karate_2, t(pred_edges_3$edges[1:15, ]), color = 'red')
x11()
plot(graph_3)

#40% of 78 is approximately 31
set.seed(1423)
data("karate")
random_edges_4 <- sample(karate_edges, size=31, replace = FALSE)
removed_karate_3 <- delete_edges(karate, random_edges_4)
hrg_4 <- fit_hrg(removed_karate_3)
pred_edges_4 <- predict_edges(removed_karate_3)
random_edges_4
pred_edges_4

E(karate)$color <- "gray"
E(karate)[random_edges_4]$color <- "red"
x11()
plot(karate)

x11()
plot(removed_karate_3)

x11()
plot_dendrogram(hrg_4)

col_4 <- rep('green',150)
col_4[c(54,5,58,7,8,19,6,52,41,3,18,11,96,44,71,4,124,10,78,73,120,33,133,17,27)] <- 'red' 

x11()
plot(pred_edges_4$prob[1:150], col = col_4,ylab = 'Probability')

E(removed_karate_3)$color <- 'gray'
graph_4 <- add_edges(removed_karate_3, t(pred_edges_4$edges[1:35, ]), color = 'red')
x11()
plot(graph_4)

#Repeating (b) with 15%
set.seed(3123)
data("kite")
#15% of 18 is approximately 3 
random_edges_5 <- sample(kite_edges, size=3, replace = FALSE)
removed_kite_2 <- delete_edges(kite, random_edges_5)
hrg_5 <- fit_hrg(removed_kite_2)
pred_edges_5 <- predict_edges(removed_kite_2)
random_edges_5
pred_edges_5

E(kite)$color <- 'gray'
E(kite)[random_edges_5]$color <- 'red'
x11()
plot(kite)

x11()
plot(removed_kite_2)

x11()
plot_dendrogram(hrg_5)

col_5 <- rep('green',30)
col_5[c(8,7,15)] <- 'red' 

x11()
plot(pred_edges_5$prob, col = col_5, ylab = 'Probability')

E(removed_kite_2)$color <- 'gray'
graph_5 <- add_edges(removed_kite_2, t(pred_edges_5$edges[1:5, ]), color = 'red')
x11()
plot(graph_5)

#Repeating (b) with 40%
set.seed(415)
data("kite")
#40% of 18 is approximately 7 
random_edges_6 <- sample(kite_edges, size=7, replace = FALSE)
removed_kite_3 <- delete_edges(kite, random_edges_6)
hrg_6 <- fit_hrg(removed_kite_3)
pred_edges_6 <- predict_edges(removed_kite_3)
random_edges_6
pred_edges_6

E(kite)$color <- 'gray'
E(kite)[random_edges_6]$color <- 'red'
x11()
plot(kite)

x11()
plot(removed_kite_3)

x11()
plot_dendrogram(hrg_6)

col_6 <- rep('green',34)
col_6[c(11,9,3,31,4,1,17)] <- 'red' 

x11()
plot(pred_edges_6$prob, col = col_6, ylab = 'Probability')

E(removed_kite_3)$color <- 'gray'
graph_6 <- add_edges(removed_kite_3, t(pred_edges_6$edges[1:7, ]), color = 'red')
x11()
plot(graph_6)
