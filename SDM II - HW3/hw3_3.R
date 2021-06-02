#####################
# Written by Rohith #
#####################

rm(list=ls())
graphics.off()

#####################
swiss <- get(load("C:/Users/X/Documents/R/HW3/SwissBankNotes.RData"))

swiss_1<-swiss[1:100,]
swiss_2<-swiss[101:200,]

pc1 <- prcomp(swiss_1)
pc2 <- prcomp(swiss_2)
pc3 <- prcomp(swiss)

summary(pc1)
summary(pc2)
summary(pc3)

#looking at the screeplots
x11()
screeplot(pc1, main ="scree plot for genuine notes")

x11()
screeplot(pc2, main ="scree plot for counterfeit notes")

x11()
screeplot(pc3, main = "scree plot for combined notes")

#looking at the score plots
x11()
plot(pc1$x,pch = 21, bg = "black", cex = 1.3, main = "score plot for genuine notes")
grid()

x11()
plot(pc2$x,pch = 21, bg = "black", cex = 1.3, main = "score plot for counterfeit notes")
grid()

x11()
colors = rep(c("blue","red"),each = 100)
plot(pc3$x,pch = 21,col = colors, bg = colors, cex = 1.3,main = "score plot for combined notes")
grid()
legend("bottomright", legend = c("real","conterfeit"), col = c("blue", "red"), pt.bg = c("blue","red"),  pch = 21)

#looking at the loading plots
x11()
plot(pc1$rotation,pch=21,bg="black",main="loading plot for genuine notes")
text(pc1$rotation,labels=rownames(pc1$rotation),pos=4,offset = 1)

x11()
plot(pc2$rotation,pch=21,bg="black",main ="loading plot for counterfeit notes")
text(pc2$rotation,labels=rownames(pc2$rotation),pos=4,offset = 1)

x11()
plot(pc3$rotation,pch=21,bg="black", main = "loading plot for combined notes")
text(pc3$rotation,labels=rownames(pc3$rotation),pos=3,offset = 1)

#looking at the biplot for the combined notes

x11()
biplot(pc3,main="biplot for combined notes")

