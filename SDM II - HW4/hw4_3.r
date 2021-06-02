rm(list = ls())
graphics.off()

##################
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
##################

g <- list(~Sex, ~SuffHeartF, ~Smoker|Sex, ~Hyperchol|Smoker:SuffHeartF, ~Inherit|Smoker, ~CAD|Inherit:Hyperchol)
cad1dag <- dagList(g)

x11()
plot(cad1dag)

##################
#Identifying d-sep
##################

dSep(as(cad1dag, "matrix"), "Sex", "SuffHeartF", NULL)
dSep(as(cad1dag, "matrix"), "Smoker", "SuffHeartF", NULL)
dSep(as(cad1dag, "matrix"), "CAD", "Sex", "Smoker")
dSep(as(cad1dag, "matrix"), "Hyperchol", "Sex", "Smoker")

##################
#Extracting cpt
##################

data(cad1, package="gRbase")
pp <- extractCPT(cad1, cad1dag)

#b)
#####################
#Building the network
#####################

plist <- compileCPT(pp)
grn1 <- grain(plist)
summary(grn1)
#Compiled

######################################
#Compiling and Propogating the network
######################################
grn1c <- compile(grn1)
grn1p <- propagate(grn1c)
summary(grn1p)

#########################
#Querying before evidence
#########################

querygrain(grn1p, nodes = c("SuffHeartF","CAD"), type = "marginal")
querygrain(grn1p, nodes = c("SuffHeartF","CAD"), type = "joint")
querygrain(grn1p, nodes = c("SuffHeartF","CAD"), type = "conditional")

#######################
#Absorbing the evidence
#######################

grn1p.ev <- setFinding(grn1p, nodes = c("Sex","Hyperchol"), states = c("Female", "Yes"))

########################
#Querying after evidence
########################

querygrain(grn1p.ev, nodes = c("SuffHeartF","CAD"), type = "marginal")
querygrain(grn1p.ev, nodes = c("SuffHeartF","CAD"), type = "joint")
querygrain(grn1p.ev, nodes = c("SuffHeartF","CAD"), type = "conditional")

#c)
################################
#Simulating with 25 observations
################################
set.seed(500)

grain.twofive <- simulate(grn1p.ev, nsim = 25)
save(grain.twofive, file="25_observations.Rdata")

xtabs(~ Smoker, data = grain.twofive)
xtabs(~ CAD, data = grain.twofive)

predicted_25 <- predict(grn1p, response = c("Smoker", "CAD"), newdata = grain.twofive, predictors = c("Sex", "Hyperchol", "SuffHeartF" , "Inherit"), type = "class")

table(predicted_25$pred$CAD, grain.twofive$CAD)
table(predicted_25$pred$Smoker, grain.twofive$Smoker)

#d)
#################################
#Simulating with 500 observations
#################################
set.seed(500)

grain.fivezerozero <- simulate(grn1p.ev, nsim = 500)
save(grain.fivezerozero, file="500_observations.Rdata")

xtabs(~ Smoker, data = grain.fivezerozero)
xtabs(~ CAD, data = grain.fivezerozero)

predicted_500 <- predict(grn1p, response = c("Smoker", "CAD"), newdata = grain.fivezerozero, predictors = c("Sex", "Hyperchol", "SuffHeartF" , "Inherit"))

table(predicted_500$pred$CAD, grain.fivezerozero$CAD)
table(predicted_500$pred$Smoker, grain.fivezerozero$Smoker)

