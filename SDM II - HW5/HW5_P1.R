rm(list = ls())
graphics.off()

library(ggplot2)

# Reading the titanic.csv
# Will have to change the directory to the folder with the csv

t_data <- read.csv(("titanic.csv"))
summary(t_data)

# Creating subsets for all men, women, children and elders
all_men <- subset(t_data, Sex == 'male' & Age >= 18)
all_women <- subset(t_data, Sex == 'female' & Age >= 18)
all_children <- subset(t_data, Age < 18)
all_elders <- subset(t_data, Age >= 65)

all_not_survived <- subset(t_data, Survived == 0)
all_survived <- subset(t_data, Survived == 1)
men_survived <- subset(all_men, Survived == 1)
women_survived <- subset(all_women, Survived == 1)
children_survived <- subset(all_children, Survived == 1)
elders_survived <- subset(all_elders, Survived == 1)

# Plotting bar graph on all passengers that survived
data1 <- data.frame(group = c("Men","Women","Children"), value = c(100*nrow(men_survived)/nrow(all_men), 100*nrow(women_survived)/nrow(all_women), 100*nrow(children_survived)/nrow(all_children)), number_of_obs = c(nrow(all_men),nrow(all_women),nrow(all_children)))
data1$right <- cumsum(data1$number_of_obs) + 30*c(0:(nrow(data1)-1))
data1$left <- data1$right - data1$number_of_obs
x11()
ggplot(data1, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("Size of the Demographic") + 
  ylab("Percentage of Demographic Survived")

# Creating subsets for men, women and children in 1st class
all_class_1 <- subset(t_data, Pclass == 1)
men_class_1 <- subset(all_men, Pclass == 1)
women_class_1 <- subset(all_women, Pclass == 1)
children_class_1 <- subset(all_children, Pclass == 1)

all_class_1_survived <- subset(all_survived, Pclass == 1)
men_class_1_survived <- subset(men_survived, Pclass == 1)
women_class_1_survived <- subset(women_survived, Pclass == 1)
children_class_1_survived <- subset(children_survived, Pclass == 1)


# Plotting bar graph on 1st class passengers that survived
data2 <- data.frame(group = c("Men in 1st class","Women in 1st class","Children in 1st class"), value = c(100*nrow(men_class_1_survived)/nrow(men_class_1), 100*nrow(women_class_1_survived)/nrow(women_class_1), 100*nrow(children_class_1_survived)/nrow(children_class_1)), number_of_obs = c(nrow(men_class_1),nrow(women_class_1),nrow(children_class_1)))
data2$right <- cumsum(data2$number_of_obs) + 30*c(0:(nrow(data2)-1))
data2$left <- data2$right - data2$number_of_obs
x11()
ggplot(data2, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("Size of the Demographic") + 
  ylab("Percentage of Demographic Survived")

# Creating subsets for men, women and children in 2nd class
all_class_2 <- subset(t_data, Pclass == 2)
men_class_2 <- subset(all_men, Pclass == 2)
women_class_2 <- subset(all_women, Pclass == 2)
children_class_2 <- subset(all_children, Pclass == 2)

all_class_2_survived <- subset(all_survived, Pclass == 2)
men_class_2_survived <- subset(men_survived, Pclass == 2)
women_class_2_survived <- subset(women_survived, Pclass == 2)
children_class_2_survived <- subset(children_survived, Pclass == 2)

# Plotting bar graph on 1st class passengers that survived
data3 <- data.frame(group = c("Men in 2nd class","Women in 2nd class","Children in 2nd class"), value = c(100*nrow(men_class_2_survived)/nrow(men_class_2), 100*nrow(women_class_2_survived)/nrow(women_class_2), 100*nrow(children_class_2_survived)/nrow(children_class_2)), number_of_obs = c(nrow(men_class_2),nrow(women_class_2),nrow(children_class_2)))
data3$right <- cumsum(data3$number_of_obs) + 30*c(0:(nrow(data3)-1))
data3$left <- data3$right - data3$number_of_obs
x11()
ggplot(data3, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("Size of the Demographic") + 
  ylab("Percentage of Demographic Survived")

# Creating subsets for men, women and children in 3rd class
all_class_3 <- subset(t_data, Pclass == 3)
men_class_3 <- subset(all_men, Pclass ==3)
women_class_3 <- subset(all_women, Pclass == 3)
children_class_3 <- subset(all_children, Pclass == 3)

all_class_3_survived <- subset(all_survived, Pclass == 3)
men_class_3_survived <- subset(men_survived, Pclass== 3)
women_class_3_survived <- subset(women_survived, Pclass == 3)
children_class_3_survived <- subset(children_survived, Pclass == 3)

# Plotting bar graph on 3rd class passengers that survived
data4 <- data.frame(group = c("Men in 3rd class","Women in 3rd class","Children in 3rd class"), value = c(100*nrow(men_class_3_survived)/nrow(men_class_3), 100*nrow(women_class_3_survived)/nrow(women_class_3), 100*nrow(children_class_3_survived)/nrow(children_class_3)), number_of_obs = c(nrow(men_class_3),nrow(women_class_3),nrow(children_class_3)))
data4$right <- cumsum(data4$number_of_obs) + 30*c(0:(nrow(data4)-1))
data4$left <- data4$right - data4$number_of_obs
x11()
ggplot(data4, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("Size of the Demographic") + 
  ylab("Percentage of Demographic Survived")

# Creating subsets for male and female passengers
all_male <- subset(t_data, Sex == 'male')
all_female <- subset(t_data, Sex == 'female')

male_survived <- subset(all_male, Survived == 1)
female_survived <- subset(all_female, Survived == 1)

# Plotting bar graph based on gender only
data5 <- data.frame(group = c("Male Passengers","Female Passengers"), value = c(100*nrow(male_survived)/nrow(all_male), 100*nrow(female_survived)/nrow(all_female)), number_of_obs = c(nrow(all_male),nrow(all_female)))
data5$right <- cumsum(data5$number_of_obs) + 30*c(0:(nrow(data5)-1))
data5$left <- data5$right - data5$number_of_obs
x11()
ggplot(data5, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("Size of the Demographic") + 
  ylab("Percentage of Demographic Survived")

# Plotting bar graphs to show the demographics amongst survivors

data6 <- data.frame(group = c("Men","Women","Children"), value = c(nrow(men_survived),nrow(women_survived),nrow(children_survived)))
bp1<- ggplot(data6, aes(x="", y=value , fill=group))+
  geom_bar(width = 1, stat = "identity") + xlab("All Survivors") + ylab("Number of Survivors")
x11()
bp1

data7 <- data.frame(group = c("Men","Women","Children"), value = c(nrow(men_class_1_survived),nrow(women_class_1_survived),nrow(children_class_1_survived)))
bp2<- ggplot(data7, aes(x="", y=value , fill=group))+
  geom_bar(width = 1, stat = "identity") + xlab("Survivors from 1st Class") + ylab("Number of Survivors")
x11()
bp2

data8 <- data.frame(group = c("Men","Women","Children"), value = c(nrow(men_class_2_survived),nrow(women_class_2_survived),nrow(children_class_2_survived)))
bp3<- ggplot(data8, aes(x="", y=value , fill=group))+
  geom_bar(width = 1, stat = "identity") + xlab("Survivors from 2nd Class") + ylab("Number of Survivors")
x11()
bp3

data9 <- data.frame(group = c("Men","Women","Children"), value = c(nrow(men_class_3_survived),nrow(women_class_3_survived),nrow(children_class_3_survived)))
bp4<- ggplot(data9, aes(x="", y=value , fill=group))+
  geom_bar(width = 1, stat = "identity") + xlab("Survivors from 3rd Class") + ylab("Number of Survivors")
x11()
bp4

# Calculating the number of passengers and in different classes travelling alone.
alone_class_1_x <- subset(all_class_1, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)
alone_class_2_x <- subset(all_class_2, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)
alone_class_3_x <- subset(all_class_3, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)

alone_class_1_y <- subset(all_class_1_survived, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)
alone_class_2_y <- subset(all_class_2_survived, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)
alone_class_3_y <- subset(all_class_3_survived, Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0)
