library(tidyverse)
animals <- read.csv("Animals.csv")  

animals <- rename(animals,Weight = "x",Hight = "y", Specise = "z")
#rename the variation

animals$Specise <- factor(animals$Specise, levels = c("a","b","c","d"),
                          labels = c("Ostrich","Deer","Bear","Gaint tortise"))
#rename the a,b,c,d to ostrich, deer, bear, gaint tortise

animals <- mutate(animals,BMI = animals$Weight/animals$Hight) 
#add another colume BMI = Weight/Hight

ggplot(animals, aes(x=Weight,y=Hight))+
  geom_point()
#polt the picture about weight and hight. I cannot find obverse relationship between weight and hight

ggplot(animals, aes(x= Weight,y = Hight))+
  geom_point()+
  facet_grid(~Specise)
#plot the picture with different specises.
#weight and height are positively correlated in the same species

ggplot(animals,aes(x = Specise,y = BMI))+
  geom_boxplot(fill = 'wheat',color = 'black')
