#Q2
x1 <- c("A","B")
y1 <- sample(x1,20,replace = T)
A <- table(y1)
B <- prop.table(A)
B

# Q3
x2 <- dbinom(25:100,100,1/6)
sum(x2)

y2 <- pbinom(24,100,1/6)
1-y2

#Q4 c
a1 <- c(1,0)
L1 <- 100
C1 <- 100
data <- matrix(0,120,3)
for (i in 0:120) {
  data[i+1,] <- c(i,L1,C1)
  L <- sample(a1,L1,replace = T,prob = c(0.985,0.015))
  C <- sample(a1,C1,replace = T,prob = c(0.99,0.01))
  X <- L1
  L1 <- C1-sum(C) + sum(L)
  C1 <- X-sum(L) + sum(C)
}
data
#c
data <- as.data.frame(data)
colnames(data) <- c("month","labour","conservative")
library(ggplot2)
p = ggplot(data = data)+
  geom_line(aes(x=month,y=labour,color="red"))+
  geom_line(aes(x=month,y=conservative,color="blue"))+
  labs(x="Month",y="supporters",blue="conservative")+
  scale_colour_discrete(name="Party",labels = c('conservative','labour'))
p
  
  
  
  
  