
# 1 -----------------------------------------------------------------------
library(tidyverse)

sbp <- read.csv('sbp.csv')
sbp <- sbp$x
mu <- mean(sbp);mu
set.seed(72340)

n <- 10
sbpsample <- sbp[sample(length(sbp),n)]

mean(sbpsample) - qt(0.975,df = n-1)*sd(sbpsample)/sqrt(n)
mean(sbpsample) + qt(0.975,df = n-1)*sd(sbpsample)/sqrt(n)

nSim <- 10000
n <- 10
ciArray <- array(0, dim=c(nSim,2))

for(i in 1:nSim){
  sbpsample <- sbp[sample(length(sbp),n)]
  ciArray[i,1] <- mean(sbpsample) - qt(0.975,df=n-1)*sd(sbpsample)/sqrt(n)
  ciArray[i,2] <- mean(sbpsample) + qt(0.975,df=n-1)*sd(sbpsample)/sqrt(n)
}

mean((ciArray[,1] < mu) & (mu < ciArray[,2]))
mean(ciArray[,2] - ciArray[,1])

# Q2 ----------------------------------------------------------------------

# a
newborn <- c(7.4,6.0, 8.6,4.5,2.0, 7.9,4.0,
             2.6,5.9, 7.3,7.3,7.0, 6.3,8.1,
             7.1,7.3, 6.6,5.2,9.8, 8.0,10.9,
             6.3,3.8, 5.0,8.0,10.7,9.7,6.0,
             6.8,10.3,7.6,6.5,7.1, 5.8,6.9)
mean(newborn)
sd(newborn)
var(newborn)

ci_1 <- mean(newborn) + qt(0.95,34)*1.91/sqrt(35);ci_1
ci_2 <- mean(newborn) - qt(0.95,34)*1.91/sqrt(35);ci_2

ci_1 <- mean(newborn) + qnorm(0.95)*1.91/sqrt(35);ci_1
ci_2 <- mean(newborn) - qnorm(0.95)*1.91/sqrt(35);ci_2

#b
sd(newborn)
ci_up <- mean(newborn) + qnorm(0.95)*sd(newborn)/sqrt(35);ci_up
ci_low <- mean(newborn) - qnorm(0.95)*sd(newborn)/sqrt(35);ci_low

#c
var_up <- 34*var(newborn)/qchisq(0.95,34);var_up
var_low <- 34*var(newborn)/qchisq(0.05,34);var_low

#d
# increasing the sample size

# Q3 ----------------------------------------------------------------------


# Q4 ----------------------------------------------------------------------

dog <- c(62.0,61.4,59.8,62.2,60.3,60.4,59.4,
         60.2,60.4,60.8,61.8,59.2,61.1,60.4,60.9)
# a
u <- mean(dog);u
sigma <- var(dog);sigma

# b
ci.low4 <- u + qt(0.95,14)*sd(dog)/sqrt(14);ci.low4
ci.up4 <- u + qt(0.95,14)*sd(dog)/sqrt(14);ci.up4

# c
var.up4 <- 14*var(dog)/qchisq(0.95,14);var.up4
var.low4 <- 14*var(dog)/qchisq(0.05,14);var.low4

# Q5 ----------------------------------------------------------------------

# a
wheat <- read_csv("wheatyields.csv", col_names = "yield")

boots <- matrix(NA, nrow(wheat), 10000)
for(i in 1:10000) {
  boots[, i] <- sample.int(nrow(wheat), nrow(wheat), replace = T)
  boots[, i] <- wheat$yield[boots[, i]]
}
boots <- apply(boots, 2, median)
wheatboot <- quantile(boots, probs = c(0.025, 0.975))

hist(boots)
#b
boots <- as.data.frame(boots)
hist <- ggplot(boots)+
  geom_histogram(aes(x = boots,y=..density..),position = 'identity')+
  geom_density(aes(x=boots,y=..density..))
hist

#c
hist + geom_vline(aes(0.95))
