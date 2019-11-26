sbp <- read.csv('sbp.csv')
wheatyield <- read.csv('wheatyields.csv',col.names = 'yield')

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