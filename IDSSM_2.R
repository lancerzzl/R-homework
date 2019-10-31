# q2 a
library(tidyverse)
E1 <- rexp(10000,rate = 2)
hist(E1)

#b
E2 <- rexp(10000,rate = 3)
hist(E2)

#c
y <- pmin(E1,E2)
hist(y)

#d
#it follow expon
1/mean(y)

rand.gen <-function(N=10){
  a <- 3
  m <- 31
  d <- 2 # seed
  rn <-vector(length=N)# initialising the vector
  for(i in 1:N){
    d <- (a*d)%%m
    rn[i] <- d/m
  }
  return(rn)
}

#b
rand.gen(N = 1000)

rand.gen2 <-function(N=10){
  a <- 65539
  m <- 2^13
  d <- 2019 # seed
  rn <-vector(length=N)# initialising the vector
  for(i in 1:N){
    d <- (a*d)%%m
    rn[i] <- d/m
  }
  return(rn)
}

rn <- rand.gen2(N = 1000)

#c
rn <- 2*rn +3

hist(rn)
# this is Uniform Distribution
# a = 0, b = 1