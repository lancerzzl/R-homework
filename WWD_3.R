## load the packeage
{
  library(cclust)
  library(tidyverse)
}
## read file
{
  animals <- read.csv("Animals.csv")
}
## rename the animals
{
  animals <-rename(animals,Weight = 'x',Height = 'y',Species = 'z') 
  animals$Species <- factor(animals$Species, levels = c("a","b","c","d"),
                            labels = c("Ostrich","Deer","Bear","Gaint tortise"))
}
## Q1 perform hierarchical clustering
{
  d <- cbind(animals["Weight"],animals["Height"]) %>% 
    dist()
  hd <- hclust(d)
  group.4 <- cutree(hd,4)
  table(animals$Species,group.4)
}

## build a function to conculate the max carvature
{
  curv <- function(x,y){
    ta <- sqrt((x[2]-x[1])^2+(y[2]-y[1])^2)
    tb <- sqrt((x[3]-x[2])^2+(y[3]-y[2])^2)
    M_matrix <- matrix(c(1,ta,ta^2,1,0,0,1,tb,tb^2),3,3,byrow=TRUE)
    M_inverse <- solve(M_matrix)
    a <- M_inverse %*% x
    b <- M_inverse %*% y
    curvature <- 2*(a[3]*b[2]-a[2]*b[3])/sqrt((a[2]^2+b[2]^2)^3)
    return(curvature)
  }
  max.curv <- function(x,y) {
      I <- c(2:(length(x)-1))
      cur <- I
    for (i in I){
      s.x <- c(x[i-1],x[i],x[i+1])
      s.y <- c(y[i-1],y[i],y[i+1])
      cur[i] <- curv(s.x,s.y)
    }
      cur <- abs(cur)
      max <- max(cur)
      n <- which.max(cur)+1
      result <- list(max,n,cur) 
      names(result) <-c("max value","number","curature")
      return(result)
  }  
}
## Q2 
{
  animals_df <- cbind(animals["Weight"],animals["Height"])
  
  tot.withinss <- function(number,df){
    c1 <- kmeans(df,number)
    return(c1$tot.withinss)
  }
  size = c(2:10)
  clk <- sapply(size,tot.withinss,df = animals_df)
  max.curv(size,clk)
  plot(size,clk)
  abline(v=3)
  
  kc <- kmeans(animals_df,3)
  table(animals$Species,kc$cluster)
}




##Q3
{
  ctot.withinss <- function(number,df,dist="manhattan"){
    df <- as.matrix(df)
    c2 <- cclust(df,number,dist = dist)
    return(sum(c2$withinss))
  }
  cclk <- sapply(size,ctot.withinss,df = animals_df)
  max.curv(size,cclk)
  plot(size,cclk)
  abline(v=3)
  x <- cbind(clk,cclk)
  
  ckc <- animals_df %>% as.matrix() %>% 
    cclust(3,dist = "manhattan")

}










