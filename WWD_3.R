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
  plot(hd)
  group.4 <- cutree(hd,4)
  table(animals$Species,group.4)
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
  plot(size,clk)
  line(3)
}
##Q3
{
  ctot.withinss <- function(number,df,method="manhattan"){
    c2 <- cclut(df,number,method=method)
    return(c2$tot.withinss)
  }
  cclk <- sapply(size,tot.withinss,df = animals_df)
  plot(size,cclk)
  x <- cbind(clk,cclk)
}

maxcurv(cclk[1:2])








