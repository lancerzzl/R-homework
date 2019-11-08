## load packcage
{
  library(lattice)
  library(mice)
  library(tidyverse)
}

## read file
{
  title <- list.files(pattern = "*_Cases.csv")
  registry <- list.files(pattern = "^[^_]+.csv$")
  
  read <- function(x){
    merge.data = read.csv(x[1],header=T,sep=",")   
    for (i in 2:length(x)){
      new.data = read.csv(x[i], header=T, sep=",")
      merge.data = rbind(merge.data,new.data)
    }
    return(merge.data)
  }
  
  reg <- read(registry)
  cancer <- read(title)
}

## fill the data
{
  reg <- select(reg,1:2)
  cancer <- filter(cancer,CANCER == 19)
  totaldata <- merge(x=cancer,y=reg,by = "REGISTRY", all.x = T,all.y = F) %>% 
    select(25,2:24)
}
{
#   reg <- select(reg,1:2)
#   #vari <- c("country","year","sex","number",)
#   cancer <- filter(cancer,CANCER == 19) %>% 
#     rename("REGISTRY","country")
#   
#   replaceID <- function(to,from,mergeby,values){
#     x <- match(from[,mergeby],to[,mergeBy])
#     to[,values][x] <- as.character(from[,values])
#     return(to)
#   }
#   
#   cancer_2 <- replaceID(cancer,reg,REGISTRY,REGISTRY)
 }   

## output a csv file
{
  write.csv(totaldata,file = "cancer_data.csv")
}

 


    
