{ ## load the package
library(ggmap)
library(maptools)
library(maps)
library(tidyverse)
}

{## Read the data
  visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury, SA")
  ll.visited <- geocode(visited)
  visit.x <- ll.visited$lon
  visit.y <- ll.visited$lat
}

{
  map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
  points(visit.x,visit.y, col="red", pch=16)
  
}

{
  worldmap <- readShapePoly(TM_WORLD_BORDERS.shp)
}