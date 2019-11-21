
# load the library --------------------------------------------------------
library(tidyverse)
library(sf)
library(spdep)
library(rgdal)
library(rgeos)
library(CARBayes)
library(RColorBrewer)

# load --------------------------------------------------------------------

scotland.data <- read.csv("Scotland.csv")
scotland <- readOGR(dsn = '.',layer = 'Scotland')

scotland.data$SMR_raw <- scotland.data$Observed/scotland.data$Expected

# SMR ---------------------------------------------------------------------

SMR <- merge(scotland,scotland.data,by='Name')
SMR <- st_as_sf(SMR)

ggplot(SMR,aes(fill = SMR_raw))+
  geom_sf(colour = NA)+
  theme_bw()+labs(x ='Longitude',y ='Latitude',fill ='SMR')+
  scale_fill_gradientn(colours =brewer.pal(9,'RdPu'),
                       breaks =c(0,1,2,3,4,5,6))

# modelling SMR -----------------------------------------------------------

w.nb <- poly2nb(scotland,row.names = rownames(scotland))
w.mat <- nb2mat(w.nb,style = "B")
model <- S.CARleroux(formula = Observed ~ offset(log(Expected)),
                     data = scotland.data,
                     family = 'poisson',
                     W = w.mat,
                     burnin = 20000,
                     n.sample = 100000,
                     thin = 10,
                     rho = 1)
scotland.data$FittedValue  <- model$fitted.values
scotland.data$SMR_smooth <- scotland.data$FittedValue/scotland.data$Expected

SMR <- merge(scotland,scotland.data,by = 'Name')
SMR <- st_as_sf(SMR)
#plot
ggplot(SMR,
       aes(fill = SMR_smooth))+
  geom_sf(colour = NA)+
  theme_bw()+
  labs(x = 'Longitude',y = 'Latutude',fill = 'SMR')+
scale_fill_gradientn(colours = brewer.pal(9,'RdPu'),
                     breaks = c(1:6))
#plot of raw us smoothed SMRs
ggplot(scotland.data,aes(SMR_raw,SMR_smooth))+
  geom_point(colour = 'blue')+
  geom_abline(intercept = 0,slope = 1,colour = 'red')+
  labs(x='Raw SMRs',y='Smooth SMRs')+
  theme_bw()