# load package ------------------------------------------------------------
library(tidyverse)
library(sf)
library(spdep)
library(rgdal)
library(rgeos)
library(CARBayes)
library(RColorBrewer)
# insert data file --------------------------------------------------------
copd.observed <- read.csv("copdobserved.csv")
copd.expected <- read.csv("copdexpected.csv")
england <- readOGR(dsn = '.',layer = 'englandlocalauthority')

# summarise the number of hospital 2001 to 2010 ---------------------------
copd.expected$ex.summarise <- rowSums(copd.expected[, 2:11])

copd.observed$ob.summarise <- rowSums(copd.observed[, 2:11])

England_data <- merge(copd.expected, copd.observed, by = "Name") %>%
  select_('Name', 'ob.summarise', 'ex.summarise') %>%
  mutate(SMR_raw = ob.summarise / ex.summarise) %>%
  rename(name = 'Name')

# SMR ---------------------------------------------------------------------
SMR <- merge(england,England_data,by = 'name')

SMR <- st_as_sf(SMR)

ggplot(SMR,aes(fill = SMR_raw))+
  geom_sf(colour = NA)+
  theme_bw()+
  labs(x="Longitude",y="Latitude",fill="SMR")+
  scale_fill_gradientn(colours =brewer.pal(9,'RdPu'),
                       breaks =c(0,1,2,3,4,5,6))


# modulling SMR -----------------------------------------------------------

W.nb <- poly2nb(england,row.names = rownames(england))
W.mat <- nb2mat(W.nb,style = 'B')

England_data$ex.summarise <- as.integer(England_data$ex.summarise)
model <- S.CARleroux(formula = ob.summarise ~ offset(log(ex.summarise)),
                     data = England_data,
                     family = 'poisson',
                     W = W.mat,
                     burnin = 20000,
                     n.sample = 100000,
                     thin = 10,
                     rho = 1
                     )
England_data$FittedValue  <- model$fitted.values
England_data$SMR_smooth <- England_data$FittedValue/England_data$ex.summarise

SMR <- merge(england,England_data,by = 'name')
SMR <- st_as_sf(SMR)
#plot
ggplot(SMR,
       aes(fill = SMR_smooth))+
  geom_sf(colour = NA)+
  theme_bw()+
  labs(x = 'Longitude',y = 'Latutude',fill = 'SMR')+
  scale_fill_gradientn(colours = brewer.pal(9,'RdPu'),
                       breaks = c(1:6))

ggplot(England_data,aes(SMR_raw,SMR_smooth))+
  geom_point(colour = 'blue')+
  geom_abline(intercept = 0,slope = 1,colour = 'red')+
  labs(x='Raw SMRs',y='Smooth SMRs')+
  theme_bw()


# Time changes ------------------------------------------------------------


