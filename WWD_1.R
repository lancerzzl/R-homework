hiv <- read_csv('indicator hiv estimated prevalence% 15-49.csv')
#open the file of data
library("tidyverse")
gp_hiv <- gather(hiv,key = "year",value = "prevalence",14:34)
#tidy the data, chosen data after 1991
gp_hiv <- select(gp_hiv,1,14,15)
#select the tidy data
names(gp_hiv)[1] <- "country"
#give the column name for the data
#gp_hiv$year <- as.numeric(gp_hiv$year)
#switch the data into numbers


gp_hiv

data <- gp_hiv %>%
  group_by(country) %>%
  summarise(MeanPrevalence = mean(prevalence))
write.table(x = data,file = "data.txt")
#output the table
