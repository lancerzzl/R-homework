library(tidyverse)
library(GGally)
oxygenfull <- read_csv("oxygenfull.csv")
ggpairs(oxygenfull,
        lower = list(continuous = "smooth"),
        diag=list(continuous="barDiag"),
        axisLabels='show')


mod_1 <- lm(formula = OXY ~ 1 + CHLOR + SUN + SPEED,data = oxygenfull)
result <- summary(mod_1)

ceofficients <- result$coefficients

mod_2 <-lm(formula = OXY ~ TEMP,data = oxygenfull)
mod_3 <-lm(formula = OXY ~ ALT, data = oxygenfull)
summary(mod_2)$adj.r.squared
summary(mod_3)$adj.r.squared

chisq.test(oxygenfull$LIGHT,oxygenfull$SUN)