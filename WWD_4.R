# load package ---------------------------------------------------------
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjstats)
library(sjPlot)
library(sjmisc)
library(arm)
library(magrittr)


# question 1 --------------------------------------------------------------

schoolboys <- read_xls('schoolboys.xls')
schoolboys <- rename(schoolboys,
                     Age = "Age centered about 13 years",
                     Height = "Height (cm)",
                     Season = "Season,months from start of year")
schoolboys$ID <- as.factor(schoolboys$ID)

# question 2 --------------------------------------------------------------

mod1 <- lm(Height ~ 1 + Age + Season, schoolboys)
summary(mod1)


# question 3 --------------------------------------------------------------

schoolboylmer1 <- lmer(Height ~ Age + (1 | ID), data = schoolboys)
summary(schoolboylmer1)
performance::icc(schoolboylmer1)



schboymerdiag <- data.frame(
  residuals = resid(schoolboylmer1),
  ID = schoolboys$ID,
  Fitted = fitted(schoolboylmer1)
)

ggplot(schboymerdiag, aes(x = Fitted, y = residuals, col = ID)) +
  geom_point() +
  facet_wrap( ~ ID) +
  ggtitle("Lowest level residuals facetting by boy")



plot_model(schoolboylmer1, type = "re")

plot_model(schoolboylmer1, type = "eff", terms = "Age")



# question4 ---------------------------------------------------------------

schoolboylmer2 <- lmer(Height ~ Age + Season + (1 + Age | ID), schoolboys)
summary(schoolboylmer2)


schboymer2diag <- data.frame(
  residuals = resid(schoolboylmer2),
  ID = schoolboys$ID,
  Fitted = fitted(schoolboylmer2)
)

ggplot(schboymer2diag, aes(x = Fitted, y = residuals, col = ID)) +
  geom_point() +
  facet_wrap( ~ ID) +
  ggtitle("Lowest level residuals facetting by boy")
anova(schoolboylmer1, schoolboylmer2)
