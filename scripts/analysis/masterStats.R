library(tidyverse)
library(lubridate)
library(lme4) # updated version of nlme.
library(sjPlot)

conflicts_prefer(lme4::lmList())

# Run `master_dataframe.R` first.
# 1. Checking the data ditribution-------------------------
hist(NEE$NEE_umol_s_m2)
hist(ER$ER_umol_s_m2)
hist(GEP$GEP_umol_s_m2)

summary(NEE$NEE_umol_s_m2)
summary(ER$ER_umol_s_m2)
summary(GEP$GEP_umol_s_m2)

## Testing if parametric
shapiro.test(NEE$NEE_umol_s_m2) # p-value = 0.02408
shapiro.test(ER$ER_umol_s_m2) # p-value = 0.003179

## Testing homoscedasticity
bartlett.test(NEE_umol_s_m2 ~ site, data = NEE) #p = 0.2472
bartlett.test(NEE_umol_s_m2 ~ treatment, data = NEE) #p = 0.0859

bartlett.test(ER_umol_s_m2 ~ site, data = ER) #p = 0.006026
bartlett.test(ER_umol_s_m2 ~ treatment, data = ER) #p = 0.6964

# 2. Does long term warming affect NEE?-------------------------
## If we proceed to model selection, REML should likely be set to FALSE.
#
# lme4: lmer(formula, data = <>. REML = ??), glmer(), nlmer().

#NEE.mod <- 
NEE.mod <- lmer(NEE_umol_s_m2 ~ treatment + (1|site:plot), data = NEE)
ER.mod <- lmer(ER_umol_s_m2 ~ treatment + (1|site:plot), data = ER)
GEP.mod <- lmer(GEP_umol_s_m2 ~ treatment + (1|site:plot), data = GEP)


lme(NEE_umol_s_m2 ~ treatment, data = NEE)

summary(NEE.mod)

tab_model(NEE.mod, transform = NULL, show.df = TRUE)

