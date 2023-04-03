library(tidyverse)
library(here)
library(lubridate)
library(lme4) # updated version of nlme.
library(lmerTest)
library(sjPlot)
library(conflicted)
# Specify which package the stats should perfer.
conflicts_prefer(lmerTest::lmList(),
                 lmerTest::lmer())

# Run `master_dataframe.R` first.
# source(here("scripts/data_assembly/master_dataframe.R"))

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
## If we proceed to model selection, REML should likely be set to FALSE. Otherwise, true.
# lme4: lmer(formula, data = <>. REML = ??), glmer(), nlmer().

# BASIC MODEL (1): FLUX AND TREATMENT
# First, set up a random effects structure.
NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (1|plot:site), data = NEE)
# Estimating the effect of treatment accounting for the variation in flux 
# (itself) across the 12 plots. The nested nature of plot:site means the model is 
# looking at 12 factors (3 site * 4 plot).

NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (1|site/plot), data = NEE)
summary(NEE.mod1)
tab_model(NEE.mod1, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)
# Estimating the effect of treatment accounting for the variation in flux across 
# sites, but ALSO the variation in `the effect of treatment on flux` across 
# the 12 plot:site factors. THIS MODEL SELECTED.

NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (treatment|plot:site), data = NEE)
# Estimating the effect of treatment accounting for the variation in flux across 
# plots, but ALSO the variation in `the effect of treatment on flux` across 
# sites. BOUNDARY (SINGLUAR) FIT.

NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (treatment|site/plot), data = NEE)
# Estimating the effect of treatment accounting for the variation in flux across 
# sites (1|site) and plots (1|site:plot), but ALSO the variation in `the effect 
# of treatment on flux` across sites and plots. FILED TO CONVERGE.

ER.mod1 <- lmer(ER_umol_s_m2 ~ treatment + (1|site/plot), data = ER)
summary(ER.mod1) # Estimate treatmentT = -0.0029292, p = 0.00115 **
tab_model(ER.mod1, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

GEP.mod1 <- lmer(GEP_umol_s_m2 ~ treatment + (1|site/plot), data = GEP)
summary(GEP.mod1) # Estimate treatmentT = 0.008918, p = 0.0245*
tab_model(GEP.mod1, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)


# 3. Test for differences between sites too.-------------------------
NEE.mod2 <- lmer(NEE_umol_s_m2 ~ treatment + site + (1|site:plot), data = NEE) 
summary(NEE.mod2)

GEP.mod2 <- lmer(GEP_umol_s_m2 ~ treatment + site + (1|site:plot), data = GEP) 
summary(GEP.mod2)

ER.mod2 <- lmer(ER_umol_s_m2 ~ treatment + site + (1|site:plot), data = ER) 
summary(ER.mod2)
 

# 4. Do treatment, GEI, SM, T_air, T_soil affect NEE? ?-------------------------
NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (treatment|site) + (1|site/plot), data = NEE)
# BOUNDARY IS SINGULAR

NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + (1|site:plot), data = NEE)
summary(NEE.mod3)
tab_model(NEE.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

GEP.mod3 <- lmer(GEP_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site:plot), data = GEP)
#GEP.mod3 <- lmer(GEP_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site/plot), data = GEP)
summary(GEP.mod3)
tab_model(GEP.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

ER.mod3 <- lmer(ER_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site:plot), data = ER)
# ER.mod3 <- lmer(ER_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site/plot), data = ER)
summary(ER.mod3)
tab_model(ER.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)



# 5. Adding in site as a fixed effect.-----------------------------------------
NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = NEE)
summary(NEE.mod3)
tab_model(NEE.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

ER.mod3 <- lmer(ER_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + T_soil + (1|site:plot), data = ER)
summary(ER.mod3)
tab_model(ER.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

GEP.mod3 <- lmer(GEP_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + T_soil + (1|site:plot), data = GEP)
summary(GEP.mod3)
tab_model(GEP.mod3, p.val = "kr", show.fstat = TRUE, show.df = TRUE, show.obs = TRUE)

# "parameter estimate of the slope of the relationship"
