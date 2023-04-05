library(tidyverse)
library(here)
library(lubridate)
library(lme4) # updated version of nlme.
library(lmerTest)
library(conflicted)
library(performance)
# Specify which package the stats should perfer.
conflicts_prefer(lmerTest::lmList(),
                 lmerTest::lmer(),
                 lmerTest::step(),
                 performance::r2())

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

# AVERAGES
mean.C <- mean(pull(filter(NEE, treatment == "C", site = "DRYAS")["NEE_umol_s_m2"]))
mean.T <- mean(pull(filter(NEE, treatment == "T", site = "DRYAS")["NEE_umol_s_m2"])) 

# Percent difference
1-mean.C/mean.T

# BASIC MODEL (1): FLUX AND TREATMENT
# First, set up a random effects structure.
NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (1|plot:site), data = NEE)
# Estimating the effect of treatment accounting for the variation in flux 
# (itself) across the 12 plots. The nested nature of plot:site means the model is 
# looking at 12 factors (3 site * 4 plot).

NEE.mod1 <- lmer(NEE_umol_s_m2 ~ treatment + (1|site/plot), data = NEE)
summary(NEE.mod1)
r2(NEE.mod1) # Conditional R2: 0.272, Marginal R2: 0.030
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
r2(ER.mod1) # Conditional R2: 0.807, Marginal R2: 0.029

GEP.mod1 <- lmer(GEP_umol_s_m2 ~ treatment + (1|site/plot), data = GEP)
summary(GEP.mod1) # Estimate treatmentT = 0.008918, p = 0.0245*
r2(GEP.mod1) # Conditional R2: 0.482, Marginal R2: 0.035


# 3. Test for differences between sites too.-------------------------
NEE.mod2 <- lmer(NEE_umol_s_m2 ~ treatment + site + (1|site:plot), data = NEE)
difflsmeans(NEE.mod2)
r2(NEE.mod2) #Conditional R2: 0.237, Marginal R2: 0.226
summary(NEE.mod2)

GEP.mod2 <- lmer(GEP_umol_s_m2 ~ treatment + site + (1|site:plot), data = GEP) 
difflsmeans(GEP.mod2)
r2(GEP.mod2) #Conditional R2: 0.417, Marginal R2: 0.414
summary(GEP.mod2)

ER.mod2 <- lmer(ER_umol_s_m2 ~ treatment + site + (1|site:plot), data = ER)
difflsmeans(ER.mod2)
r2(ER.mod2) #  Conditional R2: 0.758, Marginal R2: 0.721
summary(ER.mod2)
 

# 4. Do treatment, GEI, SM, T_air, T_soil affect NEE? ?-------------------------
# I'm deciding to leave these out of the paper because ultimately, including 
# site in the full model means they can just be selected out again. Given that 
# site cannot be included in the random effects structure (too few levels and 
# singular) fit, there is essentially no difference. I will mention this 
# challenge in the data analysis part.
NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (treatment|site) + (1|site/plot), data = NEE)
# BOUNDARY IS SINGULAR
NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site:plot), data = NEE)
# air and soil are highly covaried: -0.578

NEE.mod3 <- lmer(NEE_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + (1|site:plot), data = NEE)
step(NEE.mod3, reduce.random = FALSE)
NEE.step3 <- lmer(NEE_umol_s_m2 ~ GEI + (1|site:plot), data = NEE)
r2(NEE.step3) # Nakagawa Conditional R2: 0.863, Marginal R2: 0.822
summary(NEE.step3)

GEP.mod3 <- lmer(GEP_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + (1|site:plot), data = GEP)
#GEP.mod3 <- lmer(GEP_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site/plot), data = GEP)
step(GEP.mod3, reduce.random = FALSE)

ER.mod3 <- lmer(ER_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + (1|site:plot), data = ER)
# ER.mod3 <- lmer(ER_umol_s_m2 ~ treatment + GEI + soil_moisture + T_air + T_soil + (1|site/plot), data = ER)
step(ER.mod3, reduce.random = FALSE)

# 5. Adding in site as a fixed effect.-----------------------------------------
# These are the full models which provide the bulk of my analysis.
NEE.mod4 <- lmer(NEE_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = NEE)
step(NEE.mod4)
NEE.step4 <- lmer(NEE_umol_s_m2 ~ GEI + (1 | site:plot), data = NEE)
r2(NEE.step4) # Conditional R2: 0.863, Marginal R2: 0.822

ER.mod4 <- lmer(ER_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = ER)
step(ER.mod4)
ER.mod4 <- lmer(ER_umol_s_m2 ~ treatment + site + GEI + (1 | site:plot), data = ER)
r2(ER.mod4) #  Conditional R2: 0.848, Marginal R2: 0.799

GEP.mod4 <- lmer(GEP_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = GEP)
step(GEP.mod4, reduce.random = FALSE)
GEP.step4 <- lmer(GEP_umol_s_m2 ~ site + GEI + (1 | site:plot), data = GEP)
r2(GEP.step4) #Conditional R2: 0.906, Marginal R2: 0.876



# "parameter estimate of the slope of the relationship"
