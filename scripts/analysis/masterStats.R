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
                 performance::r2(),
                 dplyr::filter())

# Run `master_dataframe.R` first.
# source(here("scripts/data_assembly/master_dataframe.R"))

# Checking the data ditribution-------------------------
hist(NEE$NEE_umol_s_m2)
hist(ER$ER_umol_s_m2)
hist(GEP$GEP_umol_s_m2)

summary(NEE$NEE_umol_s_m2)
summary(ER$ER_umol_s_m2)
summary(GEP$GEP_umol_s_m2)

## Testing if parametric
shapiro.test(NEE$NEE_umol_s_m2) # p-value = 0.02408
hist(NEE$NEE_umol_s_m2, breaks = 13) # quite normal looking
qqnorm(NEE$NEE_umol_s_m2)

shapiro.test(ER$ER_umol_s_m2) # p-value = 0.003179
hist(ER$ER_umol_s_m2, breaks = 13) # fairly left skewed (most values closer to zero)
qqnorm(ER$ER_umol_s_m2) # maybe a bit logistic
# Log-transform ER
#ER <- ER %>%
#  mutate(logER = log(ER_umol_s_m2 + 1))

shapiro.test(GEP$GEP_umol_s_m2) # p-value = 5.691e-05
hist(GEP$GEP_umol_s_m2) # fairly left skewed (most values closer to zero
qqnorm(GEP$GEP_umol_s_m2) # maybe a bit logistic

#GEP <- GEP %>%
#  mutate(logGEP = log(GEP_umol_s_m2))
#shapiro.test(GEP$logGEP) # p-value = 0.006108
#hist(GEP$logGEP) # still quite left skewed (most values closer to zero)
#qqnorm(GEP$logGEP)

# How to fit non-normal distributions?

## Testing homoscedasticity
bartlett.test(NEE_umol_s_m2 ~ site, data = NEE) #p = 0.2472
bartlett.test(NEE_umol_s_m2 ~ treatment, data = NEE) #p = 0.0859

bartlett.test(ER_umol_s_m2 ~ site, data = ER) #p = 0.006026
bartlett.test(ER_umol_s_m2 ~ treatment, data = ER) #p = 0.6964

# Does long term warming affect NEE?-------------------------
## If we proceed to model selection, REML should likely be set to FALSE. Otherwise, true.
# lme4: lmer(formula, data = <>. REML = ??), glmer(), nlmer().

# Avg
mean.C <- mean(pull(filter(ER, treatment == "C")["ER_umol_s_m2"]))
mean.T <- mean(pull(filter(ER, treatment == "T")["ER_umol_s_m2"])) 

# Percent difference
1-mean.C/mean.T
1-mean.site1/mean.site2

# Table of raw flux values
flux_means <- matrix(c(
  "NEE.C.DRYAS", mean(pull(filter(NEE, treatment == "C", site == "DRYAS")["NEE_umol_s_m2"])),
  "NEE.C.MEAD", mean(pull(filter(NEE, treatment == "C", site == "MEAD")["NEE_umol_s_m2"])),
  "NEE.C.WILL", mean(pull(filter(NEE, treatment == "C", site == "WILL")["NEE_umol_s_m2"])),
  "NEE.T.DRYAS", mean(pull(filter(NEE, treatment == "T", site == "DRYAS")["NEE_umol_s_m2"])),
  "NEE.T.MEAD", mean(pull(filter(NEE, treatment == "T", site == "MEAD")["NEE_umol_s_m2"])),
  "NEE.T.WILL", mean(pull(filter(NEE, treatment == "T", site == "WILL")["NEE_umol_s_m2"])),
  
  "GEP.C.DRYAS", mean(pull(filter(GEP, treatment == "C", site == "DRYAS")["GEP_umol_s_m2"])),
  "GEP.C.MEAD", mean(pull(filter(GEP, treatment == "C", site == "MEAD")["GEP_umol_s_m2"])),
  "GEP.C.WILL", mean(pull(filter(GEP, treatment == "C", site == "WILL")["GEP_umol_s_m2"])),
  "GEP.T.DRYAS", mean(pull(filter(GEP, treatment == "T", site == "DRYAS")["GEP_umol_s_m2"])),
  "GEP.T.MEAD", mean(pull(filter(GEP, treatment == "T", site == "MEAD")["GEP_umol_s_m2"])),
  "GEP.T.WILL", mean(pull(filter(GEP, treatment == "T", site == "WILL")["GEP_umol_s_m2"])),
  
  "ER.C.DRYAS", mean(pull(filter(ER, treatment == "C", site == "DRYAS")["ER_umol_s_m2"])),
  "ER.C.MEAD", mean(pull(filter(ER, treatment == "C", site == "MEAD")["ER_umol_s_m2"])),
  "ER.C.WILL", mean(pull(filter(ER, treatment == "C", site == "WILL")["ER_umol_s_m2"])),
  "ER.T.DRYAS", mean(pull(filter(ER, treatment == "T", site == "DRYAS")["ER_umol_s_m2"])),
  "ER.T.MEAD", mean(pull(filter(ER, treatment == "T", site == "MEAD")["ER_umol_s_m2"])),
  "ER.T.WILL", mean(pull(filter(ER, treatment == "T", site == "WILL")["ER_umol_s_m2"]))),
  ncol = 2, byrow = TRUE)

colnames(flux_means) <- c("info", "flux.mean")

flux_means <- 
  tibble(as.data.frame(flux_means)) %>%
  separate(info, into = c("flux", "treatment", "site"), sep = "\\.") %>%
  mutate(flux.mean = as.numeric(flux.mean)) 

stdev <- GEP %>%
  group_by(treatment) %>%
  summarise(sd_NEE = sd(NEE_umol_s_m2, na.rm=TRUE),
            sd_GEP = sd(GEP_umol_s_m2, na.rm=TRUE),
            sd_ER = sd(ER_umol_s_m2, na.rm=TRUE))
  
stdev_site <- GEP %>%
    group_by(treatment, site) %>%
  summarise(sd_NEE = sd(NEE_umol_s_m2, na.rm=TRUE),
            sd_GEP = sd(GEP_umol_s_m2, na.rm=TRUE),
            sd_ER = sd(ER_umol_s_m2, na.rm=TRUE))
Difference in warming response across sites?-----------
SITE <- "MEAD"
mean.C <- mean(pull(filter(NEE, treatment == "C")["NEE_umol_s_m2"]))
mean.T <- mean(pull(filter(NEE, treatment == "T")["NEE_umol_s_m2"]))

mean.T-mean.C
(1-mean.C/mean.T)*100
# NEE warming response: 
## all = +0.005857338 or 30.1%
## DRYAS = +0.003238455 or 22.24%
## WILL = +0.01047813 or 33.37%
## MEAD = +0.002315167 or 23.85%

# GEP warming response:
## all = +0.008781643 or 24.49%
## DRYAS = +0.00675672 or 22.98%
## WILL = +0.01272733 or 23.68%
## MEAD = +0.005915545 or 32.00%

# ER warming response:
## all = -0.002882499 or 17.59%
## DRYAS = -0.003161213 or 21.30%
## WILL = -0.0022492 or 9.673%
## MEAD = -0.003600377 or 41.01%

# BASIC MODEL (1): FLUX AND TREATMENT----------------
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


# 2. Test for differences between sites too.-------------------------
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
 

# 3. Do treatment, GEI, SM, T_air, T_soil affect NEE? ?-------------------------
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

# 4. Adding in site as a fixed effect.-----------------------------------------
# These are the full models which provide the bulk of my analysis.
NEE.mod4 <- lmer(NEE_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = NEE)
summary(NEE.mod4)
plot(NEE.mod4)
step(NEE.mod4)
NEE.step4 <- lmer(NEE_umol_s_m2 ~ GEI + (1 | site:plot), data = NEE)
summary(NEE.step4)
r2(NEE.step4) # Conditional R2: 0.863, Marginal R2: 0.822

ER.mod4 <- lmer(ER_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = ER)
summary(ER.mod4)
plot(ER.mod4)
step(ER.mod4)
ER.step4 <- lmer(ER_umol_s_m2 ~ treatment + site + GEI + (1 | site:plot), data = ER)
summary(ER.step4)
r2(ER.mod4) #  Conditional R2: 0.846, Marginal R2: 0.797

GEP.mod4 <- lmer(GEP_umol_s_m2 ~ treatment + site + GEI + soil_moisture + T_air + (1|site:plot), data = GEP)
summary(GEP.mod4)
plot(GEP.mod4)
plot(resid(GEP.mod4, type = "pearson") ~ fitted(GEP.mod4))
qqnorm(resid(GEP.mod4, type = "pearson"))
qqline(resid(GEP.mod4, type = "pearson"))

step(GEP.mod4, reduce.random = FALSE)
GEP.step4 <- lmer(GEP_umol_s_m2 ~ site + GEI + (1 | site:plot), data = GEP)
summary(GEP.step4)
r2(GEP.step4) #Conditional R2: 0.906, Marginal R2: 0.876



# "parameter estimate of the slope of the relationship"
