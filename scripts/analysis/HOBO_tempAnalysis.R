# Generating box plots to determine difference in temperature between sites.
# Run `temp_data.R` before using this script.

library(tidyverse)
library(lubridate)
library(ggplot2)
library(here)

# Stats packages:
library(fitdistrplus)
library(lme4)

# Output from `master_dataframe.R`
addT("air", "all") #creates air_dailyAvg_T
addT("soil", "all") #creates soil_dailyAvg_T
fullTemp <- as_tibble(full_join(air_dailyAvg_T, soil_dailyAvg_T,
                                by = c("doy", "site", "plot", "treatment"),
                                keep = FALSE))

rm(soil_dailyAvg_T, air_dailyAvg_T, soil_temp, air_temp)

bySite <- fullTemp %>%
  group_by(site, treatment, doy) %>%
  summarise(air_temp = mean(air_dailyAvg_T, na.rm = TRUE),
            soil_temp = mean(soil_dailyAvg_T, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na()

bySeason <- bySite %>%
  group_by(site, treatment) %>%
  summarise(soil_temp = mean(soil_temp, na.rm = TRUE),
            air_temp = mean(air_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na()

# TEMPERATURE STATS-----------------
## Check if data is parametric
#shapiro.test(air_bySite$air_temp) # p = 0.001343: not normal.
#shapiro.test(soil_bySite$soil_temp) # p = 1.564e-0.5: not normal.

# Homoscedasticity: checking if variances are approximately equal between sites.
#bartlett.test(air_temp ~ site, data = air_bySite) #p = 0.2472: variances equal

#aov <- aov(air_temp ~ site, data = air_bySite)
#summary(aov)
#TukeyHSD(aov)
#plot(TukeyHSD(aov))

descdist(air_bySite$air_temp) # Mayhaps beta or uniform distr?

lm(air_temp ~ site + treatment, data = air_bySite)