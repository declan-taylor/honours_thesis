# This script compiles the master dataframe used for analysis. 
# Step 1: combine all the assembled IRGA flux files into one master dataframe 
# (organized by DOY and filter-able for SITE).
# Step 2: add other environmental variables as additional rows. Those variables
# are combined in their respective spreadsheets.

library(tidyverse)
library(here)

# STEP ONE--------------------
asIRGA <- list.files(here("data/IRGA_flux/assembled_files"), full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows %>%
  # Assign a "period" value dictating whether the data should be assigned into
  # one of the three periods of observation that I will compare.
  mutate(period = ifelse(DOY %in% 179:183, 1,
                  ifelse(DOY %in% 195:197, 2,
                  ifelse(DOY %in% 206:208, 3,
                         NA))),
         # The date and time data must be dealt with. I use lubridate functions to
         # produce a universal POSIX date/time stamp which I will match with other
         # databases to align data.
         datetime = as.POSIXct(paste(as.Date(DOY, origin = "2021-12-31"), hhmm, ss.s), 
                               format = "%Y-%m-%d %H%M %S", 
                               tz = "CST6CDT", origin = "1970-01-01"),
         time = hms::as_hms(datetime),
         plot = as.factor(plot)) %>%
  # Remove rows that are not during any measurements.
  filter(!is.na(light) | !is.na(treatment)) %>%
  # By grouping along variables that make each IRGA unique and adding a row 
  # number, we are essentially adding a 1Hz duration counter.
  group_by(site, plot, treatment, light, DOY) %>%
  mutate(duration = row_number()) %>%
  # The input files for the asIRGA dataframe contain 140 seconds of IRGA flux,
  # for visualization and trend exploration. For NEE calculations we use only
  # 120 seconds, which means we trim the first 5 and last 15 as these are the 
  # ones most frequently disturbed by placing/removing the chamber. This filter 
  # also removes all the data that does not contain manually added plot info.
  filter(duration >= 6 & duration <= 125) %>%
  # Discarding extreme outliers (based on manual exploration of data).
  filter(CO2_ppm > 350 & CO2_ppm < 500) %>%
  # Reorder columns.
  select(c("site", "plot", "treatment", "light", "year", "DOY", "datetime", "time", "duration", "battery_V", "CO2_ppm", "H2O_ppt", "T_panel", "T_soil", "T_air"))

# STEP THREE: calculate NEE--------------------
NEE <- asIRGA %>%
  summarise(flux_ppm_s = lm(CO2_ppm ~ time)$coefficients['time']) %>%
  ungroup()

# STEP THREE: add in other data--------------------
# Soil moisture data
SM <- soil_moisture %>%
  filter(plot > 10) %>%
  mutate(plot = as.factor(plot))

left_join(NEE, SM, 
          by = c("DOY", "site", "plot", "treatment"),
          keep = FALSE)
# Soil temperature data

