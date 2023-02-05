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
         time = hms::as_hms(datetime)) %>%
  select(c("site", "plot", "treatment", "light", "year", "DOY", "datetime", "time", "battery_V", "CO2_ppm", "H2O_ppt", "T_panel", "T_soil", "T_air"))

# STEP TWO--------------------
