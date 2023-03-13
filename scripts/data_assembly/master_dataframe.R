# This script compiles the master dataframe used for analysis. 
# Step 1: combine all the assembled IRGA flux files into one master dataframe 
# (organized by DOY and filter-able for SITE).
# Step 2: add other environmental variables as additional rows. Those variables
# are combined in their respective spreadsheets.
library(conflicted)
library(rgdal)
library(lubridate)
library(tidyverse)
library(here)

# Set a preference for dplyr functions.
conflicts_prefer(dplyr::filter(),
                 dplyr::select(),
                 dplyr::mutate())

# STEP ONE: Load IRGA data--------------------
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
         plot = as.factor(plot),
         doy = DOY) %>%
  # Remove rows that are not during any measurements.
  filter(!is.na(light) | !is.na(treatment)) %>%
  # By grouping along variables that make each IRGA unique and adding a row 
  # number, we are essentially adding a 1Hz duration counter.
  group_by(site, plot, treatment, light, doy) %>%
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
  select(c("site", "plot", "treatment", "light", "year", "doy", "datetime", "time", "duration", "battery_V", "CO2_ppm", "H2O_ppt", "T_panel", "T_soil", "T_air"))

# STEP TWO: add in SM and greenness data--------------------
# Soil moisture data
source(here("scripts/data_assembly/soil_moisture_data.R")) # cleans and loads the data

soil_moisture <- soil_moisture %>%
  filter(plot > 10) %>% # Removing the plots not used in CO2 measurements
  mutate(plot = as.factor(plot)) %>%
  # We are missing soil moisture data from DRYAS DOY183; DOY182 is substituted.
  # DRYAS DOY182 data is not used elsewhere so the DOY is directly altered in 
  # the dataframe.
  mutate(doy = case_when(doy == 182 & site == "DRYAS" ~ 183,
                         TRUE ~ doy))

asIRGA <- left_join(asIRGA, soil_moisture,
                    by = c("doy", "site", "plot", "treatment", "year"),
                    keep = FALSE)

# Greenness data
source(here("scripts/data_assembly/GEI.R"))
generate_GEI(here("data/greenness/cropped_images"))

asIRGA <- left_join(asIRGA, GEI,
                    by = c("doy", "site", "plot", "treatment"),
                    keep = FALSE)

# STEP THREE: add in TEMPERATURE data--------------------
## DOY 179, 182, 183 all are NAs from manual data entry. 192 and 195 are bad 
## data. WILLDOY208 12T (dark) is an outlier. MEADDOY195 is missing.
source(here("scripts/data_assembly/temp_data.R"))
addT("air", "daytime") # Creates a dataframe called air_temp
addT("soil", "daytime") # Creates a dataframe called soil_temp

fullTemp <- as_tibble(full_join(soil_temp, air_temp,
                            by = c("doy", "site", "plot", "treatment"),
                            keep = FALSE))

asIRGA <- left_join(asIRGA, fullTemp,
                    by = c("doy", "site", "plot", "treatment"),
                    keep = FALSE)

# the case_when() function selectively overwrites T_soil with the HOBO 
# temperature averages in situations where the temperature probes are not 
# working properly/at all.
asIRGA <- asIRGA %>%
  mutate(T_soil = case_when(doy == 192 | doy == 195 ~ soil_daytimeT,
                            is.na(T_soil) == TRUE ~ soil_daytimeT,
                            site == "WILL" & doy == 208 & plot == 12 & treatment == "T" & light == "dark" ~ soil_daytimeT,
                            TRUE ~ T_soil),
         # Repeate the process identically for air temperature.
         T_air = case_when(doy == 192 | doy == 195 ~ air_daytimeT,
                            is.na(T_air) == TRUE ~ air_daytimeT,
                            site == "WILL" & doy == 208 & plot == 12 & treatment == "T" & light == "dark" ~ air_daytimeT,
                            TRUE ~ T_air)) %>%
  # Filling the missing meadow data with no HOBO or IRGA: averaging hobo from 
  # the surrounding sites on the same day.
  mutate(DOY195MEAD = mean(soil_temp %>% 
                             filter(site == "MEAD" & doy == 195 & treatment == "C") %>%
                             pull(soil_daytimeT)),
         T_soil = case_when(site == "MEAD" & doy == 195 & plot == 11 & treatment == "C" ~ mean(soil_temp %>% 
                                                                                                 filter(site == "MEAD" & doy == 195 & treatment == "C") %>%
                                                                                                 pull(soil_daytimeT)),
                            TRUE ~ T_soil)) %>%
  select(-c(soil_daytimeT, air_daytimeT, DOY195MEAD))

rm(air_temp, soil_temp, fullTemp, soil_moisture, GEI) # keeping the global environment clean.

# STEP FOUR: calculate NEE--------------------
source(here("scripts/data_assembly/flux_conversion.R"))
fluxData <- asIRGA %>%
  # asIRGA still grouped by site, plot, treatment, light, DOY from STEP ONE.
  summarise(flux_ppm_s = lm(CO2_ppm ~ time)$coefficients['time'],
            flux_sd = sd(CO2_ppm),
            T_air = mean(T_air),
            T_soil = mean(T_soil),
            H2O_ppt = mean(H2O_ppt),
            soil_moisture = mean(soil_moisture),
            GEI = mean (GEI)) %>%
  ungroup() %>%
  mutate(flux_umol_s_m2 = fluxConvert(flux_ppm_s, T_air),
         treatment = as.factor(treatment))

# Separate the light readings
NEE <- fluxData %>%
  filter(light == "light") %>%
  rename(NEE_umol_s_m2 = flux_umol_s_m2) %>%
  select (site, plot, treatment, doy, T_air, T_soil, soil_moisture, GEI, NEE_umol_s_m2)

ER <- fluxData %>%
  filter(light == "dark") %>%
  rename(ER_umol_s_m2 = flux_umol_s_m2) %>%
  select(site, plot, treatment, doy, ER_umol_s_m2, T_air, T_soil, soil_moisture, GEI)

# We missed DRYAS 13C dark DOY 207 when the wires broke in a cloud of mosquitos.
# Also removed from NEE here so that NEE and ER are the same size.
GEP <- NEE %>%
  filter(!(site == "DRYAS" & plot == 13 & treatment == "C" & doy == 207)) %>%
  left_join(select(ER, -c("T_air", "T_soil")), 
            by = c("site", "plot", "treatment", "doy", "soil_moisture"),
                 keep = FALSE) %>%
  # NEE = GEP + ER (where ER is measured as a negative flux)
  mutate(GEP_umol_s_m2 = NEE_umol_s_m2 - ER_umol_s_m2)

