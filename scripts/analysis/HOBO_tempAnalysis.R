# Generating box plots to determine difference in temperature between sites.
# Run `temp_data.R` before using this script.

library(tidyverse)
library(ggplot2)
library(here)

# Output from `master_dataframe.R`
addT("air", "all") #creates air_dailyAvg_T
addT("soil", "all") #creates soil_dailyAvg_T

