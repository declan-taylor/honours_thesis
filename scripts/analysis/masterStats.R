library(tidyverse)
library(lubridate)
library(lme4)

# Run `master_dataframe.R` first.
# 1. Checking the data ditribution.
hist(NEE$NEE_umol_s_m2)
hist(ER$ER_umol_s_m2)
hist(GEP$GEP_umol_s_m2)

