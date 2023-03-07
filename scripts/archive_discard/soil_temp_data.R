# A script to clean soil temperature data from the HOBO loggers. Based closely
# on what I did to clean and organize the soil moisture data. This data however,
# is not chronologically ordered by DOI, but rather the appending of each
# subsequent CSV orders the dataframe by plot.

library(tidyverse)
library(tundra)
library(here)

# List the files we will run this script over.
stFiles <- list.files(path = here("data/soil_temperature"),
                      pattern = ".csv")

# A useful little function for data processing down below.
not_all_na <- function(x)any(!is.na(x))

# A blank tibble for all the soil moisture data to be pasted into.
soil_temp <- tibble(site = character(),
                    plot = numeric(),
                    treatment = character(),
                    year = numeric(),
                    DOY = numeric(),
                    soil_moisture = numeric())

# Extract some basic information from the filename.
year <- 2022
site <- str_extract(i, "^[A-z]+(?=_)")
plot <- as.numeric(str_extract(i, "[0-9]{2}"))
treatment <- str_extract(i, "(?<=[0-9]{2})[A-z](?=_)")

# Process the CSV exported from HOBOware. NOTE TO SELF: GO LOOK AT OLD HOBO WORK!
stData <- load_file(paste0("data/soil_temperature/", i))



#SOME POTENTIALLY USEFUL FUNCTIONS:
#date_extractor()
#day_identifier()
#hour_identifier()
