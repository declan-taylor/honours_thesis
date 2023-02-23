# This script averages the three-per-plot readings for soil moisture and 
# creates a long-data single sheet from all of the individual datasheets.

# Manual edits: DOY208 data was incorrectly put under the "DRYAS" tab when SM
# was done at WILL as part of NEE measurements. The DOY208 CSV was thus renamed,
# and this script was re-run.

library(tidyverse)
library(here)

# List the files we will run this script over.
smFiles <- list.files(path = here("data/soil_moisture/extracted_CSVs"),
                      pattern = ".csv")

# A useful little function for data processing down below.
not_all_na <- function(x)any(!is.na(x))

# A blank tibble for all the soil moisture data to be pasted into.
soil_moisture <- tibble(site = character(),
                        plot = numeric(),
                        treatment = character(),
                        year = numeric(),
                        doy = numeric(),
                        soil_moisture = numeric())

# for loop to organize the soil moisture data and extract relevant date/plot
# information from the filename.

for(i in smFiles){
  # Extract some basic information from the filename.
  year <- 2022
  doy <- as.numeric(str_extract(i, "(?<=2022_)[0-9]{3}(?=_SM)"))
  site <- str_extract(i, "(?<=SM_)[A-Z]{3,5}(?=\\.csv)")
  
  # Load the data and average/reformat it.
  smData <- read_csv(paste0("data/soil_moisture/extracted_CSVs/", i)) %>%
    # Drop unused columns
    select(where(not_all_na)) %>%
    # Make the north centre south columns consistently named.
    rename_with(~ str_replace_all(.x, "North.*$", "north")) %>%
    rename_with(~ str_replace_all(.x, "Center.*$", "centre")) %>%
    rename_with(~ str_replace_all(.x, "South.*$", "south")) %>%
    rename(plot = "Plot") %>%
    # Separate the "CO2" from the treatment column value in CO2 plots
    separate("OTC/Cntr", 
             c("CO2", "treatment"),
             sep = "_",
             remove = TRUE, convert = TRUE,
             fill = "left") %>%
    # Add in data gathered from the filename.
    mutate(year = year,
           site = site,
           doy = doy,
           # Average the three soil moisture readings
           soil_moisture = (as.numeric(north) + as.numeric(centre) + as.numeric(south))/3,
           .before = "CO2") %>%
    # Drop irrelevant columns.
    select(c("site", "plot", "treatment", "year", "doy", "soil_moisture"))
  
  # Append the reformated soil moisture data to the (originally blank) tibble
  # created before this for loop is run. ENSURE TIBBLE IS BLANK BEFORE RUNNING
  # THIS LOOP!
  soil_moisture <<- rbind(soil_moisture, smData)
} 


# Remove the unnecessary objects from global environment.
rm(smFiles, smData, not_all_na, doy, i , site, year)

# Optional: write the soil moisture data to the data directory.
# write_csv(soil_moisture, file = "data/soil_moisture/2022_SM_ALLSITES.csv")
