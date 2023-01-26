library(lubridate)
library(tidyverse)
library(tundra)
library(here)

Prep_CR21_File <- function(file){
  IRGA <- read_delim(paste(here("data/IRGA_flux/data_logger_files/"), file, sep = ""), 
                     delim = ",",
                     col_names = FALSE) %>%
    mutate(row_number = row_number())
  
  IRGA_even <- IRGA %>%
    filter(FALSE == FSA::is.odd(IRGA$row_number)) %>%
    mutate(row_number = as.numeric(row_number) - 1)
  
  IRGA <- full_join(IRGA, IRGA_even, by = "row_number") %>%
    filter(TRUE == FSA::is.odd(IRGA$row_number)) %>%
    select(X1.x, X1.y)
  
  IRGA[c("blank",
         "X1",
         "year",
         "DOY",
         "hhmm",
         "ss.s",
         "battery_V",
         "CO2_v",
         "CO2_ppm")] <- str_split(IRGA$X1.x,
                                  "[0-9]{2}(?=[\\+,\\-])",
                                  9,
                                  simplify = TRUE)
  
  IRGA[c("blank",
         "H2O_v",
         "H2O_ppt",
         "T_panel",
         "T_soil",
         "T_air",
         "X14",
         "X15")] <- str_split(IRGA$X1.y,
                              "[0-9]{2}(?=[\\+,\\-])",
                              8,
                              simplify = TRUE)
  
  IRGA <<- IRGA %>%
    add_column("site" = NA,
               "plot" = NA,
               "treatment" = NA,
               "light" = NA) %>%
    select(c(site:light,
             X1:X15))
}

for(i in list.files(path = here("data/IRGA_flux/data_logger_files"), 
                    pattern = "\\.DAT")){
  Prep_CR21_File(i)
  
  write_csv(IRGA, paste(here("data/IRGA_flux/converted_files/"), 
                        tools::file_path_sans_ext(i), 
                        ".csv", sep = ""))
}
