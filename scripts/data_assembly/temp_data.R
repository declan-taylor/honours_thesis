# This function generates a single dataframe in the global environment for all
# of the soil or temperature data, which can then be bound to the master
# dataframe. The function gets called in the master_dataframe.R script.

addT <- function(dataType){
  data <- here(paste0("data/", dataType, "_temperature"))
  file_list <- list.files(path = data,
                          pattern = "csv",
                          recursive = FALSE)
  
  assign(paste0(dataType, "_temp"), NULL,
         env = .GlobalEnv)
  
  for(i in file_list){
    # Generate a tibble with the appropriate temperature data.
    temp_i <-  tundra::load_file(paste0(data, "/", i), sep = ",") %>%
      # Rename the date_time and temp columns by extracting their complete but
      # inconsistent names using the catch-all regular expressions/
      rename_all(~gsub("date.*", "datetime", .x)) %>%
      rename_all(~gsub("temp.*", paste0(dataType, "_temp_C"), .x)) %>%
      # Add columns that contain site, treatment, and plot number info on each row.
      add_column("site" = str_extract(i, "[A-z]{3,5}(?=\\_)"),
                 "treatment" = str_extract(i, "[0-9]{1,2}(?=[A-z])"),
                 "plot" = str_extract(i, "(?<=[0-9]{1,2})[A-z]"), .name_repair = unique) %>%
      # Correct time data and generate a DOY column
      mutate(datetime = mdy_hms(datetime,
                                tz = "Canada/Central"),
             doy = yday(datetime)) %>%
      select(site, plot, treatment, datetime, doy, paste0(dataType, "_temp_C"))
    
    # Append the next CSV's data to the big dataframe and assign it to the global environment.
    assign(paste0(dataType, "_temp"),
           rbind(get(paste0(dataType, "_temp"),
                     env = .GlobalEnv), temp_i),
           env = .GlobalEnv)
  }
}
