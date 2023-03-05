# Secondary function: the main function requires knowledge of averaging 
# temperatures based on

# MAIN FUNCTION
# This function generates a single dataframe in the global environment for all
# of the soil or temperature data, which can then be bound to the master
# dataframe. The function gets called in the master_dataframe.R script.

addT <- function(dataType){
  data <- here(paste0("data/", dataType, "_temperature"))
  file_list <- list.files(path = data,
                          pattern = "csv",
                          recursive = FALSE)
  
  # NULL dataframe for all temp data
  assign(paste0(dataType, "_temp"), NULL,
         env = .GlobalEnv)
  
  # NULL dataframe for daily average temp
  assign(paste0(dataType, "_dailyAvg_T"), NULL,
         env = .GlobalEnv)
  
  for(i in file_list){
    # Generate a tibble with the appropriate temperature data.
    temp_i <-  tundra::load_file(paste0(data, "/", i), sep = ",") %>%
      # Rename the date_time and temp columns by extracting their complete but
      # inconsistent names using the catch-all regular expressions/
      rename_all(~gsub("date.*", "datetime", .x)) %>%
      rename_all(~gsub("temp.*", "temp_C", .x)) %>%
      mutate(temp_C = as.numeric(temp_C)) %>%
      # Add columns that contain site, treatment, and plot number info on each row.
      add_column("site" = str_extract(i, "[A-z]{3,5}(?=\\_)"),
                 "plot" = as.factor(str_extract(i, "[0-9]{1,2}(?=[A-z])")),
                 "treatment" = str_extract(i, "(?<=[0-9]{1,2})[A-z]"), .name_repair = unique) %>%
      # Correct time data and generate a DOY column
      mutate(datetime = mdy_hms(datetime,
                                tz = "Canada/Central"),
             doy = yday(datetime),
             hour = hour(datetime)) %>%
      # Name temperature based on dataType and select appropriate columns
      select(site, plot, treatment, datetime, doy, hour, temp_C)
    
    # A daily average temperature will be used to plot and explore seasonal 
    # temperature change between plots, sites, etc.
    assign(paste0(dataType, "_dailyAvg_T"),
           rbind(get(paste0(dataType, "_dailyAvg_T"),
                     env = .GlobalEnv),
                 # Summarizing the temperature data.
                 temp_i %>%
                   group_by(site, plot, treatment, doy) %>%
                   summarise(dailyAvg_T = mean(temp_C)) %>%
                   # Adding datatype to column name
                   rename_all(~gsub("dailyAvg_T", paste0(dataType, "_dailyAvg_T"), .x)) %>%
                   ungroup()),
           env = .GlobalEnv)
    
    # A *daytime* average temperature will be used to fill in missing IRGA temperature probe data. 
    # Append the next CSV's data to the big dataframe and assign it to the global environment.
    
    
    # Generate a daytime average to sub in for missing T probe data.
    assign(paste0(dataType, "_temp"),
           rbind(get(paste0(dataType, "_temp"),
                     env = .GlobalEnv), 
                 # Summarise the temperature data only during daytime hours.
                 temp_i %>%
                   group_by(site, plot, treatment, doy) %>%
                   filter(hour >= 10 & hour <= 16) %>%
                   summarise(daytimeT = mean(temp_C)) %>%
                   rename_all(~gsub("daytimeT", paste0(dataType, "_daytimeT"), .x)) %>%
                   ungroup()),
           env = .GlobalEnv)
  }
}
