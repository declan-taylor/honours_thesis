# To explore my initial IRGA data with Greg and look for trends.
# Grid: facet by light and treatment, colour code by plot. One page per site per day

library(tidyverse)
library(ggplot2)
library(viridis)

# Create a dataframe for each combination of site and day by filtering for DOYs.
# Add
for(i in unique(asIRGA$DOY)){
  doy <- as.character(i)
  site <- asIRGA$site[1]
  oneDay <- filter(asIRGA, DOY == i) %>%
    filter(!is.na(light) | !is.na(treatment)) %>%
    # By grouping along variables that make each IRGA unique and adding a row 
    # number, we are essentially adding a 1Hz duration counter.
    group_by(plot, treatment, light) %>%
    mutate(duration = row_number())
  
  # Plot IRGA-measured CO2 flux
  CO2plot <- ggplot(oneDay, aes(duration, CO2_ppm))+
    geom_point(aes(shape = factor(plot),
                   colour = factor(plot)),
               alpha = 0.6)+
    geom_smooth(aes(colour = factor(plot)),
                method = "lm", se = FALSE) +
    scale_y_continuous(limits = c(370,  440))+
    scale_colour_viridis_d() +
    facet_grid(treatment ~ light)+
    labs(title = paste(site, "_DOY", doy, sep = "")) +
    xlab("Duration (s)") +
    ylab("CO2 (ppm)")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Assign the figure with a site/date-specific name to the global environment.
  assign(paste(site, "_DOY", doy, sep = ""), CO2plot, envir = .GlobalEnv)
}
