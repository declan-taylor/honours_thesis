# source(here("scripts/data_assembly/master_dataframe.R"))
library(tidyverse)
library(ggplot2)
library(viridis)

for(i in unique(asIRGA$doy)){
  oneDay <- filter(asIRGA, doy == i)
  
  doy <- as.character(i)
  site <- oneDay$site[1]
  # Plot IRGA-measured CO2 flux
  CO2plot <- ggplot(oneDay, aes(duration, CO2_ppm))+
    geom_point(aes(shape = plot,
                   colour = plot),
               alpha = 0.6)+
    geom_smooth(aes(colour = plot),
                method = "lm", se = FALSE) +
    # scale_y_continuous(limits = min(oneDay$CO2_ppm), max(oneDay$CO2_ppm))+
    scale_colour_viridis_d(guide = "none") +
    scale_shape_discrete(guide = "none") +
    facet_grid(treatment ~ light)+
    labs(title = paste(site, "_DOY", doy, sep = ""),
         x = NULL,
         y = NULL) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = NULL)
  
  # Assign the figure with a site/date-specific name to the global environment.
  assign(paste(site, "_DOY", doy, sep = ""), CO2plot, envir = .GlobalEnv)
}

# Assemble final exploratory figures
ggsave("lmFluxes.png", plot = 
         ((DRYAS_DOY183+ ylab("CO2 (ppm)")) /
            plot_spacer() /
            (DRYAS_DOY196+ ylab("CO2 (ppm)")) /
            (DRYAS_DOY207+ xlab("Duration (s)")+ ylab("CO2 (ppm)")))  |
         (MEAD_DOY182 / 
            plot_spacer()/ 
            MEAD_DOY195 / 
            (MEAD_DOY206 + xlab("Duration (s)")+ theme(legend.position = "bottom",
                                                       legend.title = NULL))) |
         (WILL_DOY179 / 
            (WILL_DOY192+ ylab("CO2 (ppm)")) / 
            WILL_DOY197 / 
            (WILL_DOY208 +xlab("Duration (s)"))),
       device = "png", path = here("figures"),
       width = 3500, height = 5000, units = "px")
