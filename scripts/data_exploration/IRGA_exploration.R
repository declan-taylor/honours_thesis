# To explore my initial IRGA data with Greg and look for trends.
# Run the master_dataframe.R script to create appropriate dfs accessed by these
# ggplot scripts.

library(tidyverse)
library(ggplot2)
library(viridis)

# PART 1: flux data--------------------
# Create a dataframe for each combination of site and day by filtering for DOYs.
for(i in unique(asIRGA$DOY)){
  oneDay <- filter(asIRGA, DOY == i)
 
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

# Assemble final exploratory figures
WILL <- WILL_DOY179 + WILL_DOY192 + WILL_DOY197 + WILL_DOY208 + patchwork::plot_layout()
MEAD <- MEAD_DOY182 + MEAD_DOY195 + MEAD_DOY206 + patchwork::plot_layout()
DRYAS <- DRYAS_DOY183 + DRYAS_DOY196 + DRYAS_DOY207 + patchwork::plot_layout()

# Save PNGs
ggsave("WILL_flux.png", plot = WILL, device = "png", path = here("figures/exploratory"),
       width = 3800, height = 2800, units = "px")
ggsave("MEAD_flux.png", plot = MEAD, device = "png", path = here("figures/exploratory"),
       width = 4400, height = 1800, units = "px")
ggsave("DRYAS_flux.png", plot = DRYAS, device = "png", path = here("figures/exploratory"),
       width = 4400, height = 1800, units = "px")

# PART 2: NEE Data--------------------
# Some quick plots of flux slopes against DOY
for(i in unique(NEE$site)){
  oneSite <- NEE %>% filter(site == i)
  
  ggplot(oneSite, aes(DOY, flux_ppm_s))+
    geom_line(aes(colour = factor(plot)))+
    geom_point(aes(colour = factor(plot),
                   shape = factor(plot)))
}