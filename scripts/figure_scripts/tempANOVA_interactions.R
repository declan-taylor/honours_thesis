library(tidyverse)
library(lubridate)
library(ggplot2)
library(here)
# Run to get ANOVA dataframes
source(here("scripts/analysis/envParamater_stats.R"))
# Run to get the HOBO data, `airtemp` and `soiltemp`.
source(here("scripts/figure_scripts/HOBO_figures.R"))

# Air Temperature ANOVA Interaction Plot------
airT_interaction <- ggplot()+
  # Add the IRGA information
  stat_summary(aes(x = treatment, 
                   y = T_air,
                   group = site, shape = site, colour = "#61346B"),
               size = 3,
               data = aovNEE,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = T_air,
                   linetype = site, group = site, colour = "#61346B"),
               data = aovNEE,
               fun = mean, 
               geom = "line") +
  # Add the HOBO information
  stat_summary(aes(x = treatment, 
                   y = air_daytimeT,
                   group = site, shape = site, colour = "#F2C75B"),
               size = 3,
               data = airT.day,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = air_daytimeT,
                   linetype = site, group = site, colour = "#F2C75B"),
               data = airT.day,
               fun = mean, 
               geom = "line") +
  # Other parameters
  scale_colour_discrete(labels = c("HOBO pendants", "IRGA temperature probes"),
                        type = getOption("ggplot2.discrete.colour"))+
  #scale_linetype_discrete(guide = "none")+
  #scale_shape_discrete(guide = "none")+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"),
                   expand=c(0.1, 0))+
  labs(x = "Treatment",
       y = "Air Temperature (ºC)",
       linetype = "Site",
       shape = "Site",
       colour = "Data Source")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Soil Temperature ANOVA Interaction Plot------
soilT_interaction <- ggplot()+
  # Add the IRGA information
  stat_summary(aes(x = treatment, 
                   y = T_soil,
                   group = site, shape = site, colour = "#61346B"),
               size = 3,
               data = aovNEE,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = T_soil,
                   linetype = site, group = site, colour = "#61346B"),
               data = aovNEE,
               fun = mean, 
               geom = "line") +
  # Add the HOBO information
  stat_summary(aes(x = treatment, 
                   y = soil_daytimeT,
                   shape = site, group = site, colour = "#F2C75B"),
               size = 3,
               data = soilT.day,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = soil_daytimeT,
                   linetype = site, group = site, colour = "#F2C75B"),
               data = soilT.day,
               fun = mean, 
               geom = "line") +
  # Other parameters
  scale_colour_discrete(labels = c("HOBO pendants", "IRGA temperature probes"),
                        type = getOption("ggplot2.discrete.colour"))+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"),
                   expand=c(0.1, 0))+
  labs(x = "Treatment",
       y = "Soil Temperature (ºC)",
       linetype = "Site",
       shape = "Site",
       colour = "Data Source")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Assemble figures------
# 1. ANOVA interactions
ggsave("tempANOVA_interactions.png",
       plot = airT_interaction + soilT_interaction + 
         patchwork::plot_layout(nrow = 1),
       device = "png", path = here("figures"),
       width = 3000, height = 1600, units = "px")

# 2. By variable: have air temp and air ANOVA together, same for soil.
ggsave("airTemp.png",
       plot = airtemp + airT_interaction + patchwork::plot_layout(),
       device = "png", path = here("figures"),
       width = 3000, height = 1400, units = "px")

ggsave("soilTemp.png",
       plot = soiltemp + soilT_interaction + patchwork::plot_layout(),
       device = "png", path = here("figures"),
       width = 3000, height = 1400, units = "px")
