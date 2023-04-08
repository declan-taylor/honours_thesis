#Run prior to this script:
#source(here("scripts/analysis/envParamater_stats.R"))

# Air Temperature Interaction Plot
airT_interaction <- ggplot()+
  # Add the IRGA information
  stat_summary(aes(x = treatment, 
                   y = T_air,
                   group = site, shape = site, colour = "purple"),
               size = 3,
               data = aovNEE,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = T_air,
                   linetype = site, group = site, colour = "purple"),
               data = aovNEE,
               fun = mean, 
               geom = "line") +
  # Add the HOBO information
  stat_summary(aes(x = treatment, 
                   y = air_daytimeT,
                   group = site, shape = site, colour = "orange"),
               size = 3,
               data = daytime_airT,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = air_daytimeT,
                   linetype = site, group = site, colour = "orange"),
               data = daytime_airT,
               fun = mean, 
               geom = "line") +
  # Other parameters
  scale_colour_discrete(labels = c("HOBO pendants", "IRGA temperature probes"),
                        guide = "none")+
  scale_linetype_discrete(guide = "none")+
  scale_shape_discrete(guide = "none")+
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

# Soil Temperature Interaction Plot
soilT_interaction <- ggplot()+
  # Add the IRGA information
  stat_summary(aes(x = treatment, 
                   y = T_soil,
                   group = site, shape = site, colour = "purple"),
               size = 3,
               data = aovNEE,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = T_soil,
                   linetype = site, group = site, colour = "purple"),
               data = aovNEE,
               fun = mean, 
               geom = "line") +
  # Add the HOBO information
  stat_summary(aes(x = treatment, 
                   y = soil_daytimeT,
                   shape = site, group = site, colour = "orange"),
               size = 3,
               data = daytime_soilT,
               fun = mean, 
               geom = "point") +
  stat_summary(aes(x = treatment, 
                   y = soil_daytimeT,
                   linetype = site, group = site, colour = "orange"),
               data = daytime_soilT,
               fun = mean, 
               geom = "line") +
  # Other parameters
  scale_colour_discrete(labels = c("HOBO pendants", "IRGA temperature probes"))+
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

ggsave("tempANOVA_interactions.png",
       plot = airT_interaction + soilT_interaction + 
         patchwork::plot_layout(nrow = 1),
       device = "png", path = here("figures"),
       width = 3000, height = 1600, units = "px")
