library(tidyverse)
library(lubridate)
library(ggplot2)
library(here)

# Generating box plots to determine difference in temperature between sites.
source(here("scripts/data_assembly/temp_data.R"))
addT("air", "all") #creates air_dailyAvg_T
addT("soil", "all") #creates soil_dailyAvg_T
fullTemp <- as_tibble(full_join(air_dailyAvg_T, soil_dailyAvg_T,
                                by = c("doy", "site", "plot", "treatment"),
                                keep = FALSE))

# The boxplots
soiltemp <- ggplot(data = fullTemp, aes(x = site,
                                        y = soil_dailyAvg_T,
                                        fill = treatment))+
  geom_boxplot()+
  geom_point(aes(colour = treatment),
             position=position_jitterdodge(jitter.width = 0.3),
             size = 0.7, alpha = 0.6)+
  xlab("Site")+
  ylab("Daily Average Soil Temperature (ºC)")+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("control (ambient)", "treatment (OTC)"))+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        guide = "none")+
  labs(fill = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

airtemp <- ggplot(data = fullTemp, aes(x = site,
                                       y = air_dailyAvg_T,
                                       fill = treatment))+
  geom_boxplot()+
  geom_point(aes(colour = treatment),
             position=position_jitterdodge(jitter.width = 0.3),
             size = 0.7, alpha = 0.6)+
  xlab("Site")+
  ylab("Daily Average Air Temperature (ºC)")+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("control (ambient)", "treatment (OTC)"),
                      # legend turned off so that boxplots can be adjacent.
                      guide = "none")+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        guide = "none")+
  labs(fill = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("tempBoxplot.png", plot = airtemp + soiltemp + patchwork::plot_layout(),
       device = "png", path = here("figures"))
