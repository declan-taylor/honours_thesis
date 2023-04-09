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
  geom_point(aes(colour = treatment),
             position=position_jitterdodge(jitter.width = 0.3),
             size = 0.7, alpha = 0.6)+
  geom_boxplot()+
  xlab("Site")+
  ylab("Daily Average Soil Temperature (ºC)")+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        guide = "none")+
  labs(fill = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

airtemp <- ggplot(data = fullTemp, aes(x = site,
                                       y = air_dailyAvg_T,
                                       fill = treatment))+
  geom_point(aes(colour = treatment),
             position=position_jitterdodge(jitter.width = 0.3),
             size = 0.7, alpha = 0.6)+
  geom_boxplot()+
  xlab("Site")+
  ylab("Daily Average Air Temperature (ºC)")+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)")#, guide = "none"
                      # legend can be turned off so that boxplots can be adjacent.
                      )+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        guide = "none")+
  labs(fill = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#ggsave("tempBoxplot.png", plot = airtemp + soiltemp + patchwork::plot_layout(),
#       device = "png", path = here("figures"))


# Trying a line figure too. With site as colour and treatment as shape.
ggplot(data = fullTemp,
       aes(x = doy, y = air_dailyAvg_T))+
  geom_point(aes(colour = site,
                 shape = treatment),
             alpha = 0.4)+
  geom_smooth(aes(colour = site,
                  linetype = treatment))+
  # Ordered DRYAS, MEAD, WILL
  scale_colour_manual(values=c("#A5A5A5", "#DE8344", "#37518A"))+
  theme_bw()

# Line figure with just treatment.
ggplot(data = fullTemp,
       aes(x = doy, y = air_dailyAvg_T))+
  geom_point(aes(colour = treatment,
                 shape = treatment),
             alpha = 0.3)+
  geom_smooth(aes(colour = treatment,
                  linetype = treatment))+
  scale_colour_manual(values=c("#89C5DA", "#DA5724"))+
 # scale_shape_manual(guide = "none")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  

# Line figure with just treatment, sites are faceted.
airtempLine <- ggplot(data = fullTemp,
                      aes(x = doy, y = air_dailyAvg_T))+
  geom_line(aes(colour = treatment,
                 group = plot),
             alpha = 0.1)+
  geom_smooth(aes(colour = treatment,
                  fill = treatment,
                  linetype = treatment),
              method = "loess", span = 0.4,
              size = 1,
              alpha = 0.4)+
  facet_wrap(~ site)+ 
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  scale_fill_manual(values=c("#89C5DA", "#DA5724"),
                    guide = "none")+
  xlab(NULL)+
  ylab("Daily Average Air Temperature (ºC)")+
  labs(colour = NULL,
       linetype = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

soiltempLine <- ggplot(data = fullTemp,
                      aes(x = doy, y = soil_dailyAvg_T))+
  geom_line(aes(colour = treatment,
                group = plot),
            alpha = 0.15)+
  geom_smooth(aes(colour = treatment,
                  fill = treatment,
                  linetype = treatment),
              method = "loess", span = 0.4,
              size = 1,
              alpha = 0.4)+
  facet_wrap(~ site,)+ 
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  scale_fill_manual(values=c("#89C5DA", "#DA5724"),
                    guide = "none")+
  xlab("Day of Year")+
  ylab("Daily Average  Soil Temperature (ºC)")+
  labs(colour = NULL,
       linetype = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#ggsave("tempLine.png", plot = airtempLine + soiltempLine + patchwork::plot_layout(ncol = 1),
#       device = "png", path = here("figures"))
