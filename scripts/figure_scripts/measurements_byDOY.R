library(tidyverse)
# source(here("scripts/data_assembly/master_dataframe.R"))

# PART ONE: Flux Lines------------
# Three ggplot items are created, one for each flux, which show the flux averages over time.
NEE_line <- ggplot(data = NEE,
                   aes(x = doy, y = NEE_umol_s_m2))+
  # A zero line for showing where NEE switches sign.
  geom_hline(yintercept = 0, linetype = 1, size = 0.5, colour = "#111111", alpha = 0.8)+
  # Smoothed line between points. Removing the band around geom_smooth 
  # (commented out) and opting to use error bars instead.
  geom_smooth(aes(colour = treatment,
                  #fill = treatment,
                  linetype = treatment),
              se = FALSE,
              size = 1.1,
              alpha = 0.1)+
  # Adding error bars to the points
  stat_summary(aes(colour = treatment),
               fun.data = "mean_se",
               geom = "errorbar", size = 0.5, #shape = 3,
               position = "identity")+
  # Data points so illustrate where measurements are coming from on the smooth line.
  geom_point(aes(colour = treatment),
             alpha = 0.7)+
  # Set colours, labels, etc.
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"),
                      guide = "none")+
  scale_linetype(labels=c("ambient", "warmed (OTC)"),
                 guide = "none")+
  #scale_fill_manual(values=c("#89C5DA", "#DA5724"),
  #                  guide = "none")+
  # Setting a common DOY range.
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = "Day of Year",
       y = bquote('NEE(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  # Split into three for sites
  facet_wrap(~ site)+
  theme_bw()+
  # Remove gridlines
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GEP_line <- ggplot(data = GEP,
                   aes(x = doy, y = GEP_umol_s_m2))+
  geom_hline(yintercept = 0, linetype = 1, size = 0.5, colour = "#111111", alpha = 0.8)+
  # Smoothed line between points. Removing the band around geom_smooth 
  # (commented out) and opting to use error bars instead.
  geom_smooth(aes(colour = treatment,
                  #fill = treatment,
                  linetype = treatment),
              se = FALSE,
              size = 1.1,
              alpha = 0.1)+
  # Adding error bars to the points
  stat_summary(aes(colour = treatment),
               fun.data = "mean_se",
               geom = "errorbar", size = 0.5, #shape = 3,
               position = "identity")+
  # Data points so illustrate where measurements are coming from on the smooth line.
  geom_point(aes(colour = treatment),
             alpha = 0.7)+
  # Set colours, labels, etc.
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"),
                      guide = "none")+
  scale_linetype(labels=c("ambient", "warmed (OTC)"),
                 guide = "none")+
  #scale_fill_manual(values=c("#89C5DA", "#DA5724"),
  #                  guide = "none")+
  # Setting a common DOY range.
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8),
                     labels = NULL)+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = NULL,
       y = bquote('GEP(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  # Split into three for sites
  facet_wrap(~ site)+
  theme_bw()+
  # Remove gridlines
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ER_line <- ggplot(data = ER,
                  aes(x = doy, y = ER_umol_s_m2))+
  geom_hline(yintercept = 0, linetype = 1, size = 0.5, colour = "#111111", alpha = 0.8)+
  # Smoothed line between points. Removing the band around geom_smooth 
  # (commented out) and opting to use error bars instead.
  geom_smooth(aes(colour = treatment,
                  #fill = treatment,
                  linetype = treatment),
              se = FALSE,
              size = 1.1,
              alpha = 0.1)+
  # Adding error bars to the points
  stat_summary(aes(colour = treatment),
               fun.data = "mean_se",
               geom = "errorbar", size = 0.5, #shape = 3,
               position = "identity")+
  # Data points so illustrate where measurements are coming from on the smooth line.
  geom_point(aes(colour = treatment),
             alpha = 0.7)+
  # Set colours, labels, etc.
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  #scale_fill_manual(values=c("#89C5DA", "#DA5724"),
  #                  guide = "none")+
  # Setting a common DOY range.
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8),
                     labels = NULL)+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = NULL,
       y = bquote('ER(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  # Split into three for sites
  facet_wrap(~ site)+
  theme_bw()+
  # Remove gridlines
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# PART TWO: Displaying other variables over the same time frame----------------
# Temperature: Taking the line graphs from `HOBOfigures.R`

#source(here("scripts/data_assembly/temp_data.R"))
source(here("scripts/data_assembly/temp_data.R"))
addT("air", "all") #creates air_dailyAvg_T
addT("soil", "all") #creates soil_dailyAvg_T
fullTemp <- as_tibble(full_join(air_dailyAvg_T, soil_dailyAvg_T,
                                by = c("doy", "site", "plot", "treatment"),
                                keep = FALSE))

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
  scale_fill_manual(values=c("#89C5DA", "#DA5724"))+
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8),
                     labels = NULL)+
  labs(x = NULL,
       y = "Daily Average Air Temperature (ºC)",
       colour = NULL,
       linetype = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

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
  scale_fill_manual(values=c("#89C5DA", "#DA5724"))+
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8),
                     labels = NULL)+
  labs(x = NULL,
       y = "Daily Average Soil Temperature (ºC)",
       colour = NULL,
       linetype = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


# Soil Moisture
plotSM <- ggplot(data = soil_moisture %>% 
                   filter(plot > 10) %>%
                   group_by(site, treatment, year, doy) %>%
                   mutate(avgSM = mean(soil_moisture)) %>%
                   ungroup(),
                 aes(x = doy))+
  geom_smooth(aes(y = soil_moisture,
                  colour = treatment,
                  linetype = treatment),
              se = FALSE, span = 0.75,
              size = 1.1)+
  #stat_summary(aes(y = soil_moisture,
  #                 colour = treatment),
  #             fun.data = "mean_se",
  #             geom = "errorbar", 
  #             size = 0.5, alpha = 0.5, #shape = 3,
  #             position = "identity")+
  geom_point(aes(y = avgSM,
                 colour = treatment,
                 group = doy),
             alpha = 0.5,)+
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8),
                     labels = NULL)+
  labs(colour = NULL,
       linetype = NULL,
       x = NULL,
       y = "Soil Moisture (%)") +
  facet_wrap(~ site)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Greenness
plotGEI <- ggplot(data = GEI %>% 
                    group_by(site, treatment, doy) %>%
                    mutate(avgGEI = mean(GEI)) %>%
                    ungroup(),
                  aes(x = doy))+
  geom_smooth(aes(y = GEI,
                  colour = treatment,
                  linetype = treatment),
              se = FALSE, span = 0.75,
              size = 1.1)+
  stat_summary(aes(y = GEI,
                   colour = treatment),
               fun.data = "mean_se",
               geom = "errorbar", size = 0.5, #shape = 3,
               position = "identity")+
  geom_point(aes(y = avgGEI,
                 colour = treatment,
                 group = doy),
             alpha = 0.5,)+
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  scale_x_continuous(limits = c(178, 210),
                     breaks = scales::extended_breaks(n = 8))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = "Day of Year",
       y = "Greenness Excess Index (GEI)") +
  facet_wrap(~ site)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

airtempLine + soiltempLine + plotSM + plotGEI + patchwork::plot_layout(ncol = 1)
airtempLine + soiltempLine + plotSM + plotGEI + GEP_line + ER_line + NEE_line + patchwork::plot_layout(ncol = 1)

# SAVE-------------

ggsave("flux_byDOY.png", plot = GEP_line + ER_line + NEE_line + patchwork::plot_layout(ncol = 1),
       height = 3500, width = 3000, units = "px",
       device = "png", path = here("figures"))


ggsave("envFactors_byDOY.png", plot = airtempLine + soiltempLine + plotSM + plotGEI + patchwork::plot_layout(ncol = 1),
       height = 5000, width = 3000, units = "px",
       device = "png", path = here("figures"))
