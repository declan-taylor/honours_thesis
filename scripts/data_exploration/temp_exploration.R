library(ggplot2)
library(here)

# Run `master_dataframe.R` prior to running.

## No temperature data at all on WILL DOY179, MEAD DOY182, DRYAS 183; those of 
# manual counts.
missingTemp <- asIRGA %>%
  ungroup() %>%
  filter(is.na(T_soil) == TRUE | is.na(T_air) == TRUE) %>%
  distinct(doy, site)

# Look for malfunctioning temperature:
badTemp <- asIRGA %>%
  filter(T_soil < -10000 | T_air < -10000)
## WILL DOY 192 and MEAD DOY 195 have "-99999" values; there are only 31 of these.
# These are correlated with huge fluxuations of completley irrelevant data. 
# Do we need to treat all the temp data the same and use HOBOs or should we 
# keep the IRGA readings for all except where we *need* HOBOs?

goodTemp <- asIRGA %>%
  filter(T_soil > -10000 & T_air > -10000) %>%
  mutate(doy = as.factor(doy))

# Removing MEAD DOY195 and WILL DOY 192. Since there were never two sites 
# recorded on the same day we can safely do this just by filtering DOYs.
goodDays <- asIRGA %>%
  filter(doy != 195) %>%
  filter(doy != 192)

# Plot to indicate where our unreliable readings are (excluding -99999 values)
unreliableTemp <- ggplot(data = goodTemp,
                         aes(x = time,
                             colour = plot))+
  geom_point(aes(y = T_soil))+
  geom_point(aes(y = T_air))+
  ylim(-100, 50)+
  scale_colour_viridis_d() +
  facet_grid(site ~ doy)+
  xlab("time (HH:MM:SS)")+
  ylab("temperature (ºC)")+
  theme_bw()

# Plot to investigate the rest of the temperature data. I found an outlier
# in WILL in both the soil and air temps.
airT_withOutlier <- ggplot(data = goodDays,
                           aes(x = time,
                               colour = site)) +
  guides(colour = "none")+
  geom_point(aes(y = T_soil),
             alpha = 0.7)+
  xlab("time (HH:MM:SS)") +
  ylab("soil temperature (ºC)")+
  labs(title = "Soil Temperature")+
  scale_colour_viridis_d()+
  theme_bw()

soilT_withOutlier <- ggplot(data = goodDays,
                            aes(x = time,
                                colour = site)) +
  geom_point(aes(y = T_air),
             alpha = 0.7)+
  xlab("time (HH:MM:SS)") +
  ylab("air temperature (ºC)")+
  labs(title = "Air Temperature")+
#  theme(legend.key = element_rect(fill = "grey"))+
  scale_colour_viridis_d()+
  theme_bw()

T_withOutlier <- airT_withOutlier + soilT_withOutlier + patchwork::plot_layout()

# FINDING THE OUTLIER: according to `T_withOutlier`, air temp never exceeds 25
# and soil_T never exceeds 20, except for this outlier in both cases.
outlier <- goodDays %>%
  filter(T_air > 25 & T_soil > 20)
# It is WILL DOY 208 12 T dark.


# FINAL STATEMENT: WE KNOW WE NEED HOBO DATA FOR:
# WILL DOY179, MEAD DOY182, DRYAS 183, WILL DOY 192, MEAD DOY195, and WILL DOY208 12T (dark).
# Use an if/else statement to fill or replace temp data on those days with HOBOs.

ggsave("temp_errorDays.png", unreliableTemp,
       device = png,
       path = here("figures/exploratory"),
       width = 3000, height = 1000, units = "px")

ggsave("temp_withOutlier.png", T_withOutlier,
       device = png,
       path = here("figures/exploratory"),
       width = 3000, height = 2000, units = "px")


  
# Histogram of temperature readings. Couldn't get it to display meaningful data.
ggplot(data = asIRGA)+
  geom_histogram(aes(x = T_soil),
                 binwidth = 1)+
  geom_histogram(aes(x = T_air),
                 binwidth = 1)+
  #scale_x_log10()+
  #scale_y_log10()+
  theme_bw()
