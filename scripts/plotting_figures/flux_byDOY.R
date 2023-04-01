library(tidyverse)
# source(here("scripts/data_assembly/master_dataframe.R"))

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

ggsave("flux_byDOY.png", plot = GEP_line + ER_line + NEE_line + patchwork::plot_layout(ncol = 1),
       height = 3000, width = 3000, units = "px",
       device = "png", path = here("figures"))
