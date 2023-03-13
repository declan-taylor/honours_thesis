#source(here("scripts/assembly/master_dataframe.R"))
library(ggplot2)

# WILL GEP: over time

WILL.GEP <- GEP %>%
  filter(site == "WILL")

# how come se on geom_smooth is much greater than on stat_summary?

WILL.GEPline <- ggplot(data = WILL.GEP,
                       aes(x = doy, y = GEP_umol_s_m2))+
  geom_hline(yintercept = 0, linetype = 1, size = 0.5, colour = "#111111", alpha = 0.8)+
  geom_point(aes(colour = treatment),
            alpha = 0.7)+
  geom_smooth(aes(colour = treatment,
                  fill = treatment,
                  linetype = treatment),
              se = FALSE,
              size = 1.1,
              alpha = 0.1)+
  stat_summary(aes(colour = treatment),
               fun.data = "mean_se",
               geom = "pointrange", size = 0.4, shape = 21,
               position = "identity")+
  scale_colour_manual(values=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_linetype(labels=c("ambient", "warmed (OTC)"))+
  scale_fill_manual(values=c("#89C5DA", "#DA5724"),
                    guide = "none")+
  scale_x_continuous(limits = c(178, 210))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = "Day of Year",
       y = bquote('Willow Site ER (' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("GEP_WILL.png", plot = WILL.GEPline,
       width = 3000, height = 2000, units = "px",
       device = "png", path = here("figures/exploratory"))
