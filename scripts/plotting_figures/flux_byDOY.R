library(tidyverse)
# source(here("scripts/data_assembly/master_dataframe.R"))

NEE_line <- ggplot(data = NEE,
                   aes(x = doy, y = NEE_umol_s_m2))+
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
                      labels=c("ambient", "warmed (OTC)"),
                      guide = "none")+
  scale_linetype(labels=c("ambient", "warmed (OTC)"),
                 guide = "none")+
  scale_fill_manual(values=c("#89C5DA", "#DA5724"),
                    guide = "none")+
  facet_wrap(~ site)+
  scale_x_continuous(limits = c(178, 210))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = "Day of Year",
       y = bquote('NEE(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GEP_line <- ggplot(data = GEP,
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
                      labels=c("ambient", "warmed (OTC)"),
                      guide = "none")+
  scale_linetype(labels=c("ambient", "warmed (OTC)"),
                 guide = "none")+
  scale_fill_manual(values=c("#89C5DA", "#DA5724"),
                    guide = "none")+
  facet_wrap(~ site)+
  scale_x_continuous(limits = c(178, 210))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = NULL,
       y = bquote('GEP(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ER_line <- ggplot(data = ER,
                  aes(x = doy, y = ER_umol_s_m2))+
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
  facet_wrap(~ site)+
  scale_x_continuous(limits = c(178, 210))+
  labs(fill = NULL,
       colour = NULL,
       linetype = NULL,
       x = NULL,
       y = bquote('ER(' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GEP_line + ER_line + NEE_line + patchwork::plot_layout(ncol = 1)
