# Load data and libraries
#source(here("scripts/assembly/master_dataframe.R"))
library(ggplot2)

# NEE, GEP, ER fluxes represented by C/T boxplots
NEE.boxplot <- ggplot(data = NEE)+
  geom_boxplot(aes(x = treatment,
                   y = NEE_umol_s_m2,
                   fill = treatment))+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  facet_wrap(~ site)+ 
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      guide = "none")+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"))+
  labs(fill = NULL,
       x = "Treatment",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GEP.boxplot <- ggplot(data = GEP) +
  geom_boxplot(aes(x = treatment,
                   y = GEP_umol_s_m2,
                   fill = treatment))+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  facet_wrap(~ site)+ 
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      guide = "none")+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"))+
  labs(fill = NULL,
            x = "Treatment",
            y = bquote('Gross Ecosystem Productivity (GEP; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ER.boxplot <- ggplot(data = ER) +
  geom_boxplot(aes(x = treatment,
                   y = ER_umol_s_m2,
                   fill = treatment))+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  facet_wrap(~ site)+ 
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      guide = "none")+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"))+
  labs(fill = NULL,
       x = "Treatment",
       y = bquote('Ecosystem Respiration (ER; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("NEE_boxplot.png", plot = NEE.boxplot,
       device = "png", path = here("figures"))
ggsave("GEP_boxplot.png", plot = GEP.boxplot,
       device = "png", path = here("figures"))
ggsave("ER_boxplot.png", plot = ER.boxplot,
       device = "png", path = here("figures"))

ggsave("flux_boxplots.png", plot = GEP.boxplot+labs(x = NULL) + 
                                    ER.boxplot+labs(x = NULL) + 
                                    NEE.boxplot + patchwork::plot_layout(ncol = 1),
       height = 4000, width = 3000, units = "px",
       device = "png", path = here("figures"))
