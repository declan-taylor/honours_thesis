#source(here("scripts/data_assembly/master_dataframe.R"))

# AIR TEMPERATURE AGAINST NEE
lm_eqn <- function(df){
  m <- lm(NEE_umol_s_m2 ~ T_air, df);
  eq <- substitute(italic(NEE) == a + b %.% italic("canopy temperature")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

T_air.NEE <- ggplot(data = NEE,
                    aes(x = T_air, y = NEE_umol_s_m2))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = (max(NEE$T_air)-((max(NEE$T_air) - min(NEE$T_air))/2)), y = 0.075,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Canopy Temperature (ºC)",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# SOIL TEMPERATURE AGAINST NEE
lm_eqn <- function(df){
  m <- lm(NEE_umol_s_m2 ~ T_soil, df);
  eq <- substitute(italic(NEE) == a + b %.% italic("soil temperature")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

T_soil.NEE <- ggplot(data = NEE,
                     aes(x = T_soil, y = NEE_umol_s_m2))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = (max(NEE$T_soil)-((max(NEE$T_soil) - min(NEE$T_soil))/2)), y = 0.075,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Soil Temperature (ºC)",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionT_air_NEE.png", plot = T_air.NEE,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

ggsave("regressionT_soil_NEE.png", plot = T_soil.NEE,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

rm(T_air.NEE, T_soil.NEE)
