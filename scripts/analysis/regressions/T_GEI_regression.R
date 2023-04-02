#source(here("scripts/data_assembly/master_dataframe.R"))

# AIR TEMPERATURE AGAINST GEI
lm_eqn <- function(df){
  m <- lm(GEI ~ T_air, df);
  eq <- substitute(italic(GEI) == a + b %.% italic("canopy temperature")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

T_air.GEI <- ggplot(data = NEE,
                 aes(x = T_air, y = GEI))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  annotate("text", x = (max(NEE$T_air)-((max(NEE$T_air) - min(NEE$T_air))/2)), y = 0.095,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Canopy Temperature (ºC)",
       y = bquote("Greenness Excess Index (GEI)"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# SOIL TEMPERATURE AGAINST GEI
lm_eqn <- function(df){
  m <- lm(GEI ~ T_soil, df);
  eq <- substitute(italic(GEI) == a + b %.% italic("soil temperature")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

T_soil.GEI <- ggplot(data = NEE,
                    aes(x = T_soil, y = GEI))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  annotate("text", x = (max(NEE$T_soil)-((max(NEE$T_soil) - min(NEE$T_soil))/2)), y = 0.095,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Soil Temperature (ºC)",
       y = bquote("Greenness Excess Index"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionT_air_GEI.png", plot = T_air.GEI,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

ggsave("regressionT_soil_GEI.png", plot = T_soil.GEI,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

rm(T_air.GEI, T_soil.GEI)
