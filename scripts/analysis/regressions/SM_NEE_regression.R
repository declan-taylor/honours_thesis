#source(here("scripts/data_assembly/master_dataframe.R"))

# SOIL MOISTURE AGAINST NEE
lm_eqn <- function(df){
  m <- lm(NEE_umol_s_m2 ~ soil_moisture, df);
  eq <- substitute(italic(NEE) == a + b %.% italic("soil moisture")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

SM.NEE <- ggplot(data = NEE,
                  aes(x = soil_moisture, y = NEE_umol_s_m2))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = (max(NEE$soil_moisture)-((max(NEE$soil_moisture) - min(NEE$soil_moisture))/2)), y = 0.08,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Soil Moisture (%)",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionSM_NEE.png", plot = SM.NEE,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

rm(SM.NEE)
