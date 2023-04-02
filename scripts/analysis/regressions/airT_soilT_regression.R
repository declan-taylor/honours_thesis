lm_eqn <- function(df){
  m <- lm(T_soil ~ T_air, df);
  eq <- substitute(italic(T_soil) == a + b %.% italic("T_air")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

airT.soilT <- ggplot(data = NEE,
                 aes(x = T_air, y = T_soil))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  annotate("text", x = (max(NEE$T_air)-((max(NEE$T_air) - min(NEE$T_air))/2)), y = 0.001,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = bquote('Air Temperature (ºC)'),
       y = bquote('Soil Temperature (ºC)'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionTair_Tsoil.png", plot = airT.soilT,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

rm(airT.soilT)
