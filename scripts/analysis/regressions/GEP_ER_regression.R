lm_eqn <- function(df){
  m <- lm(ER_umol_s_m2 ~ GEP_umol_s_m2, df);
  eq <- substitute(italic(ER) == a + b %.% italic("GEP")*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

GEP.ER <- ggplot(data = GEP,
                 aes(x = GEP_umol_s_m2, y = ER_umol_s_m2))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  annotate("text", x = (max(GEP$GEP_umol_s_m2)-((max(GEP$GEP_umol_s_m2) - min(GEP$GEP_umol_s_m2))/2)), y = -0.001,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 5,
           label = lm_eqn(GEP), parse = TRUE)+
  labs(x = bquote('Gross Ecosystem Productivity (GEP; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'),
       y = bquote('Ecosystem Respiration (ER; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionGEP_ER.png", plot = GEP.ER,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

rm(GEP.ER)
