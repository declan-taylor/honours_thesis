
# Regression line
lm_eqn <- function(df){
  m <- lm(NEE_umol_s_m2 ~ GEI, df);
  eq <- substitute(italic(NEE) == a + b %.% italic(GEI)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Regression plot
GEI.NEE <- ggplot(data = NEE,
                  aes(x = GEI, y = NEE_umol_s_m2))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875")+
  annotate("text", x = 0.02, y = 0.05,
           label = lm_eqn(NEE), parse = TRUE)+
  labs(x = "Greenness Excess Index",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionGEI_NEE.png", plot = GEI.NEE,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures"))
