# Figure with a single plot's flux and an R^2 value.
 
 lmFlux <- asIRGA %>%
   filter(site == "WILL" & doy == 208 & plot == 14 & treatment == "C" & light == "light")

 lm_eqn <- function(df){
   m <- lm(CO2_ppm ~ duration, df);
   eq <- substitute(italic(CO["2, ppm"]) == a + b %.% italic(duration)*","~~italic(R)^2~"="~r2, 
                    list(a = format(unname(coef(m)[1]), digits = 2),
                         b = format(unname(coef(m)[2]), digits = 2),
                         r2 = format(summary(m)$r.squared, digits = 3)))
   as.character(as.expression(eq));
 }
 
lmDemo <- 
  ggplot(lmFlux, aes(duration, CO2_ppm))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = "#019875") +
  xlab("Duration (s)") +
  ylab("CO2 (ppm)")+
  annotate("text", x = 90, y = 400,
           label = lm_eqn(lmFlux), parse = TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("lmFlux_demo.png", plot = lmDemo,
       device = "png", path = here("figures"))
  