#source(here("scripts/data_assembly/GEI.R"))
generate_GEI(here("data/greenness/cropped_images"))

# Regression line
lm_eqn <- function(df, x_var_flux_type){
  m <- lm(get(paste0(x_var_flux_type, "_umol_s_m2")) ~ GEI, df);
  eq <- substitute(italic(NEE) == a + b %.% italic(GEI)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Regression plot NEE
GEI.NEE <- ggplot(data = NEE,
                  aes(x = GEI, y = NEE_umol_s_m2))+
  geom_point(aes(colour = treatment, shape = treatment))+
  geom_smooth(aes(colour = treatment, fill = treatment),
              method = "lm", se = TRUE, alpha = 0.2)+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = 0.035, y = 0.08,
           # SIZE SHOULD CHANGE FOR SINGULAR PLOT
           size = 4.5,
           label = lm_eqn(NEE, "NEE"), parse = TRUE)+
  scale_y_continuous(limits = c(-0.035, 0.09))+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        labels=c("ambient", "warmed (OTC)"))+
  scale_shape_discrete(labels=c("ambient", "warmed (OTC)"))+
  labs(x = "Greenness Excess Index",
       y = bquote('Net Ecosystem Exchange (NEE; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# NEXT DO THIS FOR ER AND GEP.
lm_eqn <- function(df, x_var_flux_type){
  m <- lm(get(paste0(x_var_flux_type, "_umol_s_m2")) ~ GEI, df);
  eq <- substitute(italic(ER) == a + b %.% italic(GEI)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

GEI.ER <- ggplot(data = ER,
                  aes(x = GEI, y = ER_umol_s_m2))+
  geom_point(aes(colour = treatment, shape = treatment))+
  geom_smooth(aes(colour = treatment, fill = treatment),
              method = "lm", se = TRUE, alpha = 0.2)+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = 0.035, y = 0.08, size = 4.5,
           label = lm_eqn(ER, "ER"), parse = TRUE)+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        labels=c("ambient", "warmed (OTC)"))+
  scale_shape_discrete(labels=c("ambient", "warmed (OTC)"))+
  scale_y_continuous(limits = c(-0.035, 0.09))+
  labs(x = "Greenness Excess Index",
       y = bquote('Ecosystem Respiration (ER; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lm_eqn <- function(df, x_var_flux_type){
  m <- lm(get(paste0(x_var_flux_type, "_umol_s_m2")) ~ GEI, df);
  eq <- substitute(italic(GEP) == a + b %.% italic(GEI)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

GEI.GEP <- ggplot(data = GEP,
                  aes(x = GEI, y = GEP_umol_s_m2))+
  geom_point(aes(colour = treatment, shape = treatment))+
  geom_smooth(aes(colour = treatment, fill = treatment),
              method = "lm", se = TRUE, alpha = 0.2)+
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", alpha = 1)+
  annotate("text", x = 0.035, y = 0.08, size = 4.5,
           label = lm_eqn(GEP, "GEP"), parse = TRUE)+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"))+
  scale_colour_discrete(type=c("#89C5DA", "#DA5724"),
                        labels=c("ambient", "warmed (OTC)"))+
  scale_shape_discrete(labels=c("ambient", "warmed (OTC)"))+
  scale_y_continuous(limits = c(-0.035, 0.09))+
  labs(x = "Greenness Excess Index",
       y = bquote('Gross Ecosystem Photosynthesis (GEP; ' *mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Export
ggsave("regressionGEI_NEE.png", plot = GEI.NEE,
       width = 2000, height = 1700, units = "px",
       device = "png", path = here("figures/regressions"))

ggsave("GEI_regressions.png", plot = GEI.GEP + GEI.ER + GEI.NEE + patchwork::plot_layout(),
       width = 5500, height = 3000, units = "px",
       device = "png", path = here("figures/regressions"))

#ggsave("GEI_regressions_poster.png", 
#       plot = GEI.GEP + 
#         theme(axis.text = element_text(size = 11),
#               axis.title = element_text(size = 15),
#               legend.position = "none") +
#         GEI.ER +
#         theme(axis.text = element_text(size = 11),
#               axis.title = element_text(size = 15),
#               legend.position = "none") +
#         GEI.NEE + 
#         theme(axis.text = element_text(size = 11),
#               axis.title = element_text(size = 15)) + 
#         patchwork::plot_layout(),
#       width = 5800, height = 3000, units = "px",
#       device = "png", path = here("poster_materials"))
