# Put all GEI regressions into a single compound figure.
source(here("scripts/analysis/regressions/GEI_NEE_regression.R"))
source(here("poster_materials/GEI_siteRegression_poster.R"))

ggsave("GEI_regressions.png", 
        # Top row of non-site-specific regressions
       (GEI.GEP + 
          theme(legend.position = "none") +
          GEI.ER +
          theme(legend.position = "none") +
          GEI.NEE),
       width = 5000, height = 3000, units = "px",
       device = "png", path = here("figures"))

ggsave("GEI_sites.png", 
        # Smaller boxes for site-specific regressions, without common y-axes.
        (GEI.GEP.sites + 
          theme(strip.text.x = element_text(face = "bold"),
                legend.position = "none")) /
          (GEI.ER.sites +
          theme(strip.text.x = element_text(face = "bold"))) /
          (GEI.NEE.sites + 
          theme(strip.text.x = element_text(face = "bold"),
                legend.position = "none")),
  width = 3000, height = 3000, units = "px",
  device = "png", path = here("figures"))
