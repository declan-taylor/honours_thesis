# To plot soil mositure data

SM <- read_csv(here("data/soil_moisture/2022_SM_ALLSITES.csv")) %>%
  filter(plot > 10) %>%
  mutate(plot = as.factor(plot))
  
SM_plot <- ggplot(data = SM, 
         aes(DOY, soil_moisture))+
    geom_point(aes(colour = factor(paste(plot, treatment))))+
    geom_line(aes(colour = factor(paste(plot, treatment))))+
    scale_colour_viridis_d() +
    facet_grid(~ site)+
    labs(colour = "Plot") +
    ylab("Soil Moisture (%)")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("SM_data.png", plot = SM_plot, device = "png", path = here("figures/exploratory"),
       width = 3800, height = 2000, units = "px")
