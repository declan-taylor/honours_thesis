library(tidyverse)
library(ggplot2)

#source(here("scripts/data_assembly/GEI.R"))
generate_GEI(here("data/greenness/cropped_images"))

GEIboxplot <- ggplot(data = GEI,
                     aes(x = treatment, y = GEI))+ 
  geom_boxplot(aes(fill = treatment))+
  facet_wrap(~ site)+ 
  xlab("Site")+
  ylab("Greeness Excess Index (GEI)")+
  scale_fill_discrete(type=c("#89C5DA", "#DA5724"),
                      labels=c("ambient", "warmed (OTC)"),
                      guide = "none")+
  scale_x_discrete(labels=c("ambient", "warmed (OTC)"))+
  labs(fill = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  # IS GEI SIGNIFICANTLY DIFFERENT BETWEEN SITES?
  
ggsave("GEIboxplot.png", plot = GEIboxplot,
       device = "png", path = here("figures"))
