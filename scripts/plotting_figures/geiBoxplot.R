library(tidyverse)
library(ggplot2)

source(here("scripts/data_assembly/GEI.R"))
generate_GEI(here("data/greenness/cropped_images"))

GEIboxplot <- ggplot(data = GEI,
                     aes(x = site, y = GEI))+
  geom_boxplot(aes(fill = treatment))+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave()