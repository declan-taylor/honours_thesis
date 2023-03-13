library(tidyverse)
library(ggplot2)

source(here("scripts/data_assembly/GEI.R"))
generate_GEI(here("data/greenness/cropped_images"))

GEI_explore <- ggplot(data = GEI,
                      aes(x = doy, y = GEI))+
  geom_smooth(aes(colour = site,
                  linetype = treatment),
              se = FALSE)+
  geom_point(aes(colour = site,
                 shape = treatment))+
  # Ordered DRYAS, MEAD, WILL
  scale_colour_manual(values=c("#A5A5A5", "#DE8344", "#37518A"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())