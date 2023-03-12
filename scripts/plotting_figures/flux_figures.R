library(ggplot2)

NEE.boxplot <- ggplot(data = NEE) +
  geom_boxplot(aes(x = treatment,
                   y = NEE_umol_s_m2,
                   fill = treatment))+
  facet_wrap(~ site, scales = "free")+ 
  theme_bw()+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))+
  labs(fill="Treatment")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  