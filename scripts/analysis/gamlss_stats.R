library(tidyverse)
library(gamlss)

conflicts_prefer(Matrix::pack(), Matrix::unpack(), Matrix::expand(),
                 MASS::select(),
                 stats::filter(), stats::lag())

gamlss::fitDist(NEE_umol_s_m2, data=NEE, type="realAll", try.gamlss=TRUE)
#Family:  c("RG", "Reverse Gumbel") 
#Fitting method: "nlminb" 

NEE.full <- gamlss(NEE_umol_s_m2 ~ treatment + soil_moisture + random (plot),
                   family = "RG",
                   data = NEE)

NEE.Fstep <- stepGAIC(NEE.full,
                      direction = "forward", trace = T)

summary(NEE.Fstep)