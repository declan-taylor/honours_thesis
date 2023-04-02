# GOAL: to determine if each SM, Tair, Tsoil, and GEI were different between 
# OTC/CTL, and between each of the three sites. Assemble into a table.

# For OTC/CTL, a simple t-test can be used. For MEAD/DRYAS/WILL, a one-way 
# ANOVA will be used as there are three factors.

str(NEE)
aovNEE <- NEE %>%
  mutate(plot = as.factor(plot))
# Check soil moisture
shapiro.test(aovNEE$soil_moisture) # p-value = 5.668e-05
qqnorm(aovNEE$soil_moisture) # logistic shaped
hist(aovNEE$soil_moisture) # right skewed

aovNEE <- aovNEE %>%
  mutate(logSM = log(soil_moisture))

# Check air temperature
shapiro.test(aovNEE$T_air) # p-value = 0.1913
bartlett.test(T_air ~ treatment, data = aovNEE) # p-value = 0.2756
bartlett.test(T_air ~ site, data = aovNEE) # p-value = 0.03785...not THAT small
qqnorm(aovNEE$T_air)
hist(aovNEE$T_air)

# Check soil temperature
shapiro.test(aovNEE$T_soil) # p-value = 0.2593
bartlett.test(T_soil ~ treatment, data = aovNEE) # p-value = 0.5259
bartlett.test(T_soil ~ site, data = aovNEE) # p-value = 0.0604

# Check GEI
shapiro.test(aovNEE$GEI) # p-value = 0.2164
bartlett.test(GEI ~ treatment, data = aovNEE) # p-value = 0.08832
bartlett.test(GEI ~ site, data = aovNEE) # p-value = 1.749e-05...yikes
qqnorm(aovNEE$GEI) # actually looks quite good
hist(aovNEE$GEI)

# SOIL MOISTURE
aov.SM <- aov(logSM ~ site * treatment, data = aovNEE)
summary(aov.SM)

# AIR TEMPERATURE
aov.airT <- aov(T_air ~ site * treatment, data = aovNEE)
summary(aov.airT)

# SOIL TEMPERATURE
aov.soilT <- aov(T_soil ~ site * treatment, data = aovNEE)
summary(aov.soilT)

# GEI
aov.GEI <- aov(GEI ~ site * treatment, data = aovNEE)
summary(aov.GEI)
# I'm not quite sure what to do about this.
