# GOAL: to determine if each SM, Tair, Tsoil, and GEI were different between 
# OTC/CTL, and between each of the three sites. Assemble into a table.

# Preliminary: load HOBO data-----------
addT("air", "all") #creates air_dailyAvg_T
addT("soil", "all") #creates soil_dailyAvg_T

airT.24 <- filter(air_dailyAvg_T, plot == 11 | plot == 12 | plot == 13 | plot == 14)
soilT.24 <- filter(soil_dailyAvg_T, plot == 11 | plot == 12 | plot == 13 | plot == 14)

addT("air", "daytime") #creates air_temp
addT("soil", "daytime") #creates soil_temp

airT.day <- filter(air_temp, plot == 11 | plot == 12 | plot == 13 | plot == 14)

soilT.day <- filter(soil_temp, plot == 11 | plot == 12 | plot == 13 | plot == 14)

rm(soil_dailyAvg_T, air_dailyAvg_T, soil_temp, air_temp)

# Investigate data----------
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

# Check IRGA air temperature
shapiro.test(aovNEE$T_air) # p-value = 0.1913
bartlett.test(T_air ~ treatment, data = aovNEE) # p-value = 0.2756
bartlett.test(T_air ~ site, data = aovNEE) # p-value = 0.03785...not THAT small
qqnorm(aovNEE$T_air)
hist(aovNEE$T_air)

# Check HOBO air temperature
shapiro.test(airT$air_dailyAvg_T) # p-value = 9.544e-09, sqrt and log 
# transformaitons did not meaningfully change this.
bartlett.test(air_dailyAvg_T ~ treatment, data = airT) # p-value = 1.255e-07
bartlett.test(air_dailyAvg_T ~ site, data = airT) # p-value = 0.1699
qqnorm(airT$air_dailyAvg_T) # fairly straight line
hist(airT$air_dailyAvg_T) # looks a bit right skewed but fairly normal

# Check IRGA soil temperature
shapiro.test(aovNEE$T_soil) # p-value = 0.2593
bartlett.test(T_soil ~ treatment, data = aovNEE) # p-value = 0.5259
bartlett.test(T_soil ~ site, data = aovNEE) # p-value = 0.0604

# Check HOBO soil temperature
shapiro.test(soilT$soil_dailyAvg_T) # p-value = 0.001456
bartlett.test(soil_dailyAvg_T ~ treatment, data = soilT) # p-value = 0.4371
bartlett.test(soil_dailyAvg_T ~ site, data = soilT) # p-value = 7.309e-06
qqnorm(soilT$soil_dailyAvg_T) # fairly straight line
hist(soilT$soil_dailyAvg_T) # looks super duper normal

# Check GEI
shapiro.test(aovNEE$GEI) # p-value = 0.2164
bartlett.test(GEI ~ treatment, data = aovNEE) # p-value = 0.08832
bartlett.test(GEI ~ site, data = aovNEE) # p-value = 1.749e-05...yikes
qqnorm(aovNEE$GEI) # actually looks quite good
hist(aovNEE$GEI)

# SOIL MOISTURE
aov.SM <- aov(logSM ~ site * treatment, data = aovNEE)
summary(aov.SM)
interaction.plot(x.factor = aovNEE$treatment,
                 trace.factor = aovNEE$site,
                 response = aovNEE$logSM,
                 type = "b",
                 fun = mean)
# MEAD had MUCH higher soil moisture overall and T > C (treatments wetter).
# DRYAS, WILL, had lower soil moisture and T < C (controls wetter)

# AIR TEMPERATURE
# (IRGA)
summary(aov(T_air ~ site * treatment, data = aovNEE))
interaction.plot(x.factor = aovNEE$treatment,
                 trace.factor = aovNEE$site,
                 response = aovNEE$T_air,
                 type = "b",
                 fun = mean)
# MEAD had the least significant increase and was the coolest. WILL and DRYAS 
# had comparable increases in temperature, and WILL was warmer than MEAD.

# (HOBO: daytime measurement period)
summary(aov(air_daytimeT ~ site * treatment, data = airT.day))
interaction.plot(x.factor = airT.day$treatment,
                 trace.factor = airT.day$site,
                 response = airT.day$air_daytimeT,
                 type = "b",
                 fun = median,
                 ylab = "Mean daily average air temperature (ºC)",
                 xlab = "Treatment",
                 trace.label = NULL)
# MEAD was the warmest by a chunk and increased a comparable amount to WILL,
# which started the coldest but overtook DRYAS. DRYAS had the least significant
# warming response.

# (HOBO: 24h measurement period)
summary(aov(air_daytimeT ~ site * treatment, data = airT.24))
interaction.plot(x.factor = airT.24$treatment,
                 trace.factor = airT.24$site,
                 response = airT.24$air_daytimeT,
                 type = "b",
                 fun = median,
                 ylab = "Mean daily average air temperature (ºC)",
                 xlab = "Treatment",
                 trace.label = NULL)
## While all the sites had comparable mean daily average air temperatures in the
## control plots (DRYAS > WILL > MEAD), they had considerably different means
## in the treatments (MEAD > WILL > DRYAS). MEAD had the biggest C/T difference,
## followed by WILL, then DRYAS.


# SOIL TEMPERATURE
# (IRGA)
summary(aov(T_soil ~ site * treatment, data = aovNEE))

# (HOBO: daytime measurement period)
summary(aov(soil_daytimeT ~ site * treatment, data = soilT.day))

# (HOBO: 24h measurement period)
summary(aov(soil_daytimeT ~ site * treatment, data = soilT.24))

# GEI
aov.GEI <- aov(GEI ~ site * treatment, data = aovNEE)
summary(aov.GEI)
interaction.plot(x.factor = aovNEE$treatment,
                 trace.factor = aovNEE$site,
                 response = aovNEE$GEI,
                 type = "b",
                 fun = mean)
# I'm not quite sure what to do about this.


# Compute means for air temperature-----------
mean(pull(filter(aovNEE, treatment == "C")["T_air"])) 
# T_air = 14.11744
mean.C <- mean(pull(filter(airT.24, treatment == "C")["air_dailyAvg_T"]))
# T_air = 9.637753
mean.C <- mean(pull(filter(airT.day, treatment == "C")["air_daytimeT"]))
# T_air = 12.74651

mean(pull(filter(aovNEE, treatment == "T")["T_air"])) 
# T_air = 14.70381
mean.T <- mean(pull(filter(airT.24, treatment == "T")["air_dailyAvg_T"]))
# T_air = 11.13659
mean.T <- mean(pull(filter(airT.day, treatment == "T")["air_daytimeT"]))
# T_air = 14.50387

1-mean.C/mean.T
# 12.12% higher for 24h.

# Compute means for soil temperature------
mean(pull(filter(aovNEE, treatment == "C")["T_soil"])) 
# T_air = 9.524282
mean.C <- mean(pull(filter(soilT.24, treatment == "C")["soil_dailyAvg_T"]))
# T_air = 7.252091
mean.C <- mean(pull(filter(soilT.day, treatment == "C")["soil_daytimeT"]))
# T_air = 8.296886

mean(pull(filter(aovNEE, treatment == "T")["T_soil"])) 
# T_air = 10.27558
mean.T <- mean(pull(filter(soilT.24, treatment == "T")["soil_dailyAvg_T"]))
# T_air = 7.062826
mean.T <- mean(pull(filter(soilT.day, treatment == "T")["soil_daytimeT"]))
# T_air = 7.851463

1-mean.C/mean.T
# -2.68% colder in 24h
# -5.67% colder in daytime