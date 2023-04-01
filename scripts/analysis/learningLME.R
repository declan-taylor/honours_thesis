library(ggplot2)
library(lme4)

# Book: https://webcat.library.ubc.ca/vwebv/holdingsInfo?bibId=3725818
# Ch 11, 12, 13. Linear regrssion, GLMs, mixed effects
# Check out partial pooling: estimate the intercept of any particular group <- pool info across all of the group to inform that paramaeter.

# Modelling practice in https://data.library.virginia.edu/correlation-of-fixed-effects-in-lme4/
# 30 observations for 20 subjects

n <- 20 # subjects
k <- 30 # observations
id <- gl(n, k)
x <- rep(1:k, n)

set.seed(1)
obs_noise <- rnorm(n * k, mean = 0, sd = 12) # noise on each observation within an sd = 12. 600 observations (30 for each subject)

set.seed(11)
subj_noise <- rnorm(n, mean = 0, sd = 20) # noise on each of the 20 observations.


# Generate a dependent variable, y, as a linear function of x using the formula y = 3 + 2*x.
y <- (3 + subj_noise[id]) + 2*x + obs_noise #subj_noise[id] repeates subject-specific noise 30x for each of the 20 subjects.
d <- data.frame(id, y, x)
## df = d, 30 observations for each subject. Subject numbered 1-20.

ggplot(d) +
  aes(x = x, y = y) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~id)
# Slopes are the same with random intercepts.

me1 <- lmer(y ~ x + (1|id), data = d)
summary(me1)
# id is a random effect because we're looking at our 30 observations (y) on 20 different samples (id).
# Results of summary:
## Under the “Fixed Effects” section the slope of x is estimated as 1.99 which is very close to the true value of 2.
## "Std. Dev" of "Random effects" are fairly close to the noise values we added.
## Uncertainty in our estimate is shown in the standard errors estimated on the intercept & fixed effect. X coefficient is 1.99 "give or take" 0.057.
## std. error is sq. root of estimated variance.

vcov(me1) #variances on fixed effects.
vcov(me1) |> diag() # diagonal values (intercept*intercept) and (x*x) of matrix output by `vcov(me1)`
vcov(me1) |> diag() |> sqrt() # square root of said diagonal values

# The formula for the correlaation coefficient. This is what the "Correlation of Fixed Effects" is 
vcov(me1)[2,1] / (vcov(me1) |> diag() |> sqrt() |> prod())

cov2cor(vcov(me1)) # also makes these calculations. 
# The resulting values show us the correlation coefficient in a matrix.







# COURTNEY MEETING
# The goal is to look at the "parameter *Estimate* of the slope of the relationship" (effect size) to determine the relationship between each variable and NEE (or other flux parameter).
# What is the intercept doing if it is significant?

# With a full model,one should examine the correlation of fixed effects
# Also use AIC or some other model selection tool to determine the salient fixed effects.
# Remove highly covarying responses (T_air and T_soil).

# Perhaps a section in results on model selection? Report the model output tables.

