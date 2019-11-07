
# Analysis of Variance, Formulas, and Experimental Design

# ANOVAs look for differences between groups by determining
# if the variability between groups is greater than the
# variability within groups

# A completely random design: all observations are randomized
# within a field, bench, etc.
Yield <- rnorm(12, 3, 2) + 1:12
Treatment <- rep(c(paste("Treatment", 1:3, sep = "_"), "Control"), each = 3, length.out = 12)
Repetition <- as.factor(rep(1:3, length.out = 12))
Results <- data.frame(Repetition, Treatment, Yield)
View(Results)
summary(aov(Yield ~ Treatment, Results))

# A randomized complete block design: all observations occur
# equally in each block, and are randomized separately within
# each block
colnames(Results)[grep("Repetition", colnames(Results))] <- "Block"
summary(aov(Yield ~ Treatment + Block, Results))
# Why can't we see if there is a 'Block:Treatment' interaction
# here?
# It's because the 'Block:Treatment' interaction wasn't replicated
# and therefore we can't see if between-group variability is
# greater than within-group variability. The
# 'Block:Treatment' interaction is actually the error term in
# this ANOVA.

# A factorial design: we use these when there are more than
# one factor, or group of treatments, in the study
Yield <- rnorm(36, 3, 2) + 36:1
Fertilization_Rate <- rep(c("High", "Medium", "Low", "None"), each = 9)
Irrigation_Rate <- rep(c("High", "Low", "None"), each = 3, length.out = 36)
Block <- as.factor(rep(1:3, length.out = 36)) # We need to turn
# 'Block' into a factor or else we won't get the correct amount
# of Block degrees of freedom
Results <- data.frame(Block, Fertilization_Rate, Irrigation_Rate, Yield)
View(Results)
summary(aov(Yield ~ Fertilization_Rate * Irrigation_Rate + Block, Results))
# In the above analysis, what is the error term? What factors were
# not included in the model?
# The error term here includes all of the terms that were not included
# in the model. These terms are the 'Fertilization_Rate:Block'
# interaction, the 'Irrigation_Rate:Block' interaction, and the
# 'Fertilization_Rate:Irrigation_Rate:Block' interaction. If we look
# to see how many degrees of freedom go with each of these terms, we
# find that it's 6, 4, and 12, respectively. These sum to 22, which is
 # the number of degrees of freedom in our error term for this ANOVA.

# How are degrees of freedom calculated?
# Degrees of freedom are the number of observations in a particular
# group minus one. To calculate the degrees of freedom for an
# interaction term, you multiply all of the degrees of freedom for
# each of the main effects that are part of that interaction
# together.

# Explain the meaning of '*' and ':' in a model

help(formula)

# Other symbols that have meaning in a model:

# '^'
summary(aov(Yield ~ (Fertilization_Rate + Irrigation_Rate + Block) ^ 2, Results))

# '/'
summary(aov(Yield ~ (Fertilization_Rate / Irrigation_Rate) + Block, Results))

# Of course, we probably wouldn't want to perform the above analyses
# (particularly the latter one, where we completely ignore the main
# effect of 'Irrigation_Rate'); these are just examples to show you
# to use symbols in a formula

# Formulas and quantitative data: a linear model example
Yield <- (1:24) ^ 2 + rnorm(24, 3, 2)
Solar_Radiation <- 1:24 + 50 + rnorm(24, 2, 2)
Soil_N <- abs((1:24 * 5 - 4) + rnorm(24, 5, 10))
Soil_P <- abs((1:24 * 5 - 5) + rnorm(24, 5, 10))
Soil_K <- abs((1:24 * 5 - 10) + rnorm(24, 5, 10))
Results <- data.frame(Solar_Radiation, Soil_N, Soil_P, Soil_K, Yield)
# We can nest data transformations directly into the formula
# if we want:
summary(lm(Yield ~ Soil_N + Soil_P + Soil_K + Solar_Radiation, Results))
lm(Yield ~ log(Soil_N) + log(Soil_K) + log(Soil_P) + Solar_Radiation, Results)
# We can see which parameters are significant by using the
# 'summary' function:
summary(lm(Yield ~ log(Soil_N) + log(Soil_K) + log(Soil_P) + Solar_Radiation, Results))
# Here's a linear model that explores interactions between
# the three measured soil nutrients:
lm(Yield ~ Soil_N * Soil_K * Soil_P + Solar_Radiation, Results)
# Okay, but what if we don't care about these interactions
# and we actually want to use the product of 'Soil_N', 'Soil_P',
# and 'Soil_K' as a predictor variable?
# We need to use the 'I' function to tell R not to interpret
# the asterisk as representing all of the main effects and
# possible interactions
lm(Yield ~ 0 + I(Soil_N * Soil_K * Soil_P) + Solar_Radiation, Results)
?I
# The 'I' function lets us use '*', '/', '+', etc. in
# formulas so that they retain their mathematical meaning
# What about polynomials?
lm(Yield ~ Soil_N + I(Soil_N ^ 2) + Soil_P + I(Soil_P ^ 2) + Soil_K + I(Soil_K ^ 2) + Solar_Radiation, Results)
# That's a little cumbersome. Let's use the 'poly' function instead.
lm(Yield ~ poly(Soil_N, 2) + poly(Soil_P, 2) + poly(Soil_K, 2) + Solar_Radiation, Results)
# These aren't the same. What's the deal?
lm(Yield ~ poly(Soil_N, 2, raw = T) + poly(Soil_P, 2, raw = T) + poly(Soil_K, 2, raw = T) + Solar_Radiation, Results)
# When the 'raw' argument is set to 'T', R will calculate
# orthogonal polynomials, In other words, higher-order
# polynomials will only explain variation in the data not
# already explained by lower-order terms. Therefore, if you
# use the 'raw = T' argument, or if you use the linear model
# from line 104 above, if you include a higher-order term in
# your model, you also need to include all of the lower-order
# terms as well, because the higher-order term will only
# explain variability not already taken into account by lower-
# order terms. By default, though, the 'poly' function will
# not do this - it will generate higher-order terms that may be
# highly correlated. In other words, higher-order terms may be
# explaining the same variability in the data that lower-order
# terms are. Thus, if you use the default method where
# 'raw = F', you do not need to include lower-order terms in
# models that have significant higher-order terms. For a more
# in-depth explanaition, check out:
# https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal
