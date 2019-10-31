
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
summary(aov(Yield ~ Treatment, Results))

# A randomized complete block design: all observations occur
# equally in each block, and are randomized separately within
# each block
colnames(Results)[grep("Repetition", colnames(Results))] <- "Block"
summary(aov(Yield ~ Treatment + Block, Results))
# Why can't we see if there is a 'Block:Treatment' interaction
# here?

# A factorial design: we use these when there are more than
# one factor, or group of treatments, in the study
Yield <- rnorm(36, 3, 2) + 36:1
Fertilization_Rate <- rep(c("High", "Medium", "Low", "None"), each = 9)
Irrigation_Rate <- rep(c("High", "Low", "None"), each = 3, length.out = 36)
Block <- as.factor(rep(1:3, length.out = 36)) # We need to turn
# 'Block' into a factor or else we won't get the correct amount
# of Block degrees of freedom
Results <- data.frame(Block, Fertilization_Rate, Irrigation_Rate, Yield)
summary(aov(Yield ~ Fertilization_Rate * Irrigation_Rate + Block, Results))
# In the above analysis, what is the error term? What factors were
# not included in the model?

# Discuss how degrees of freedom are calculated

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

# Talk about subsamples and pseudoreplications

# Nested designs

colnames(Results)[grep("Yield", colnames(Results))] <- "Yield_1"
Results$Yield_2 <- Results$Yield_1 + rnorm(nrow(Results), 2, 2)
Results$Yield_3 <- Results$Yield_1 + rnorm(nrow(Results), -2, 2)

# # There are other, more complicated designs. I'll talk about
# # split-plot designs and we'll analyze example of a strip-plot
# # design, which is a type of split-plot design.
# 
# Berry <- read.table("http://pages.stat.wisc.edu/~yandell/pda/data/Berry/berry.dat", header = T)
# View(Berry)
# ?reshape
# Berry_long <- reshape(Berry, idvar = c("sample", "tissue"), timevar = "conc",
#              times = paste("conc", 1:7, sep = ""), direction = "long", 
#              varying = list(paste("conc", 1:7, sep = "")))
# colnames(Berry_long)[3:4] <- c("time", "conc")
# Berry_long$time <- as.factor(gsub("conc", "", Berry_long$time))
# Berry_long$sample <- as.factor(Berry_long$sample)
# rownames(Berry_long) <- NULL
# View(Berry_long)
# str(Berry_long)
# 
# # Repeated measures is a type of split-plot: 'time' is nested
# # within 'sample'.
# 
# # In this case, 'tissue' is also nested within 'sample', so
# # this is a strip-plot design.
# 
# # What are the proper error terms for 'time' and 'tissue'?
# # (Sketch the design on the board)
# 
# Berry_mod_A <- aov(conc ~ tissue + sample + Error(sample / tissue) + 
#                      time + time:sample + time:tissue, Berry_long)
# summary(Berry_mod_A)
# Berry_mod_B <- aov(conc ~ time + sample + Error(sample / time) + 
#                      tissue + tissue:sample + time:tissue, Berry_long)
# summary(Berry_mod_B)
# Berry_mod <- aov(conc ~ (tissue + time + sample) ^ 2, Berry_long)
# (Berry_mod_ANOVA_table <- data.frame(unclass(summary(Berry_mod))))
# 
# # We need to do custom F-tests now because we can only
# # specify one 'Error' term per model:
# rownames(Berry_mod_ANOVA_table)
# # Let's use regular expressions to get at these:
# tissue_F_value <- Berry_mod_ANOVA_table[grep("^tissue\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"] / Berry_mod_ANOVA_table[grep("^tissue:sample\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"]
# time_F_value <- Berry_mod_ANOVA_table[grep("^time\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"] / Berry_mod_ANOVA_table[grep("^time:sample\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"]
# tissue_time_F_value <- Berry_mod_ANOVA_table[grep("^tissue:time\\s*$", rownames(Berry_mod_ANOVA_table)), "F.value"]
# 
# # F_value for sample:
# # ((MSsample + MSResiduals) / (MStissue:sample + MAtime:sample))
# 
# sample_F_value <- (Berry_mod_ANOVA_table[grep("^sample\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"] + Berry_mod_ANOVA_table[grep("^Residuals\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"]) / (Berry_mod_ANOVA_table[grep("^tissue:sample\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"] + Berry_mod_ANOVA_table[grep("^time:sample\\s*$", rownames(Berry_mod_ANOVA_table)), "Mean.Sq"])
# 
# # Explain variance components

# Formulas and quantitative data: a linear model example
Yield <- (1:24) ^ 2 + rnorm(24, 3, 2)
Solar_Radiation <- 1:24 + 50 + rnorm(24, 2, 2)
Soil_N <- abs((1:24 * 5 - 4) + rnorm(24, 5, 10))
Soil_P <- abs((1:24 * 5 - 5) + rnorm(24, 5, 10))
Soil_K <- abs((1:24 * 5 - 10) + rnorm(24, 5, 10))
Results <- data.frame(Solar_Radiation, Soil_N, Soil_P, Soil_K, Yield)
# We can nest data transformations directly into the formula
# if we want:
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
lm(Yield ~ I(Soil_N * Soil_K * Soil_P) + Solar_Radiation, Results)
?I
# The 'I' function lets us use '*', '/', '+', etc. in
# formulas so that they retain their mathematical meaning
# What about polynomials?
lm(Yield ~ Soil_N + I(Soil_N ^ 2) + Soil_P + I(Soil_P ^ 2) + Soil_K + I(Soil_K ^ 2) + Solar_Radiation, Results)
# That's a little cumbersome. Let's use the 'poly' function instead.
lm(Yield ~ poly(Soil_N, 2) + poly(Soil_P, 2) + poly(Soil_K, 2) + Solar_Radiation, Results)
# These aren't the same. What's the deal?
lm(Yield ~ poly(Soil_N, 2, raw = T) + poly(Soil_P, 2, raw = T) + poly(Soil_K, 2, raw = T) + Solar_Radiation, Results)




demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
View(demo1)
