
# Logistic Regression
# By David Moore
# 7-22-2019

# Check out these very helpful website:
# https://rcompanion.org/rcompanion/e_06.html
# https://rcompanion.org/rcompanion/e_07.html

# Simple Logistic Regression (i.e., only one predictor variable)

# Import and format the data
df1 <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df1 <- df1[, -grep("gpa|rank", colnames(df1))]
str(df1)
View(df1)

# Create the model
model1 <- glm(admit ~ gre, data = df1, family = binomial(link = "logit"))
summary(model1)

# Plot the data and the model
plot(admit ~ gre, data = df1, main = "Simple Logistic Regression Example", xlab = "GRE Score", ylab = "Probability of Graduate School Admission")
df2 <- data.frame(gre = seq(min(df1$gre), max(df1$gre), by = 1))
df2$predict <- predict(model1, newdata = df2, type = "response")
lines(predict ~ gre, data = df2)

# Determining if the model parameters are significant
library (car)
Anova(model1, type = "II", test = "Wald") # The null hypothesis of the Wald test is that the coefficient is not significantly different from zero; a p-value less than alpha means you should reject the null hypothesis and that the term is worth including in the model
# You should report Wald p-values in your final reports and publications

# Determining how well the model fits the data using pseudo-r-squared values
library (rcompanion)
nagelkerke(model1) # Pseudo-r-squared values are not true r-squared values because they aren't based on linear correlations, but they are still informative
library (BaylorEdPsych)
PseudoR2(model1)
# You'll probably want to report one of the pseudo-r-squared values in your final reports and publications

# Multiple Logistic Regression (i.e., more than one predictor variable)

# Import and format the data
df3 <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df3$rank <- as.factor(df3$rank)
str(df3)
View(df3)

# Create the model
model2 <- glm(admit ~ gre + gpa + rank, data = df3, family = binomial(link = "logit"))
summary(model2) # This is just for show; we will need to go through and determine which predictor variables are worth including in the final model using stepwise regression

# Using stepwise regression to determine which predictor variables are worth including in the model
model2_null <- glm(admit ~ 1, data = df3, family = binomial(link = "logit"))
model2_full <- glm(admit ~ gre + gpa + rank, data = df3, family = binomial(link = "logit"))
(stepwise_model <- step(model2_null, scope = list(upper=model2_full), direction = "both", test="Chisq", data = df3))
summary(stepwise_model) # It appears that all 3 predictor variables are worth including in the final model

# Determining if the model parameters are significant
library (car)
Anova(model2_full, type = "II", test = "Wald")

# Determining how well the model fits the data using pseudo-r-squared values
library (rcompanion)
nagelkerke(model2_full)
library (BaylorEdPsych)
PseudoR2(model2_full)
