
# Four Different Distributions: The Normal, t, F, and Chi-Square Distributions
# By David Moore
# 7-8-2019

# Shapiro-Wilk Test for Normality
w <- c(12, 14, 14.5, 17, 11, 21, 3, 5, 19, 24)
shapiro.test(w) # If the p-value is below 0.05, the data are probably not normally distributed (in this example, they seem to follow a normal distribution)
hist(w, probability = T); lines(density(w), col = "red")

# The standard normal distribution
x <- seq(-4, 4, 0.01)
plot(x, dnorm(x), main = "Standard Normal Distribution", xlab = "z-score (or Quantile)", ylab = "Probability Density")

# dnorm, pnorm, qnorm, and rnorm functions

# dnorm: returns the probability density (in other words, the y-coordinate
# of the point for the x-coordinate you provide)
dnorm(0)
dnorm(1)

# pnorm: returns the area under the curve (by default, we get the area
# under the curve and to the left of the x-coordinate we provide)
pnorm(0)
pnorm(1.96)
pnorm(-2)
pnorm(2)
pnorm(2, lower.tail = F)
1 == pnorm(2) + pnorm(2, lower.tail = F)

# qnorm is the inverse of pnorm - in other words, it returns the quantile,
# or the x-coordinate, for the probability you provide (again, by default,
# it's assumed the area you provide is under the curve and to the left
# (i.e., the lower tail) of the x-coordinate)
qnorm(.5)

# rnorm: returns randomly-generated numbers that come from
# a normal distribution
rnorm(10)
rnorm(10, mean = 5, sd = 2) # These functions work for distributions with different means and standard deviations too

# t distributions
y <- seq(-4, 4, 0.01)
df1 <- data.frame(y, normal = dnorm(y), t2 = dt(y, 2), t5 = dt(y, 5), t10 = dt(y, 10))
matplot(df1$y, df1[, 2:5], pch = 16, main = "Normal and t Distributions", ylab = "Probability Density", xlab = "z (or t)"); legend("topright", legend = c("Normal", "t (2 df)", "t (5 df)", "t (10 df)"), pch = 16, col = 1:4)
# A t-distribution with infinity degrees of freedom is a normal distribution
# (t-distributions penalize you for having small sample sizes)

# Like pnorm, qnorm, dnorm, and rnorm, there are also pt, qt, dt, and rt (this time, you have to specify the degrees of freedom)
pt(0, 4)
pt(0, 40)
pt(1.96, 4)
dt(2, 9)
qt(0.975, 10)
rt(10, 4)

# F distributions
y <- seq(0, 5, 0.01)
df2 <- data.frame(y, f1.1 = df(y, 1, 1), f5.2 = df(y, 5, 2), df2.5 = df(y, 2, 5), df5.15 = df(y, 5, 15), df15.5 = df(y, 15, 5), df20.20 = df(y, 20, 20))
matplot(df2$y, df2[, 2:7], pch = 16, main = "F Distributions", ylab = "Probability Density", xlab = "F"); legend("topright", legend = c("df = 1, 1", "df = 2, 5", "df = 5, 2", "df = 5, 15", "df = 15, 5", "df = 20, 20"), pch = 16, col = 1:6)
# F-scores are ratios of variances; thus, there is a degrees of freedom parameter associated with the numerator,
# and there is a degrees of freedom parameter associated with the numerator. Since F-scores are ratios of two
# positive numbers, F-scores are always between zero and infinity.

pf(1, 20, 20)
qf(0.5, 20, 20)
df(1.75, 20, 20)
rf(20, 20, 20)

# Chi-square distributions
y <- seq(0, 80, 0.1)
df3 <- data.frame(y, chisq1 = dchisq(y, 1), chisq5 = dchisq(y, 5), chisq15 = dchisq(y, 15), chisq40 = dchisq(y, 40))
matplot(df3$y, df3[, 2:5], pch = 16, main = "Chi-Square Distributions", ylab = "Probability Density", xlab = expression(chi^2), ylim = c(0, 0.25)); legend("topright", legend = c("df = 1", "df = 5", "df = 15", "df = 40"), pch = 16, col = 1:4)
# Chi-square values have one degree of freedom parameter. For each factor in the experiment of study, you calculate
# N - 1 (N is the number of levels of a particular factor). You multiply all of the N - 1 values from each factor
# together, and the result is the degrees of freedom.
# Like the F-distribution, the Chi-square distribution is not symmetrical, and it's also never negative (it is also
# calculated using squared deviations, and since differences are squared, they can never be negative)

pchisq(1.8, 1, lower.tail = F)
pchisq(5.68, 1, lower.tail = F)
qchisq(0.5, 40)
