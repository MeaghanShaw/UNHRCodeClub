
# Distributions
# 3-2-2020

# All of these distributions have a total area under
# the curve of one. This is useful because
# probabilities are between 0 and 1. If our data
# follow one of these distributions, we can
# calculate the probability of an observation being
# within a certain range of values based on the
# area under the curve between two x-coordinates.

# The uniform distribution

# Observations from every quantile are equally likely
# under a uniform distribution.

x <- seq(0, 1, 0.01)
Uniform <- dunif(x)
plot(x, Uniform, type = "l", main = "Uniform Distribution",
     xlab = "Quantile", ylab = "Density")

# This distribution isn't too exciting. We'll go over
# density, probability, and quantile functions with the
# normal distribution. Keep in mind that these three
# concepts are applicable to the uniform distribution
# and every other distribution too.

# The normal, or Gaussian, distribution

x <- seq(-4, 4, 0.01)
y <- dnorm(x)
plot(x, y, xlab = "Quantile (or z Value)",
     ylab = "Density",
     main = "The Standard Normal Distribution",
     type = "l")

# The standard normal distribution, or the Z distribution,
# has a mean of 0 and a standard deviation of 1. There are
# many other normal distributions, though, each having
# different means and standard deviations. In other words,
# normal distributions don't have to be centered at zero
# and have a standard deviation of 1. The standard
# normal distribution is a special case of the normal
# distribution for which this is true.

# Normal distributions all have the same area under the
# curve - the area under the curve equals one for all of
# them. This makes it easy to determine probabilities,
# because like these areas under the curve, probabilities
# always sum to one.

# Probability, density, and quantile functions

# All three of these assume the standard normal
# distribution unless you specify otherwise.

# The density function

# You specify the quantile (i.e., the x-value), and
# it returns the density (i.e., the y-value).

dnorm(0)
dnorm(-1.96)
dnorm(50, mean = 48, sd = 2)

# The probability function

# You specify the quantile (i.e., the x-value), and
# it returns the probability (i.e., the area under
# the curve).

pnorm(0)
pnorm(-1.96)
pnorm(0, mean = 1, sd = 5)

# What do these probabilities represent?

# They represent the area under the curve to the left
# of the quantile you specify (in other words, they
# represent the 'lower tail'. If you want to return
# the upper tail, you can do so using another argument.

pnorm(1.96, lower.tail = F)

# This will return the area under the curve to the
# right of the specified quantile.

# The quantile function

# You specify the probability (i.e., the area under the
# curve), and it will return the quantile (i.e., the
# x-value).

qnorm(0.5)
qnorm(0.025)
qnorm(0.025, lower.tail = F)
qnorm(0.01)
qnorm(0.99)

# There is also a random-number-generating function
# that will generate numbers randomly from the normal
# distribution.

rnorm(10)
rnorm(10, 25, 2)

# Converting normally-distributed samples into z scores

# To convert normally-distributed samples into z scores,
# or quantiles from the standard normal distribution,
# we first subtract the mean and then divide by the
# standard deviation.

(Lengths <- rnorm(16, 15, 2.2))
((Lengths - mean(Lengths)) / sd(Lengths))

# This process re-scales your measurements so that the
# mean is zero and the standard deviation is one.

# What is a quantile-quantile plot?

# Let's say you have several measurements that you
# suspect are normally-distributed. We can use the
# 'Lengths' vector we just created.

Lengths

# These measurements have a mean and a standard
# deviation.

(Lengths_Mean <- mean(Lengths))
(Lengths_Standard_Deviation <- sd(Lengths))

# Based on this information, you could construct a
# normal curve that has this mean and this standard
# deviation.

x <- seq(Lengths_Mean - 10, Lengths_Mean + 10, by = 0.1)
y <- dnorm(x, mean = Lengths_Mean, sd = Lengths_Standard_Deviation)
plot(x, y, type = "l", main = "Theoretical Distribution of Lengths",
     xlab = "Length", ylab = "Frequency")
abline(h = 0)

# We have 16 length measurements from this sample.
# Therefore, let's split this curve up into 16
# parts, each part having an equal area under the
# curve.

Area <- 1 / length(Lengths)
for (i in seq_len(length(Lengths) - 1)) {
  abline(v = qnorm(Area * i, mean = Lengths_Mean,
                   sd = Lengths_Standard_Deviation),
         col = "red")
}

# Each of these sections contains it's own median.
# These medians are:

Medians <- NULL
for(i in seq_len(length(Lengths))) {
  Medians[i] <- qnorm(((Area / 2) * ((i * 2) - 1)),
                      mean = Lengths_Mean,
                      sd = Lengths_Standard_Deviation)
}
Medians

# Or, if we look at these on the plot:

points(Medians, dnorm(Medians, mean = Lengths_Mean,
                      sd = Lengths_Standard_Deviation),
       pch = 20, col = "blue")
points(Medians, rep(0, length(Lengths)), pch = 20,
       col = "blue")

# If our measurements truly followed a normal
# distribution that had a mean of

Lengths_Mean

# and a standard deviation of

Lengths_Standard_Deviation

# then our measurements would be exactly these
# medians.

# So, let's plot these medians against our
# measured lengths (after we order them from
# least to greatest).

plot(Medians, sort(Lengths),
     main = "Quantile-Quantile Plot",
     xlab = "Theoretical Values",
     ylab = "Actual Values")
abline(coef = c(0, 1), col = "red")

# This is a quantile-quantile plot. The trendline
# for this plot should be very close to a line
# having a slope of 1 and a y-intercept of 0; if
# it is, then we would say that our measurements
# follow a normal distribution.

# The t distribution

# The t distribution is like the normal distribution,
# but it is based on finite sample sizes (and thus
# degrees of freedom), and thus it is more variable
# than the normal distribution, particularly when
# sample sizes are small.

# A t distribution with infinity degrees of freedom
# is a normal distribution.

x <- seq(-5, 5, 0.1)
Normal <- dnorm(x)
t_Inf <- dt(x, Inf)
all(Normal == t_Inf)
t_25 <- dt(x, 25)
t_10 <- dt(x, 10)
t_5 <- dt(x, 5)
t_2 <- dt(x, 2)
t_1 <- dt(x, 1)
matplot(x, cbind(t_Inf, t_25, t_10, t_5, t_2, t_1),
        type = "l", lty = 1, col = 1:6,
        xlab = "Quantile (or t Value)",
        ylab = "Density", main = "t Distributions")
legend("topright", title = "Degrees of Freedom",
       legend = c("Infinity", "25", "10", "5", "2", "1"),
       col = 1:6, lty = 1)

# The same functions for density, probability, quantile,
# and randomly-generated numbers are available for t
# distributions. In addition to the arguments you had
# to specify for these functions for normal distributions,
# you also have to specify the degrees of freedom.

dt(0, 2)
pt(2, 10)
qt(0.05, 19)

# Celebration of Knowledge # 1:

# Calculate the quantile of an observation that delineates
# the upper 2.5 % of a t distribution with 24 degrees of
# freedom.

qt(0.025, 24, lower.tail = F)
qt(0.975, 24)

# Central Limit Theorem

# No matter what distribution your observations follow,
# if you repeatedly sample a population and calculate
# the means for each of these samples, these means
# will follow a normal distribution.

# This is arguably the most important theorem in all
# of statistics.

# Standard errors are similar to, but different than,
# standard deviations

# The standard error equals the standard deviation
# divided by the square root of the number of
# observations.

# Let's write a function to calculate standard errors
# for us. (R doesn't have a built-in function that
# does this.)

SE_fxn <- function (x) {
  sd(x) / sqrt(length(x))
}

# Here's an example:

# We can use the 'Lengths' vector we created before
# again.

Lengths

# This sample has a standard deviation and a mean.
# (We've already calculated these.)

sd(Lengths)
mean(Lengths)

# The sample's standard error is:

SE_fxn(Lengths)

# Conceptually, the standard error is an estimate
# of how far the sample mean is likely to be from
# the population mean. The more observations you
# take, the more likely the mean you calculate
# will be close to the true population mean.

# Celebration of Knowledge # 2

# When you put error bars on a plot, should you
# use the standard error or the standard
# deviation? Why?

# Answer: you should use twice the standard error.
# This is because if you're trying to show
# differences between groups, it's the means that
# you're trying to prove are different. You don't
# care about individual observations. Furthermore,
# since you probably want to show 95 % confidence
# limits for the mean, you'd multiple the standard
# error by 2. Why? It's because 2 is very close to
# the z score that delineates p = 0.025 in each tail.

qnorm(0.025, lower.tail = F)

# The F distribution

# The F distribution is a ratio of variances.

# Variances are always positive because they are
# calculated as the sums of squared residuals divided
# by the number of observations. Therefore, the
# F distribution cannot be negative since it is the
# ratio of two positive numbers.

x <- seq(0, 5, 0.01)
F_1_1 <- df(x, 1, 1)
F_1_5 <- df(x, 1, 5)
F_5_1 <- df(x, 5, 1)
F_5_5 <- df(x, 5, 5)
F_5_25 <- df(x, 5, 25)
F_25_5 <- df(x, 25, 5)
F_25_25 <- df(x, 25, 25)
F_100_100 <- df(x, 100, 100)
matplot(x, cbind(F_1_1, F_1_5, F_5_1, F_5_5, F_5_25,
                 F_25_5, F_25_25, F_100_100),
        type = "l", lty = 1, col = 1:8,
        xlab = "Quantile (or F Value)", ylab = "Density",
        main = "F Distributions")
legend("topright", title = "Degrees of Freedom",
       legend = c("1, 1", "1, 5", "5, 1", "5, 5",
                  "5, 25", "25, 5", "25, 25", "100, 100"),
       col = 1:8, lty = 1)

# There are two degrees of freedom associated with
# each F distribution. The first is for the numerator
# and the second is for the denominator.

# When we perform an analysis of variance, we look at
# the ratio of between-group variance to within-
# group variance. Thus, the first degrees of freedom
# corresponds with the between-group variance, and
# the second corresponds with the within-group
# variance.

# Using an F distribution with 20 and 21 degrees of
# freedom, calculate the quantile where p = 0.025
# for both the upper and the lower tail. Why are
# these numbers different? They were the same (albeit
# with a different sign) for the normal distribution.

qf(0.025, 20, 21)
qf(0.025, 20, 21, lower.tail = F)

# Celebration of Knowledge # 3

# When performing an analysis of variance, you want
# to know if the variance between groups is
# significantly greater than the variance within
# groups.

# The F distribution is not symmetrical like the
# other distributions we've seen so far.

# Why is it important to make sure that you're looking
# at the upper, and not the lower, tail of the
# distribution when you calculate your critical F
# value?

# Answer: because the between-group variance is the
# numerator of the F statistic, and the within-group
# variance is the denominator of the F statistic.
# Because of this, if the between-group variance is
# significantly greater than the within-group variance,
# you'll be dealing with a ratio that's greater than
# one (remember, the between-group variance is the
# numerator), and thus you will be dealing with the
# upper, not the lower, tail.

# Let's look at this graph again:

matplot(x, cbind(F_1_1, F_1_5, F_5_1, F_5_5, F_5_25,
                 F_25_5, F_25_25, F_100_100),
        type = "l", lty = 1, col = 1:8,
        xlab = "Quantile (or F Value)", ylab = "Density",
        main = "F Distributions")
legend("topright", title = "Degrees of Freedom",
       legend = c("1, 1", "1, 5", "5, 1", "5, 5", "5, 25",
                  "25, 5", "25, 25", "100, 100"), col = 1:8,
       lty = 1)

# Let's zoom in on the turquoise and fuschia curves.

x <- seq(0, 7.5, 0.01)
F_5_25 <- df(x, 5, 25)
F_25_5 <- df(x, 25, 5)
matplot(x, cbind(F_5_25, F_25_5), type = "l", lty = 1,
        col = 5:6, xlab = "Quantile (or F Value)",
        ylab = "Density", main = "Comparing Two F Distributions")
legend("topright", title = "Degrees of Freedom",
       legend = c("5, 25", "25, 5"), col = 5:6, lty = 1)
abline(h = 0)

# The fuschia curve represents an F test where there
# were 6 groups and 26 observations in each group
# (hence the 5 and 25 degrees of freedon). Thus, we
# have lots of observations within each group, and our
# standard errors will be smaller. Therefore, group
# means don't have to be far apart to be significantly
# different.

# On the other hand, the turquoise curve represents
# an F test where there were 26 groups and 6
# observations within each group. Thus, we have fewer
# observations in each group, and our standard errors
# will be larger, meaning that group means will have
# to be relatively far apart to be significantly
# different.

# What does this mean in relation to the curves on the
# plot?

# Look at the upper tails. Notice that the fuschia
# curve is above the turquoise curve in the upper tail.
# The turquoise curve approaches zero much more quickly
# than the fuschia curve. Imagine finding the quantile,
# or F value, that delineates p = 0.025 in both of the
# upper tails. This F value would be greater for the
# fuschia curve.

polygon(x = c(qf(0.05, 5, 25, lower.tail = F),
              x[findInterval(qf(0.05, 5, 25, lower.tail = F),
                             x):length(x)], length(x)),
        y = c(0, F_5_25[findInterval(qf(0.05, 5, 25, lower.tail = F),
                                     x):length(F_5_25)], 0), col = 5,
        border = NA)
polygon(x = c(qf(0.05, 25, 5, lower.tail = F),
              x[findInterval(qf(0.05, 25, 5, lower.tail = F),
                             x):length(x)], length(x)),
        y = c(0, F_25_5[findInterval(qf(0.05, 25, 5, lower.tail = F),
                                     x):length(F_25_5)], 0), col = 6,
        border = NA)

# Therefore, when you have more observations in each
# group, it's easier to find significant differences
# between groups: the ratio of between-group variance
# to within-group variance must be

qf(0.05, 25, 5, lower.tail = F)

# 4.52 to confidently state that that there are
# differences between groups when you only have 6
# observations within each group and 26 groups, but the
# ratio of between-group variance to within-group
# variance only needs to be

qf(0.05, 5, 25, lower.tail = F)

# 2.60 to confidently state that that there are
# differences between groups when you have 26
# observations within each group and 6 groups.

# The chi-squared distribution

# Let's start with an example. We want to know
# if a 6-sided die is fair, so we roll it 96
# times. If it were fair, each side would, in
# theory, be rolled up 16 times. In practice,
# though, this doesn't always happen. Let's
# pretend that we rolled this die 96 times
# and got the following results:

Number <- 1:6
Expected <- rep(16, 6)
Observed_1 <- c(13, 10, 22, 18, 19, 14)
(Die_Data_1 <- data.frame(Number = Number,
                          Expected = Expected,
                          Observed = Observed_1))

# Based on the data we got, how sure are we that
# this die is fair? It seemed to preferentially
# result in a 3, a 4, and a 5.

# To answer this question, we will use a chi-
# square test.

# Let's first calculate the square of the
# differences between the observed and the
# expected values, relativized by the
# expected values.

Die_Data_1$Chi_Square <- with(Die_Data_1,
                              ((Observed - Expected) / Expected) ^ 2)

# Our chi-square test statistic is simply the
# sum of these values.

(Chi_Square_Test_Statistic_1 <- sum(Die_Data_1$Chi_Square))

# Let's look at the chi-square distribution for
# this test, which has 5 degrees of freedom.

x <- seq(0, 20, by = 0.1)
y <- dchisq(x, 5)
plot(x, y, type = "l",
     main = "Chi-Square Distribution\nfor 5 Degrees of Freedom",
     xlab = expression(paste("Quantile, or ", chi ^ 2)),
     ylab = "Density")
abline(h = 0)

# Let's plot the chi-square test statistic we got
# on this plot.

abline(v = Chi_Square_Test_Statistic_1, col = 2)

# Clearly, this line is nowhere near the upper
# tail of this distribution, so it is very
# unlikely that this die is not fair.

# Just to be totally clear, let's plot the
# region on this distribution that corresponds
# to a probability of 0.05 in the upper tail.

polygon(x = c(qchisq(0.05, 5, lower.tail = F),
              x[findInterval(qchisq(0.05, 5, lower.tail = F),
                             x):length(x)], length(x)),
        y = c(0, y[findInterval(qchisq(0.05, 5, lower.tail = F),
                                x):length(y)], 0), col = 3,
        border = NA)

# It's pretty obvious that our calculated chi-
# square test statistic does not fall in this
# shaded region, so we can confidently say that
# this die is fair. Our chi-square test statistic
# is very small, which means that our observed
# values were not very different from our
# expected values. In fact, there is a 0.4 %
# chance that this die isn't fair:

pchisq(Chi_Square_Test_Statistic_1, 5)

# To figure this out, I just calculated the area
# under this chi-square distribution to the left
# of our calculated chi-square test statistic.

# What if we ran this test and got very different
# results?

Number <- 1:6
Expected <- rep(16, 6)
Observed_2 <- c(3, 3, 4, 5, 2, 79)
(Die_Data_2 <- data.frame(Number = Number,
                          Expected = Expected,
                          Observed = Observed_2))

# Clearly, this die favors the number 6.

Die_Data_2$Chi_Square <- with(Die_Data_2,
                              ((Observed - Expected) / Expected) ^ 2)
(Chi_Square_Test_Statistic_2 <- sum(Die_Data_2$Chi_Square))

# Let's check out what this looks like graphically.

x <- seq(0, 20, by = 0.1)
y <- dchisq(x, 5)
plot(x, y, type = "l",
     main = "Chi-Square Distribution\nfor 5 Degrees of Freedom",
     xlab = "Quantile", ylab = "Density")
abline(h = 0)
abline(v = Chi_Square_Test_Statistic_2, col = 2)
polygon(x = c(qchisq(0.05, 5, lower.tail = F),
              x[findInterval(qchisq(0.05, 5, lower.tail = F),
                             x):length(x)], length(x)),
        y = c(0, y[findInterval(qchisq(0.05, 5, lower.tail = F),
                                x):length(y)], 0), col = 3,
        border = NA)

# We can confidently say that this die is not
# fair. The chi-square test statistic is large,
# meaning that observed and expected values are
# very different. In fact, there is a 99.8 % 
# percent chance that this die isn't fair:

pchisq(Chi_Square_Test_Statistic_2, 5)

# To figure this out, I just calculated the area
# under this chi-square distribution to the left
# of our calculated chi-square test statistic.

# If you haven't noticed yet, the same types of
# functions are available for this distribution.

dchisq(2, 9)
pchisq(10, 5, lower.tail = F)
qchisq(0.05, 11)
rchisq(20, 14)

# Contingency tables

# If we studied graduate student use of Dimond
# Library, we might have collected data that look
# like these data.

Never <- c(42, 76)
Once_a_Semester <- c(11, 31)
Once_a_Week <- c(22, 9)
Every_Day <- c(24, 3)
Contingency_Table <- data.frame(Never = Never,
                                Once_a_Semester = Once_a_Semester,
                                Once_a_Week = Once_a_Week,
                                Every_Day = Every_Day)
rownames(Contingency_Table) <- c("Masters", "Doctoral")
View(Contingency_Table)

# We can calculate row and column sums to look
# at library usage within groups and across
# groups.

Contingency_Table$Total <- rowSums(Contingency_Table)
Contingency_Table <- rbind(Contingency_Table,
                           colSums(Contingency_Table))
rownames(Contingency_Table)[nrow(Contingency_Table)] <- "Total"
View(Contingency_Table)

# You could imagine a more complicated
# contingency table that could be three-
# dimensional if we also broke the data down
# by which program or college these graduate
# students were in.

# For now, let's run a chi-square test to see
# if anything interesting is going on with this
# data.

# We have our observed values, but the next
# question is: what do we use for expected values?

# There's a neat trick we can do using row and
# column sums, and using the total number of
# participants, to generate expected values.
# Here's the formula:

# Expected value =
# (row sum * column sum) / total number of participants

# For example, the expected value for the first
# cell is:

Contingency_Table[nrow(Contingency_Table),
                  1] * Contingency_Table[1,
                                         ncol(Contingency_Table)] / Contingency_Table[nrow(Contingency_Table),
                                                                                      ncol(Contingency_Table)]

# We can generate all of our 'expected' values
# at once like this (after we first get rid of
# the column of row sums and the row of column
# sums - we need to only use the original
# contingency data that only contained observed
# values):

Contingency_Table <- as.matrix(Contingency_Table[seq_len(nrow(Contingency_Table) - 1),
                                                 seq_len(ncol(Contingency_Table) - 1)])
chisq.test(Contingency_Table)$expected

# This function will perform the chi-square
# test for us, too:

chisq.test(Contingency_Table)

# Why are there 3 degrees of freedom for this
# chi-square test? There are 8 cells, so
# shouldn't there be 7 degrees of freedom?

# We assign degrees of freedom by rows and by
# columns. Since there are 2 rows, there is 1
# degree of freedom for degree (masters or
# doctoral), and there are 3 degrees of freedom
# for library visit frequency. 3 times 1 equals
# 1; hence, this contingency table has 3 degrees
# of freedom.

# Going beyond

# There are many, many, many different distributions.

# Which one makes the most sense, conceptually, for
# your data?

par(mfrow = c(3, 1))

x <- seq(0, 4, 0.01)
Weibull_1 <- dweibull(x, shape = 1)
Weibull_2 <- dweibull(x, shape = 2)
Weibull_3 <- dweibull(x, shape = 3)
matplot(x, cbind(Weibull_1, Weibull_2, Weibull_3),
        main = "Weibull Distributions", type = "l",
        xlab = "Quantile", ylab = "Density")

x <- seq(0, 4, 0.01)
Lognormal <- dlnorm(x)
plot(x, Lognormal, type = "l",
     main = "Lognormal Distribution",
     xlab = "Quantile", ylab = "Density")

x <- seq(0, 4, 0.01)
Logistic <- dlogis(x)
plot(x, Logistic, type = "l",
     main = "Logistic Distribution",
     xlab = "Quantile", ylab = "Density")

par(mfrow = c(1, 1))
