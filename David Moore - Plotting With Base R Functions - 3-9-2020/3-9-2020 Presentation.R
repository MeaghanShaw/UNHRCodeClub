
# Plotting With Base R Functions
# 3-9-2020

# Tables

# Tables show frequencies (i.e., numbers of
# observations).

View(iris)
table(iris$Species)

# Pie charts

# Pie charts require data in a table format.
# Tables are essentially named vectors where
# names are groups and numeric vector elements
# are frequencies.

pie(table(iris$Species))

# If you try to create a pie chart without
# starting with a table, you won't get too
# far.

pie(iris$Species)

# Again, a table in R is essentially just a
# named vector (or a named matrix if it's 2-
# dimensional, or a named array if it's 3-
# dimensional).

names(table(iris$Species))
vector_1 <- c(3, 5, 2, 7)
names(vector_1) <- c("One", "Two", "Three", "Four")
pie(vector_1)

# Here's an example of a 2-dimensional
# table.

table(mtcars$am, mtcars$cyl)

# Here's another example of a pie chart.

library (MASS)
View(Cars93)
pie(table(Cars93$Origin))
pie(table(Cars93$Manufacturer))

# If we need to make and adjustment to a graph
# to make it more readable, there's usually an
# argument that we can use to help accomplish
# that.

?par

# The 'cex', or character expansion, argument
# will help us make this chart more readable.

pie(table(Cars93$Manufacturer), cex = 0.4)

# Bar charts

# Bar charts also require data in table format,
# just like pie charts.

barplot(table(Cars93$Manufacturer))

# Let's use the 'las' argument to rotate the
# x-axis labels.

barplot(table(Cars93$Manufacturer), las = 2)

# Let's use the 'cex.names' argument to shrink
# these labels so they can all fit in.

barplot(table(Cars93$Manufacturer), las = 2, cex.names = 0.5)

# We have two ways of inputting data into the
# 'plot()' function. The first is by providing
# the predictor variable as the first argument
# and providing the response variable as the
# second argument.

?plot
plot(mtcars$mpg, mtcars$disp)

# Both the 'plot()' and 'lm()' functions take
# a formula as their first argument too. This
# formula replaces the two arguments from the
# previous example.

# Formulas in R

# The general syntax of a formula is:

# response variable ~ predictor variable

# (This is the opposite order from the former
# example, where we first provided the predictor
# and then we provided the response).

plot(mtcars$disp ~ mtcars$mpg)
lm(mtcars$disp ~ mtcars$mpg)

# Formulas can get complicated when there are
# more than one predictor variable or more than
# one response variable. We'll talk more about
# that some other time.

# Box plots

# Box plots, like most types of plots in R,
# require a formula (not a table) as an input.

boxplot(iris$Sepal.Length ~ iris$Species)

# We can make this plot prettier by adding
# a main title and axis labels.

boxplot(iris$Sepal.Length ~ iris$Species,
        main = "Iris spp. Sepal Lengths",
        ylab = "Sepal Length",
        xlab = "Species")

# Scatterplots

library (UKgrid)
help(UKgrid)

# Here I'll show you another option for
# inputting formulas. When both the predictor
# and the response variables are found in the
# same data frame, you can use the 'data'
# argument to specify the data frame they
# come from. If you do this, you won't have to
# type out the data frame name each time you
# list a variable.

# Here, we'll just plot the first 100 rows.

plot(ND ~ TIMESTAMP, data = UKgrid[1:100, ])

# If we want to plot the data as a line instead
# of as points, we can use the 'type' argument.

plot(ND ~ TIMESTAMP, UKgrid[1:100, ], type = "l")

# Scatterplots (or line plots) containing more
# than one curve

# To add points to a graph, use the 'points'
# function.

A <- 1:100
B <- sin(A / 5) + rnorm(length(A), 0, 0.25)
C <- sin((A / 5) - 10) + rnorm(length(A), 0, 0.25)
D <- 2 * sin(A / 5) + rnorm(length(A), 0, 0.25)
plot(D ~ A, col = "red", pch = 1)
points(C ~ A, col = "green", pch = 1)
points(B ~ A, col = "blue", pch = 1)
legend("bottomright",
       legend = c("D", "C", "B"),
       col = c("red", "green", "blue"),
       pch = 1)

# To add lines to a graph, use the 'lines'
# function.

plot(D ~ A, col = "red", type = "l", lty = 1)
lines(C ~ A, col = "green", type = "l", lty = 1)
lines(B ~ A, col = "blue", type = "l", lty = 1)
legend("topright",
       legend = c("D", "C", "B"),
       col = c("red", "green", "blue"),
       lty = 1,
       horiz = T)

# Alternatively, you can use the 'matplot',
# or matrix plot, function to plot multiple
# curves on the same plot all at once.

matplot(A, cbind(D, C, B), pch = 16,
        col = c("red", "green", "blue"),
        xlab = "Time", ylab = "Relative Response")
legend(40, -1, legend = c("D", "C", "B"),
       col = c("red", "green", "blue"), pch = 16,
       horiz = T)

# Legends

# We can alter the plotting region so that
# the legend doesn't cover any points. Let's
# specify new y-axis limits using the 'ylim'
# argument.

matplot(A, cbind(D, C, B), pch = 16,
        col = c("red", "green", "blue"),
        xlab = "Time",
        ylab = "Relative Response",
        ylim = c(-2.5, 3.5))
legend("top", legend = c("D", "C", "B"),
       col = c("red", "green", "blue"),
       pch = 16, horiz = T)

# Plot margins

?par

# We could also insert legends outside of
# the plotting region if we first set new
# plot margins.

par(mar = c(9, 4, 4, 2), xpd = T)
matplot(A, cbind(D, C, B), pch = 16,
        col = c("red", "green", "blue"),
        xlab = "Time",
        ylab = "Relative Response")
legend(37.5, -5, legend = c("D", "C", "B"),
       col = c("red", "green", "blue"),
       pch = 16, horiz = T)

# There are two ways of resetting plotting
# parameters to their default values.

# You can manually reset them,

par(mar = c(5, 4, 4, 2) + 0.1)

# or you can reset all of the parameters
# to their default values all at once.

dev.off()

# Putting more than one plot in a figure

# We use the 'mfrow' argument
# to specify the number of rows and the
# number of columns.

par(mfrow = c(1, 3))
plot(iris$Sepal.Length ~ iris$Sepal.Width)
plot(iris$Sepal.Length ~ iris$Petal.Length)
plot(iris$Sepal.Length ~ iris$Petal.Width)
par(mfrow = c(1, 1))

# Playing with axis labels

# What if we need a Greek symbol, like omega,
# in our axis label?

plot(iris$Sepal.Length ~ iris$Petal.Length,
     ylab = "Current (A)",
     xlab = expression(paste("Ohms (", Omega, ")")))

# What if we need a superscript in our axis
# labels?

plot(iris$Sepal.Length ~ iris$Petal.Length,
     ylab = expression(paste("Velocity (cm hr" ^ "-1" * ")")))

# Uh-oh. We can't see the superscript. Here's
# the fix. Let's first create the plot without
# a y-axis label.

plot(iris$Sepal.Length ~ iris$Petal.Length, ylab = "")

# Let's now add it in, and we'll specify that
# we want it to be a little closer to the plot
# and farther from the edge of the figure.

title(ylab = expression(paste("Velocity (cm hr" ^ "-1" * ")")),
      line = 2.5)

# How do we know what the plot margins are?

par('mar')

# How do we know what the x- and y-limits of
# the plot are?

par('usr')

# The first element is the lower bound of the
# x-limits; the second element is the upper
# bound of the x-limits; the third element is
# the lower bound of the y-limits; the fourth
# element is the upper bound of the y-limits.

# We can use this information to add text to
# a plot if we want. Let's put some text in
# the exact center of the plot. The x-
# coordinate of the plot's center is the
# average of the lower and upper bounds of
# the x-limits, and the y-coordinate of the
# plot's center is the average of the lower
# and upper bounds of the y-limits.

text(mean(c(par('usr')[1], par('usr')[2])),
     mean(c(par('usr')[3], par('usr')[4])),
     "Plot\nCenter",
     col = "red")

# Creating plots with a secondary y-axis

data_frame_1 <- data.frame(Time = 1:50,
                  Temperature = (sin(1:50 / pi) + rnorm(50, 60, 0.25)),
                  Sap_Flow = (sin(1:50 / pi) + rnorm(50, 1, 0.25)))
par(mfrow = c(2, 1))
plot(Temperature ~ Time, data_frame_1)
plot(Sap_Flow ~ Time, data_frame_1)
dev.off()

# Generate the plot

# The default value of the 'mar' argument
# is c(5, 4, 4, 2) + 0.1; we need to
# leave space on the right side of the
# plot so that we can add a secondary y-
# axis.

# The first element represents how far from
# the bottom margin the plot should start;
# the second element represents how far from
# the left margin the plot should start; the
# third element represents how far from the
# top margin the plot should start; the fourth
# element represents how far from the right
# margin the plot should start.

par(mar = c(5,5,2,5) + 0.1)
with(data_frame_1, plot(Sap_Flow ~ Time, col = "red",
               pch = 1, xlab = "Time",
               ylab = expression(paste("Heat Pulse Velocity, cm hr" ^ "-1"))))
par(new = T)
with(data_frame_1, plot(Temperature ~ Time,
               col = "green", axes = F,
               xlab = NA, ylab = NA, pch = 2))

# When we add the data points corresponding to
# the secondary y-axis, we need to make sure not
# to add any y-axis information in this original
# plotting step, because by default, whatever we
# add will go on the left side of the graph and
# appear on top of the original axis.

axis(side = 4)
mtext(side = 4, line = 3,
      'Air Temperature, \u00B0 C')
legend("bottomleft",
       legend = c("Heat Pulse Velocity",
                  "Air Temperature"),
       pch = c(1, 2), col=c("red", "green"),
       bty = "n")

# Make sure in the legend that the data match up
# with the colors and the types of points (or
# lines) used.

# Reset plotting parameters to their default
# values.

dev.off()

# Interaction plots

View(npk)
interaction.plot(npk$P, npk$N, npk$yield)
interaction.plot(npk$N, npk$P, npk$yield)

# Switching the first two arguments in the
# previous line allows us to look at our
# data in different ways.

# Correlation matrices

pairs(iris[, 1:4])

# Polygons

plot(1, type = "n", xlab = "", ylab = "",
     xlim = c(0, 10), ylim = c(0, 10))

# We can draw shapes whose vertices
# are at particular x- and y-coordinates.

polygon(c(2, 4, 6), c(5, 5, 7))

# This is a triangle with vertices at the
# points (2, 5), (4, 5), and (6, 7).

# Here is a more advanced plot using
# polygons.

data_frame_2 <- data.frame(A = 1:8,
                           B = rnorm(8, 20, 2))
plot(B ~ A, data_frame_2, type = "n", xlab = "",
     ylab = "",
     ylim = c(0, max(data_frame_2$B) + 10))
for (i in seq_len(nrow(data_frame_2) - 1)) {
  polygon(rep(data_frame_2$A[i:(i + 1)], each = 2),
          c(0, data_frame_2$B[i:(i + 1)], 0),
          col = i)
}
legend("top", legend = LETTERS[seq_len(nrow(data_frame_2) - 1)],
       col = seq_len(nrow(data_frame_2) - 1), pch = 16, horiz = T)

# Layout matrices

# We can construct more complex
# arrangements of graphs using the
# 'layout' function.

layout(matrix(c(1, 2, 3,
                1, 2, 3,
                4, 4, 4), nrow = 3, byrow = T))
with(mtcars, plot(mpg ~ cyl))
with(mtcars, plot(mpg ~ disp))
with(mtcars, plot(mpg ~ am))
with(mtcars, plot(mpg ~ hp))
dev.off()
