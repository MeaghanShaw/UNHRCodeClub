
# Plotting With Base R Functions

View(iris)

# Pie charts
# Pie charts require data in a table format
# (this is essentially a named vector; names
# are groups and numeric vector elements are
# frequencies)
pie(table(iris$Species))
pie(iris$Species)
table(iris$Species)
str(table(iris$Species))
names(table(iris$Species))

vector_freq <- c(3, 5, 2, 7)
names(vector_freq) <- c("One", "Two", "Three", "Four")
pie(vector_freq)
?pie

# install.packages("MASS")
library(MASS)
str(Cars93)
pie(table(Cars93$Origin))
pie(table(Cars93$Manufacturer))
pie(table(Cars93$Manufacturer), cex = 0.4)

# Bar charts (these also require data in table format,
# just like pie charts)
barplot(table(Cars93$Manufacturer))
barplot(table(Cars93$Manufacturer), las = 2)
barplot(table(Cars93$Manufacturer), las = 2, cex.names = 0.5)

# Box plots
boxplot(iris$Sepal.Length ~ iris$Species)
# first argument is a formula (y ~ x)
boxplot(iris$Sepal.Length ~ iris$Species, main = "Iris spp. Sepal Lengths", ylab = "Sepal Length", xlab = "Species")

# Scatterplots
library (UKgrid)
help(UKgrid)
plot(ND ~ TIMESTAMP, UKgrid[1:100, ])

# Lines instead of points
plot(ND ~ TIMESTAMP, UKgrid[1:100, ], type = "l")

# Correlation matrices
pairs(iris[, 1:4])

# Scatterplots (or line plots) containing more than one curve

# To add points to a graph, use the 'points' function
A <- 1:100
B <- sin(A / 5) + rnorm(length(A), 0, 0.25)
C <- sin((A / 5) - 10) + rnorm(length(A), 0, 0.25)
D <- 2 * sin(A / 5) + rnorm(length(A), 0, 0.25)
plot(D ~ A, col = "red", pch = 1)
points(C ~ A, col = "green", pch = 1)
points(B ~ A, col = "blue", pch = 1)
legend("bottomright", legend = c("D", "C", "B"), col = c("red", "green", "blue"), pch = 1)

# To add lines to a graph, use the 'lines' function
plot(D ~ A, col = "red", type = "l", lty = 1)
points(C ~ A, col = "green", type = "l", lty = 1)
points(B ~ A, col = "blue", type = "l", lty = 1)
legend("topright", legend = c("D", "C", "B"), col = c("red", "green", "blue"), lty = 1, horiz = T)

# Alternatively, you can use 'matplot' to plot multiple
# curves on the same plot all at once
matplot(A, cbind(D, C, B), pch = 16, col = c("red", "green", "blue"), xlab = "Time", ylab = "Relative Response"); legend("bottomright", legend = c("D", "C", "B"), col = c("red", "green", "blue"), pch = 16)

# Legends

# We can alter the plotting region so that the legend
# doesn't cover any points
matplot(A, cbind(D, C, B), pch = 16, col = c("red", "green", "blue"), xlab = "Time", ylab = "Relative Response", ylim = c(-2.5, 3))
legend("top", legend = c("D", "C", "B"), col = c("red", "green", "blue"), pch = 16, horiz = T)

# Or, we can insert legends outside of the plotting region
# if we first set new plot margins
par(mar = c(9, 4, 4, 2), xpd = T)
matplot(A, cbind(D, C, B), pch = 16, col = c("red", "green", "blue"), xlab = "Time", ylab = "Relative Response")
legend(35, -4, legend = c("D", "C", "B"), col = c("red", "green", "blue"), pch = 16, horiz = T)

# Reset plotting parameters to their default values
dev.off()

# Putting more than one plot in a figure
par(mfrow = c(1, 3)) # We use the 'mfrow' argument
# to specify the number of rows and the number of
# columns
plot(iris$Sepal.Length ~ iris$Sepal.Width)
plot(iris$Sepal.Length ~ iris$Petal.Length)
plot(iris$Sepal.Length ~ iris$Petal.Width)
dev.off()

# Creating plots with a secondary y-axis

# Generate the data
Sap_Flow_Data <- read.table("D:/UNH PhD Research Stuff/NE SARE Project/Raw Data Files/Birch_2_CR1000_32_Heat_Ratio_Method_2019_04_19_17_55_16.dat", skip = 1, header = T, stringsAsFactors = F, sep = ",", na.strings = "NAN")
Sap_Flow_Data <- Sap_Flow_Data[-c(1:2), -c(40:67, 104:131)]
Sap_Flow_Data$TIMESTAMP <- as.POSIXct(Sap_Flow_Data$TIMESTAMP)
Sap_Flow_Data[, grep("Delta", colnames(Sap_Flow_Data))] <- lapply(Sap_Flow_Data[, grep("Delta", colnames(Sap_Flow_Data))], as.numeric)
Deltas <- Sap_Flow_Data[, grep("Delta60_100", colnames(Sap_Flow_Data))] - Sap_Flow_Data[, grep("Delta0", colnames(Sap_Flow_Data))]
colnames(Deltas) <- paste("Delta", 1:ncol(Deltas), sep = "_")
Upper_Thermocouples_Only <- rep(c(TRUE, FALSE), each = 3)
Ratios <- (Deltas[, Upper_Thermocouples_Only]) / (Deltas[, !Upper_Thermocouples_Only])
colnames(Ratios) <- paste("Ratio", 1:ncol(Ratios), sep = "_")
Heat_Pulse_Velocity_Function <- function(x) {((0.0019/0.5)*log(x))*3600}
Heat_Pulse_Velocities <- as.data.frame(lapply(Ratios, Heat_Pulse_Velocity_Function))
colnames(Heat_Pulse_Velocities) <- paste("Heat_Pulse_Velocity", 1:ncol(Heat_Pulse_Velocities), sep = "_")
Heat_Pulse_Velocities$TIMESTAMP <- Sap_Flow_Data$TIMESTAMP
Heat_Pulse_Velocities$Air_Temp <- Sap_Flow_Data$CR1000TmpC
str(Heat_Pulse_Velocities)

# Generate the plot
par(mar = c(5,5,2,5)) # The default value of
# the 'mar' argument is c(5, 4, 4, 2) + 0.1;
# we need to leave space on the right side of
# the plot so that we can add a secondary y-
# axis
with(Heat_Pulse_Velocities, plot(TIMESTAMP, Heat_Pulse_Velocity_1, col = "red", pch = 1, xlab = "Time", ylab = expression(paste("Heat Pulse Velocity, cm hr" ^ "-1"))))
par(new = T)
with(Heat_Pulse_Velocities, plot(TIMESTAMP, Air_Temp, col = "green", axes = F, xlab = NA, ylab = NA, pch = 2))
# When we add the data points corresponding to
# the secondary y-axis, we need to make sure not
# to add any y-axis information in this original
# plotting step, because by default, whatever we
# add will go on the left side of the graph and
# appear on top of the original axis.
axis(side = 4)
mtext(side = 4, line = 3, 'Air Temperature, \u00B0 C')
legend("bottomleft", legend=c("Heat Pulse Velocity", "Air Temperature"), pch = c(1, 2), col=c("red", "green"), bty = "n")
# Make sure in the legend that the data match up
# with the colors and the types of points (or
# lines) used

# Reset plotting parameters to their default values
dev.off()

# Make y-axis label superscripts visible

# Heat Pulse Velocity units = cm per hr
y_label <- expression(paste("Heat Pulse Velocity, cm hr" ^ "-1"))

with(Heat_Pulse_Velocities, plot(TIMESTAMP, Heat_Pulse_Velocity_1, col = "red", pch = 1, xlab = "Time", ylab = y_label))

# Method 1 - shifts the whole plot
par(mar = c(5, 5, 4, 2))
with(Heat_Pulse_Velocities, plot(TIMESTAMP, Heat_Pulse_Velocity_1, col = "red", pch = 1, xlab = "Time", ylab = y_label))
dev.off()

# Method 2 - keeps the plot intact; shifts just the y axis label
with(Heat_Pulse_Velocities, plot(TIMESTAMP, Heat_Pulse_Velocity_1, col = "red", pch = 1, xlab = "Time", ylab = ""))
title(ylab = y_label, line = 2.5)

# Interaction plots
View(npk)
interaction.plot(npk$P, npk$N, npk$yield)
interaction.plot(npk$N, npk$P, npk$yield)
# Switching the first two arguments in the previous
# line allows us to look at our data in different
# ways.
