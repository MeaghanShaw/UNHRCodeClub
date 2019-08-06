
# Curve Smoothing
# By David Moore
# 8-5-2019

my_data <- data.frame(predictor = seq_len(500), response = (50 * sin(1:500 / 25 + 25) + rnorm(100, 5, 20)))
plot(my_data$response ~ my_data$predictor, main = "My Data", ylab = "Response", xlab = "Predictor")

########## Local Polynomial Regression ##########

# Since we are performing regression analysis and not calculating moving averages,
# equally-spaced observations are not a requirement.

# The 'span' argument is used to set the degree of smoothing. A higher 'span' value
# means a smoother curve. 'span' can be between 0 and 1.

?loess

span_0.9 <- loess(my_data$response ~ my_data$predictor, span = 0.9)
span_0.75 <- loess(my_data$response ~ my_data$predictor)
span_0.5 <- loess(my_data$response ~ my_data$predictor, span = 0.5)
span_0.25 <- loess(my_data$response ~ my_data$predictor, span = 0.25)
span_0.1 <- loess(my_data$response ~ my_data$predictor, span = 0.1)
span_0.05 <- loess(my_data$response ~ my_data$predictor, span = 0.05)
span_0.025 <- loess(my_data$response ~ my_data$predictor, span = 0.025)

# The default for this function is to use 2nd-degree polynomials, but you can
# specify otherwise using the 'degree = ' argument.

plot(my_data$response ~ my_data$predictor, main = "Localized Polynomial Regression", ylab = "Response", xlab = "Predictor")

lines(span_0.9$fitted ~ span_0.9$x, col = "yellow")
lines(span_0.75$fitted ~ span_0.75$x, col = "red")
lines(span_0.5$fitted ~ span_0.5$x, col = "blue")
lines(span_0.25$fitted ~ span_0.25$x, col = "green")
lines(span_0.1$fitted ~ span_0.1$x, col = "orange")
lines(span_0.05$fitted ~ span_0.05$x, col = "purple")

########## Locally-Weighted Polynomial Regression ##########

# Since we are performing regression analysis and not calculating moving averages,
# equally-spaced observations are not a requirement

span_0.9 <- lowess(my_data$response ~ my_data$predictor, f = 0.9)
span_0.67 <- lowess(my_data$response ~ my_data$predictor)
span_0.5 <- lowess(my_data$response ~ my_data$predictor, f = 0.5)
span_0.25 <- lowess(my_data$response ~ my_data$predictor, f = 0.25)
span_0.1 <- lowess(my_data$response ~ my_data$predictor, f = 0.1)
span_0.05 <- lowess(my_data$response ~ my_data$predictor, f = 0.05)

plot(my_data$response ~ my_data$predictor, main = "Locally-Weighted Polynomial Regression", ylab = "Response", xlab = "Predictor")

lines(span_0.9$y ~ span_0.9$x, col = "yellow")
lines(span_0.67$y ~ span_0.67$x, col = "red")
lines(span_0.5$y ~ span_0.5$x, col = "blue")
lines(span_0.25$y ~ span_0.25$x, col = "green")
lines(span_0.1$y ~ span_0.1$x, col = "orange")
lines(span_0.05$y ~ span_0.05$x, col = "purple")

########## Smoothing Splines ##########

# Since we are performing regression analysis and not calculating moving averages,
# equally-spaced observations are not a requirement.

# This function uses cubic curves to fit the data.

# This function works in such a way that the first and second derivatives ot the
# fitted curves are always defined. This could be important if you have to deal
# with derivatives or slopes of your fitted curves.

# The 'spar' argument can be between 0 and 1 and is the smoothing parameter.

spar_0.9 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.9)
spar_0.75 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.75)
spar_0.5 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.5)
spar_0.25 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.25)
spar_0.1 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.1)
spar_0.05 <- smooth.spline(my_data$response ~ my_data$predictor, spar = 0.05)

plot(my_data$response ~ my_data$predictor, main = "Smoothing Splines", ylab = "Response", xlab = "Predictor")

lines(spar_0.9$y ~ spar_0.9$x, col = "yellow")
lines(spar_0.75$y ~ spar_0.75$x, col = "red")
lines(spar_0.5$y ~ spar_0.5$x, col = "blue")
lines(spar_0.25$y ~ spar_0.25$x, col = "green")
lines(spar_0.1$y ~ spar_0.1$x, col = "orange")
lines(spar_0.05$y ~ spar_0.05$x, col = "purple")

########## Kernel Smoothing ##########

# Kernel smoothing requires equally-spaced data points.

bandwidth_500 <- ksmooth(my_data$predictor, my_data$response, bandwidth = 500)
bandwidth_100 <- ksmooth(my_data$predictor, my_data$response, bandwidth = 100)
bandwidth_10 <- ksmooth(my_data$predictor, my_data$response, bandwidth = 10)
bandwidth_5 <- ksmooth(my_data$predictor, my_data$response, bandwidth = 5)
bandwidth_2 <- ksmooth(my_data$predictor, my_data$response, bandwidth = 2)

plot(my_data$response ~ my_data$predictor, main = "Kernel Smoothing", ylab = "Response", xlab = "Predictor")

lines(bandwidth_500$y ~ bandwidth_500$x, col = "yellow")
lines(bandwidth_100$y ~ bandwidth_100$x, col = "red")
lines(bandwidth_10$y ~ bandwidth_10$x, col = "blue")
lines(bandwidth_5$y ~ bandwidth_5$x, col = "green")
lines(bandwidth_2$y ~ bandwidth_2$x, col = "orange")

########## Simple Moving Average (MA) ##########

# R doesn't have a moving average function, but the 'filter' function
# can be used instead.

ma <- function(x, n = 5) {stats::filter(x, rep(1 / n, n), sides = 2)}

# See below for a quick tutorial on how the 'filter' function works.

?stats::filter
x <- 1:100
stats::filter(x, rep(1, 3))
stats::filter(x, rep(1, 3), sides = 1)
stats::filter(x, rep(1, 3), sides = 1, circular = TRUE)
data.frame(x = 1:25, y = stats::filter(1:25, rep(1 / 5, 5)))

window_5 <- ma(my_data$response, 5)
window_13 <- ma(my_data$response, 13)
window_25 <- ma(my_data$response, 25)
window_75 <- ma(my_data$response, 75)
window_151 <- ma(my_data$response, 151)

plot(my_data$response ~ my_data$predictor, main = "Moving Average", ylab = "Response", xlab = "Predictor")

lines(window_5 ~ my_data$predictor, col = "yellow")
lines(window_13 ~ my_data$predictor, col = "red")
lines(window_25 ~ my_data$predictor, col = "blue")
lines(window_75 ~ my_data$predictor, col = "green")
lines(window_151 ~ my_data$predictor, col = "orange")

# Notice that endpoints are missing, and notice that the larger your window is,
# the smoother the curve is. Endpoints are missing because moving average
# windows are centered on the value (we set this using the 'sides = 2' argument
# in the 'filter' function), and therefore the larger the window, the more
# missing endpoint values will be returned.

########## Auto_Regressive Integrated Moving Average (ARIMA) ##########

# ARIMA models can use moving averages, auto-regression slopes, and differences
# (if data are not stationary).

# ARIMA models require equally-spaced data.

# The 'auto.arima' function will calculate the optimal parameters for auto-regression,
# differencing, and moving averages (note that all three of these are not always used).

ARIMA_model_auto <- auto.arima(my_data$response)
summary(ARIMA_model_auto) # This algorithm found that a fifth-order auto-regressive model
# and a twelfth-order moving average model were optimal (note that this time series is
# stationary, and thus the optimal solution did not include differencing).

# You can also try out other models using the 'arima' function
ARIMA_model_6_0_5 <- arima(my_data$response, order = c(6L, 0L, 5L))
ARIMA_model_5_0_9 <- arima(my_data$response, order = c(5L, 0L, 9L))
my_data$response + ARIMA_model_6_0_5$residuals

plot(my_data$response ~ my_data$predictor, main = "Auto-Regressive Integrated Moving Average", ylab = "Response", xlab = "Predictor")

points(ARIMA_model_auto$fitted ~ my_data$predictor, col = "yellow")
points(my_data$response - ARIMA_model_6_0_5$residuals ~ my_data$predictor, col = "red")
points(my_data$response - ARIMA_model_5_0_9$residuals ~ my_data$predictor, col = "blue")

# See below for an ARIMA example with non-stationary data.

my_data$non_stationary <- my_data$response + my_data$predictor ^ 1.05
plot(non_stationary ~ predictor, data = my_data)

(ARIMA_non_stationary_model <- auto.arima(my_data$non_stationary)) # Since these
# data are not stationary, a differencing term was included in the optimal model
plot(diff(my_data$non_stationary))
plot(my_data$non_stationary)
points(ARIMA_non_stationary_model$fitted ~ my_data$predictor, col = "yellow")

########## An example of using curve fitting for real-world data problems ##########

# Download the data from GitHub and import it into R
Heat_Pulse_Velocity_Data <- read.csv("Heat Pulse Velocity Data.csv")
Heat_Pulse_Velocity_Data <- Heat_Pulse_Velocity_List$Sycamore_1
# I have sap flow data for which I need to re-set the baseline (i.e., zero flow)
# to zero. As of right now, zero-flow values do not occur at y = 0.

Heat_Pulse_Velocity_Data <- Heat_Pulse_Velocity_Data[is.finite(Heat_Pulse_Velocity_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1), ]
plot(Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Record_Number, data = Heat_Pulse_Velocity_Data)
abline(h = 0, col = "red")

# First, I will subset the data so that only rows containing times of zero-flow are
# included.

library (lubridate)
Heat_Pulse_Velocity_Data$Zero_Flow <- hour(Heat_Pulse_Velocity_Data$Time) == 3
Zero_Flow_Data <- Heat_Pulse_Velocity_Data[which(Heat_Pulse_Velocity_Data$Zero_Flow == T), ]

# Here is a plot of only zero-flow values. They are close to, but not exactly centered on, y = 0.

plot(Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Record_Number, data = Zero_Flow_Data)

# Since these data are not equally-spaced, let's use the 'loess' function to fit
# a smooth curve through them. Let's test out several models to see which one
# is the best fit for the data.

loess_model_1 <- loess(Zero_Flow_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Zero_Flow_Data$Record_Number)
lines(loess_model_1$fitted ~ loess_model_1$x[, 1], col = "yellow")
loess_model_2 <- loess(Zero_Flow_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Zero_Flow_Data$Record_Number, span = 0.5)
lines(loess_model_2$fitted ~ loess_model_2$x[, 1], col = "red")
loess_model_3 <- loess(Zero_Flow_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Zero_Flow_Data$Record_Number, span = 0.25)
lines(loess_model_3$fitted ~ loess_model_3$x[, 1], col = "blue")
loess_model_4 <- loess(Zero_Flow_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Zero_Flow_Data$Record_Number, span = 0.1)
lines(loess_model_4$fitted ~ loess_model_4$x[, 1], col = "green")

# Something between the blue (span = 0.25) and the green (span = 0.1) curve
# would probably be ideal. Let's try span = 0.2.

loess_model_5 <- loess(Zero_Flow_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Zero_Flow_Data$Record_Number, span = 0.2)
lines(loess_model_5$fitted ~ loess_model_5$x[, 1], col = "orange")

# Now that we have the best model selected, we can generate predicted values for
# all sap flow data points and then subtract these predicted values from the
# actual values to reset the baseline (zero-flow) values to y = 0.

Heat_Pulse_Velocity_Data$Predicted_Zero_Flow <- predict(loess_model_5, Heat_Pulse_Velocity_Data$Record_Number)
Heat_Pulse_Velocity_Data$Corrected_Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 <- Heat_Pulse_Velocity_Data$Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 - Heat_Pulse_Velocity_Data$Predicted_Zero_Flow
with(Heat_Pulse_Velocity_Data, plot(Corrected_Sensor_1_Outer_Thermocouple_Multiplexer_Port_1 ~ Record_Number))
abline(h = 0, col = "red")
