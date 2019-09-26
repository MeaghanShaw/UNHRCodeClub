
# R Coding Club meeting - 9-26-2019

# Agenda:
# 1. One more example using the 'lapply' and 'mapply' functions
# 2. 'if' statements and the 'ifelse' function

# 1. A practical example using the 'lapply' and 'mapply' functions:

df1 <- data.frame(A = LETTERS[1:20], B = seq_len(20), C = ((1:20) / 2.1 + rnorm(20, 0.5)))
df2 <- data.frame(A = LETTERS[1:21], B = seq_len(21), C = ((1:21) / 2.2 + rnorm(21)))
df3 <- data.frame(A = LETTERS[1:23], B = seq_len(23), C = ((1:23) / 2.3 + rnorm(23, -0.5, 0.5)))
(list1 <- list(element1 = df1, element2 = df2, element3 = df3))
plot(list1$element1$C ~ list1$element1$B)
plot(list1$element2$C ~ list1$element2$B)
plot(list1$element3$C ~ list1$element3$B)

# We suspect there is a positive, linear relationship between
# B and C in all 3 data frames, and we also have reason to
# want to keep each data frame separate and not combine them
# for our regression analysis.

# We could do each of them individually by copying and pasting
# code, but that's pretty inefficient, especially if we have
# a lot of data frames.

# Instead, let's use the 'lapply' function to do this all in
# one step.

# First, we practice using one of the list elements:

practice <- list1$element1
practice
(linear_model <- lm(practice$C ~ practice$B))
linear_model$coefficients
coef(linear_model)
str(summary(linear_model))
summary(linear_model)$r.square
(Regression_Parameters <- c(
  coef(linear_model)[2], coef(linear_model)[1], summary(linear_model)$r.square
))
names(Regression_Parameters) <- c("Slope", "Intercept", "R_Squared")
Regression_Parameters

# Okay; I'm happy with that. Let's write a function that we can
# use on all of the list elements at once.

# We can do this by writing a function of one variable, x, and
# replacing all occurences of 'practice' with 'x'.

Extract_Regression_Parameters_fxn <- function (x) {
  linear_model <- lm(x$C ~ x$B)
  Regression_Parameters <- c(
    coef(linear_model)[2], coef(linear_model)[1], summary(linear_model)$r.square
  )
  names(Regression_Parameters) <- c("Slope", "Intercept", "R_Squared")
  return (Regression_Parameters)
}

# Now, let's use it on our list.

lapply(list1, Extract_Regression_Parameters_fxn)

# Great - that worked. Let's save it as it's own list.

Regression_Parameter_List <- lapply(list1, Extract_Regression_Parameters_fxn)

# Now, we can generate predicted values for each predictor
# variable ('B') in each of our original data frames.

# We'll eventually use the 'mapply' function to do this to
# each list element all at once, but for now, let's practice
# again using the first element of each list.

practice1 <- list1$element1
practice2 <- Regression_Parameter_List$element1
str(practice2)
(Predicted_Values <- practice1$B * practice2["Slope"] + practice2["Intercept"])
practice1$B
practice2["Slope"]

# In the above code, column 'B' in the 'practice1' data frame
# had several elements, whereas 'Slope' and 'Intercept' in
# the 'practice2' vector each only had one element. Therefore,
# 'Slope' and 'Intercept' were recycled until the longer
# vector, column 'B' in the 'practice1' data frame, was
# used up.

# To convert the above to an 'mapply'-friendly format, we'll
# have to write a function of two variables. One of the
# variables will correspond with our 'list1', and the other
# will correspond with our 'Regression_Parameter_List'.

# The 'm' in 'mapply' stands for 'multivariate'.

Predicted_Values_fxn <- function (x, y) {
  x$PredVal <- x$B * y["Slope"] + y["Intercept"]
  return (x)
}

# In this case, 'x' corresponds with elements from 'list1',
# and 'y' corresponds with elements from
# 'Regression_Parameter_List'.

# Now we can use 'mapply':

mapply(Predicted_Values_fxn, x = list1, y = Regression_Parameter_List, SIMPLIFY = F)
Map(Predicted_Values_fxn, x = list1, y = Regression_Parameter_List)

# (The 'Map' function does the exact same thing as 'mapply', but
# the SIMPLIFY argument is always (by default) set to FALSE.)

# I think that worked.

Predicted_Values_list <- mapply(Predicted_Values_fxn, x = list1, y = Regression_Parameter_List, SIMPLIFY = F)
lapply(Predicted_Values_list, diff)

# As our last step, we can actually add a column in each of
# our original data frames that contains predicted values.
# There are two ways we can do this.

# Way 1:

mapply(function(x, y) {
  x$Predicted_Value <- y; return (x)
}, x = list1, y = Predicted_Values_list, SIMPLIFY = F)

# Way 2:

mapply(function (x, y) {
  x$Predicted_Value <- x$B * y["Slope"] + y["Intercept"]; return (x)
}, x = list1, y = Regression_Parameter_List, SIMPLIFY = F)

# Note: instead of writing out 'return (x)', we can simply
# just write 'x' instead:

(Updated_list <- mapply(function(x, y) {
  x$Predicted_Value <- y
  x
}, x = list1, y = Predicted_Values_list, SIMPLIFY = F))

# Note: we could also have used R's 'predict' function to do
# this:

practice <- list1$element1
(linear_model <- lm(practice$C ~ practice$B))
predict(linear_model)

# 2. 'if' statements and the 'ifelse' function

x <- 5
if (x == 5) {"x equals 5"}
x <- 4
if (x == 5) {"x equals 5"}
if (x == 5) {"x equals 5"} else {"x does not equal 5"}
x <- 1:10
if (x > 6) {"x is greater then 6"} else {"x is less than or equal to 6"}

# 'if' statements don't work with vectors.

?ifelse
ifelse(x > 6, "x is greater than 6", "x is less than or equal to 6")

# We can nest 'ifelse' functions
ifelse(x > 6, "x is greater than 6", ifelse(
  x > 2, "x is greater than 2 but not greater than 6", "x is less than or equal to 2"
))

# the 'cut' function can be used to do this same thing

# Let's use the 'ifelse' function to tell us more information
# about our predicted values from the previous example

Updated_list

# We want to know if the predicted value is greater than or less
# than the observed value (for whatever reason).

# Practicing on one list element, we have:

practice <- Updated_list$element1
ifelse(practice$C > practice$Predicted_Value,
       "The observed value is greater than the predicted value",
       "The observed value is less than the predicted value"
)
practice$ObservedVersusPredicted <- ifelse(practice$C > practice$Predicted_Value, "The observed value is greater than the predicted value", "The observed value is less than the predicted value")
View(practice)

# That worked. Now, let's scale up and apply this to each list
# element.

ObservedVersusPredicted_fxn <- function (x) {
  x$ObservedVersusPredicted <- ifelse(x$C > x$Predicted_Value, "The observed value is greater than the predicted value", "The observed value is less than the predicted value")
  return (x)
}
(Updated_list <- lapply(Updated_list, ObservedVersusPredicted_fxn))

# Let's take a look at one of the list elements to see if it
# worked.

View(Updated_list$element2)

# We could have also done this using logical vectors instead
# of character vectors:

practice$ObservedGreaterThanPredicted <- ifelse(practice$C > practice$Predicted_Value, T, F)
View(practice)

# An even shorter method:

practice$ObservedGreaterThanPredicted <- practice$C > practice$Predicted_Value
View(practice)

# Exercise: take either of the above methods of generating
# logical (not character) vectors for the
# 'ObservedGreaterThanPredicted' column and scale it up
# using the 'lapply' function so that you obtain a
# 'ObservedGreaterThanPredicted' column in each list
# element in the 'Updated_list' list.
