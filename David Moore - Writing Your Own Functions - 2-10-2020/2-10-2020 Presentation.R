
# R Coding Club Meeting
# 2/10/2020

# Long Data Versus Wide Data

# There are advantages and disadvantages to both;
# often, the type of data frame you should use
# depends on the type of analysis you're doing
# and the functions you're using to do the
# analysis.

View(Indometh)

# Remember - there are two ways to get help:
help(Indometh)
?Indometh

# 'Celebration of Knowledge' Question # 1:
# How does one find out what subject 2's
# concentration of indometacin ('conc') was
# at time = 1.25?

# (Answer below.)

















# Method # 1: using 'which()' and '&'
Indometh$Subject == 2
Indometh$time == 1.25
Indometh$Subject == 2 & Indometh$time == 1.25
which(Indometh$Subject == 2 & Indometh$time == 1.25)
Indometh$conc[which(Indometh$Subject == 2 & Indometh$time == 1.25)]

# Method # 2: using 'which()' and 'intersect()'
which(Indometh$Subject == 2)
which(Indometh$time == 1.25)
intersect(which(Indometh$Subject == 2), which(Indometh$time == 1.25))
Indometh$conc[intersect(which(Indometh$Subject == 2), which(Indometh$time == 1.25))]

# Method # 3: using 'with()'
with(Indometh, conc[which(Subject == 2 & time == 1.25)])
# Using 'with()', you don't have to type out
# 'Indometh$' every time

# Long Data Versus Wide Data
Indometh
?reshape
wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time", direction = "wide")
View(wide)

reshape(wide, idvar = "Subject", varying = list(2:12),
        v.names = "conc", direction = "long")

# Writing Your Own Functions

# I want to write a function that takes a number,
# squares it, and adds 1.

# First, I figure out how to perform the function
# I want on one specific thing (in this case, the
# specific thing I'm using is 'object1', which
# represents the number 3).

object1 <- 3
object1 ^ 2 + 1

# Now that I've figured out how to do that, I
# copy and paste that code into the body of my
# function (i.e., that which is between the
# curly brackets), making sure that the name of
# the variable that is the argument to the
# function matches up with the name of the
# variable that is inside the body of the
# function.

fxn1 <- function (x) {x ^ 2 + 1}

fxn1(7)

# This function also works on vectors.
fxn1(1:10)

# I want to write a function that takes a name
# (i.e., a character string), and adds
# ", I hope you have a good day!" to the end.

# I'll use the same process as before: I'll
# try to write code that does what I want on
# a single object, and then I'll insert this
# code into the body of the function once I'm
# happy with it.

object2 <- "David"
paste(object2, ", I hope you have a good day!", sep = "")

fxn2 <- function (var1) {
  paste(var1, ", I hope you have a good day!", sep = "")
}

fxn2("Roger")

# I want to write a function that multiplies a
# number by 3 in the first step, squares the
# result in a second step, and adds 7 in a final
# step.

w <- 2 * 3
x <- w ^ 2
x + 7

fxn3 <- function (var2) {
  output1 <- var2 * 3
  output2 <- output1 ^ 2
  output2 + 7
}

fxn3(2)

# Functions always return the last thing you
# ask for unless otherwise specified.

# I want to write a function that takes two
# distinct numbers as the inputs, squares the
# first one, and adds the second one to that.

object1 <- 3
object2 <- 2
object1 ^ 2 + object2

fxn4 <- function (x, y) {x ^ 2 + y}

fxn4(3, 2)

# You can provide arguments to functions
# out-of-order if you name the arguments
# inside the function.
fxn4(y = 2, x = 3)

# Celebration of Knowledge # 2:
# I want to create a function of three variables.
# I want this function to be written in 3
# separate steps.

# On the first line, the first argument is doubled.

# On the second line, the second argument is added
# to the result from the first line.

# On the third line, the result of the second line
# is raised to the power of the third argument.

# I want to return only the result of the third line.

# (Answer below.)

















x <- 2
y <- 5
z <- 3
object1 <- x * 2
object2 <- object1 + y
object2 ^ z

fxn5 <- function (A, B, C) {
  var1 <- A * 2
  var2 <- var1 + B
  var2 ^ C
}

fxn5(2, 5, 3)

# I want to write a function that squares a
# number and then pastes the result into a
# character string that reads, "[output number]
# is the square of [input number]".

object1 <- 4
object2 <- object1 ^ 2
paste(object2, "is the square of", object1)

# Option # 1
fxn6 <- function (var1) {
  var2 <- var1 ^ 2
  paste(var2, "is the square of", var1)
}

# Option # 2: you can do this in one line of
# code if you nest the squaring operation
# inside of the paste argument.
fxn6 <- function (var1) {
  paste(var1 ^ 2, "is the square of", var1)
}

# Option # 3: you can also write this entire
# function on one line of code if you separate
# what would normally be separate lines of code
# with semicolons.
fxn6 <- function (var1) {output <- var1 ^ 2; paste(output, "is the square of", var1)}

fxn6(6)

# I want to write a function that calculates the
# carbon-to-nitrogen ratio for any data frame that
# has a column named 'carbon_mg_per_kg' containing
# carbon concentration and a column named
# 'nitrogen_mg_kg' containing nitrogen concentration.

# Functions can be written to take any type of R
# object as an input - you aren't limited to just
# scalars and vectors. In this case, a data frame
# is the input to this function.

Year1Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                         Site = rep(c("Forest", "Pasture", "Corn Field"), each = 3),
                         carbon_mg_per_kg = rnorm(9, 18, 5),
                         nitrogen_mg_per_kg = rnorm(9, 4.8, 0.5))
Year2Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                         Site = rep(c("Forest", "Pasture", "Corn Field"), each = 3),
                         carbon_mg_per_kg = rnorm(9, 19, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5, 0.5))
Year3Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                         Site = rep(c("Forest", "Pasture", "Corn Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5))

# Step # 1: practice the operation on one of the
# data frames.

# Option # 1
Year1Data$CtoNratio <- Year1Data$carbon_mg_per_kg / Year1Data$nitrogen_mg_per_kg

# Option # 2:
Year1Data$CtoNratio <- with(Year1Data, carbon_mg_per_kg / nitrogen_mg_per_kg)

# Option # 3:
with(Year1Data, CtoNratio <- carbon_mg_per_kg / nitrogen_mg_per_kg)

# Here's our function:
fxn7 <- function (df) {
  df$CtoNratio <- df$carbon_mg_per_kg / df$nitrogen_mg_per_kg
  df
}

# Since functions return the last thing you ask for,
# you have to specify in the last line of the function
# what the output of the function should be. In this
# case, you want the entire data frame as the output,
# not just the new column. If you use the assignment
# operator to create a new object in the last line of
# code in the function body, the function won't
# output anything.

fxn7(Year2Data)

# How can I generalize the previous function so that
# it works for any column containing the word
# 'carbon' and any column containing the word
# 'nitrogen' (i.e., without the '_mg_per_kg'
# suffix)?

# Here's how to find any occurrence of 'Carbon'
# or 'carbon' in the column names of a data frame:
grep("[Cc]arbon", colnames(Year1Data))

# Likewise, here's how to find any occurence of
# 'Nitrogen' or 'nitrogen' in the column names
# of a data frame:
grep("[Nn]itrogen", colnames(Year1Data))

# Now, let's use this in our function:
fxn8 <- function (df) {
  df$CtoNratio <- df[, grep("[Cc]arbon", colnames(Year1Data))] / df[, grep("[Nn]itrogen", colnames(Year1Data))]
  df
}

fxn8(Year3Data)
