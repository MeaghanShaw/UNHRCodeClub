
# Lists
# 2-24-2020

# My plea:

# Please learn how to use lists. Copying and pasting
# your code to re-run it for all of your different
# data frames (and going through and changing the names
# of your data frames each time) is time-consuming, and
# you're bound to miss changing a name at some point,
# which will give you wrong results that you might not
# catch when you go back and check. Lists make performing
# the same operation on many different data frames
# extremely easy and efficient, and you're guaranteed
# not to make any mistakes.

# You can save anything in a list.

AAA <- 3
BBB <- "cat"
CCC <- 1:10
DDD <- LETTERS
EEE <- matrix(1:24, ncol = 4)
FFF <- mtcars
GGG <- list(obj1 = iris, obj2 = 4 , obj3 = state.abb)
HHH <- summary(lm(iris$Sepal.Length ~ iris$Petal.Length))
my_list <- list(AAA = AAA, BBB = BBB, CCC = CCC, DDD = DDD, EEE = EEE, FFF = FFF, GGG = GGG, HHH = HHH)
str(my_list)

# The structure of this list looks crazy and ridiculously
# complicated, but it's not. We are going to break it down.

# Extracting from and subsetting lists

# Let's create a new list.

linear_model <- with(mtcars, lm(hp ~ mpg))
str(linear_model)

# Extracting by position: use double square brackets

linear_model[[1]]
linear_model[[5]]

# Extracting by name: use the dollar sign

linear_model$coefficients
linear_model$fitted.values

# The first and fifth elements in this list are nothing
# more than named vectors. Therefore, we can work with
# them as if they were just normal vectors, because they
# are.

str(linear_model[[1]])
class(linear_model[[1]])

# We can also subset and extract from them like any other
# vector (by position, by name, or using the 'which'
# function).

linear_model[[1]][1]
linear_model[[1]]["(Intercept)"]
linear_model[[1]][which(names(linear_model[[1]]) == "(Intercept)")]

# We just extracted using double square brackets and
# positions. Let's do the same using dollar signs and
# names.

linear_model$coefficients[1] # By position
linear_model$coefficients["(Intercept)"] # By name

# What's the difference between single square brackets
# and double square brackets?

linear_model[1]
str(linear_model[1])
linear_model[[1]]
str(linear_model[[1]])

# Double square brackets return just the individual
# element without it being part of the greater list's
# structure. Single square brackets return the requested
# elements with them still being part of the original
# list.

# Clearly, if we wish to extract more than one list
# element at once, what we will end up with will still
# be a list because it will contain more than one
# element from the original list. Therefore, we need to
# use single square brackets when extracting more than
# one list element at once.

linear_model[1:3]

# 'linear_model' is a nested list. Let's look at the
# 7th element of this list, which is itself a list.

linear_model[[7]]
# or
linear_model$qr

# Therefore, to extract and subset from this list, the
# concepts we will use to do so are the same as if we
# were subsetting from the original 'linear_model' list.

linear_model$qr$qraux
# or
linear_model[[7]][[2]]
# or mix-and-match:
linear_model$qr[[2]]
# or
linear_model[[7]]$qraux

# When we checked the structure of the original
# 'linear_model' list, we could see the levels
# of nesting by the indentations.

str(linear_model)

# There are a few handy helper functions that you
# can use with linear models (i.e., the output of
# the 'lm' function) to avoid having to extract and
# subset like we've been doing.

coef(linear_model)
predict(linear_model)

# I can assure you that it will be worth your while
# learning how to extract and subset from lists like
# we've been practicing, though.

# Remember that first list I showed you today?

str(my_list)

# We can perform operations on all elements of this
# list at once using the 'lapply' function.

lapply(my_list, class)

# The first argument of the 'lapply' function is the
# list whose elements you wish to perform the operation
# on. The second argument is the function you with to
# use on each of the list elements.

# 'my_list' contains many different types of elements,
# and it doesn't really make sense to perform the same
# operation on all of these list elements. For example,
# we couldn't ask R to perform a mathematical operation
# on all of these list elements because they aren't all
# of class 'numeric'.

# Let's look at an example where we can perform the same
# operation on all list elements.

# We may have many different data frames from the same
# study. For whatever reason, it may not make sense to
# combine them into one data frame. It makes sense to
# store all of these data frames in one list.

Year1Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 18, 5),
                        nitrogen_mg_per_kg = rnorm(9, 4.8, 0.5),
                        Year = 1)
Year2Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 19, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5, 0.5),
                        Year = 2)
Year3Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5),
                        Year = 3)
MyData <- list(Year1Data = Year1Data, Year2Data = Year2Data, Year3Data = Year3Data)

# Since these data frames are all structured the same way
# (all of their columns contain basically the same information),
# we can write functions to manipulate them all at once. This
# is much, much better than copying and pasting your code
# multiple times and then going back and changing the data
# frame name each time you want to perform an operation on one
# of the data frames.

lapply(MyData, nrow)
lapply(MyData, colnames)
lapply(MyData, class)

# Let's do some more sophisticated operations.

# Review: we want to write a function that will calculate the
# carbon-to-nitrogen ratio for each row in each data frame.
# How can we do this? Hint: let's practice with just one data
# frame to start. Remember, to access the first data frame in
# this list, you have to write either 'MyData[[1]]' or
# 'MyData$Year1Data'.

MyData[[1]]$C_to_N_Ratio <- MyData[[1]]$carbon_mg_per_kg / MyData[[1]]$nitrogen_mg_per_kg
# or
MyData$Year1Data$C_to_N_Ratio <- MyData$Year1Data$carbon_mg_per_kg / MyData$Year1Data$nitrogen_mg_per_kg

# Okay; that worked. Now, let's put this code into the body
# of a function.

fxn1 <- function (x) {
  x$C_to_N_Ratio <- x$carbon_mg_per_kg / x$nitrogen_mg_per_kg
}

# Here, 'x' represents a data frame in our list. In other words,
# it represents a list element.

# Question: What did we replace with 'x' to get from our practice
# line of code on lines 188 and 190 to the function on lines 195
# to 197?

# Answer: We replaced the data frame ('MyData[[1]]' or
# 'MyData$Year1Data) name with 'x'.

# Question: What's missing from this function?

# Answer: We didn't ask the function to return the data frame.
# Remember, by default, functions always return what you ask
# for in the last line of code in the body of the function. In
# our case, the last line of code in the body of our function
# assigned a value to an object, so it didn't return anything,
# just like in normal R coding.

fxn1 <- function (x) {
  x$C_to_N_Ratio <- x$carbon_mg_per_kg / x$nitrogen_mg_per_kg
  return (x)
}

# Now, let's apply this function to all of the data frames in
# the 'MyData' list at once. Since all of the data frames contain
# 'carbon_mg_per_kg' and 'nitrogen_mg_per_kg' columns, this
# function should work well.

lapply(MyData, fxn1)

# We can also write the function out inside of the 'lapply'
# function:

lapply(MyData, function (x) {
  x$C_to_N_Ratio <- x$carbon_mg_per_kg / x$nitrogen_mg_per_kg
  return (x)
})

# How can we extract just one column of interest from all
# three data frames in this list?

lapply(MyData, function (x) {
  x$nitrogen_mg_per_kg
})
# or
lapply(MyData, `[`, "nitrogen_mg_per_kg")

# The 'split' function

# What if we started out with one massive data frame
# containing data from all three years?

MyData <- rbind(Year1Data, Year2Data, Year3Data)
View(MyData)

# We can split this into different data frames by year,
# and if we use the 'split' function to do this, all of
# the resulting data frames will be nicely stored on a
# list. The elements in this list will be named, and the
# names will be the factor that you split by.

split(MyData, MyData$Year)

# The 'mapply' function

# What if we have two different lists from the same study
# that contain different types of information? For example,
# we may have a list of data frames containing raw data,
# and we may have a list of data frames containing metadata.

# Raw data:
MyData <- rbind(Year1Data, Year2Data, Year3Data)
MyData <- split(MyData, MyData$Site)

# Metadata:
Corn_Field <- data.frame(phosphorus_mg_per_kg = 1.8, potassium_mg_per_kg = 2.3)
Forest <- data.frame(phosphorus_mg_per_kg = 0.9, potassium_mg_per_kg = 2.1)
Pasture <- data.frame(phosphorus_mg_per_kg = 1.2, potassium_mg_per_kg = 1.9)
(Metadata <- list(Corn_Field = Corn_Field, Forest = Forest, Pasture = Pasture))

# 1. Both the raw data list and the metadata list contain
# data frames from the different sites in the same order
# (i.e., the corn field is the first, then the forest, and
# finally the pasture).

# 2. Perhaps the researchers only thought it important to
# measure potassium and phosphorus at each site once before
# the study began; hence, these data are not structured
# like the carbon and nitrogen data.

# Can we calculate carbon-to-phosphorus ratios? To do so,
# we will divide each carbon value by the corresponding
# phosphorus value for each site. Thus, the phosphorus
# value will be repeated (or 'recycled') within each site.

# Since we are now working with two lists, we need to write
# functions of two variables. The 'm' in 'mapply' stands for
# multivariate.

# Can you figure out what this function is doing?
mapply(rep, 1:4, 4:1)

# As always, let's start of simple again. We will try to
# get our desired output using just the corn field data.

MyData$Corn_Field$carbon_mg_per_kg
Metadata$Corn_Field$phosphorus_mg_per_kg

MyData$Corn_Field$carbon_mg_per_kg / Metadata$Corn_Field$phosphorus_mg_per_kg
# In the above line of code, the 1.8 was recycled and
# used as the denominator of each ratio.

# We can save this output as a new column in our corn
# field data frame:
MyData$Corn_Field$C_to_P_Ratio <- MyData$Corn_Field$carbon_mg_per_kg / Metadata$Corn_Field$phosphorus_mg_per_kg

# This works; let's scale up. First, let's write the
# function we want to use. We'll let 'x' represent
# the raw data and 'y' represent the metadata. In other
# words, any time we see a 'MyData' in the preceding
# line of code, we'll replace it with an 'x', and any
# time we see a 'Metadata' in the preceding line of
# code, we'll replace it with a 'y'.

fxn2 <- function (x, y) {
  x$C_to_P_Ratio <- x$carbon_mg_per_kg / y$phosphorus_mg_per_kg
  return (x)
}

fxn2

mapply(fxn2, x = MyData, y = Metadata, SIMPLIFY = F)

# The 'SIMPLIFY = FALSE' argument tells the function
# that you want the output to be of the same structure
# as the input. In other words, it won't attempt to
# simplify the output to vector, matrix, or data frame
# formats.

# Working with nested lists (if time allows)

# We may find ourselves with lists nested within lists.

MyData <- list(Year1Data = Year1Data, Year2Data = Year2Data, Year3Data = Year3Data)
NestedList <- list(MyData, MyData, MyData)
str(NestedList)

# Let's try to perform the same operations we did above
# on the data frames nested deeply within this new list.

lapply(NestedList, function (x) {
  lapply(x, nrow)
})

# The output here is structured just like the list we
# started with.

lapply(NestedList, function (x) {
  lapply(x, colnames)
})

# Let's step up our game a little bit.

# Let's create that 'C_to_N_Ratio' column in each of our
# data frames again.

lapply(NestedList, function (x) {
  lapply(x, function (y) {
    y$C_to_N_Ratio <- y$carbon_mg_per_kg / y$nitrogen_mg_per_kg
    return (y)
  })
})

# The final challenge:

# What if we have two nested lists that we want to perform
# operations on? For example:

NestedList
MetadataNested <- list(Metadata, Metadata, Metadata)

# Can you figure out how to use the 'mapply' function to
# create a 'C_to_P_Ratio' column in each of the data
# frames? This operation will be similar to the one we
# did previously, but now we are working with one more
# level of nesting than we were previously.

# Answer:

mapply(function (a, b) {
  mapply(fxn2, x = a, y = b, SIMPLIFY = F)
}, a = NestedList, b = MetadataNested, SIMPLIFY = F)
