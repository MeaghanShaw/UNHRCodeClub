
# Cool Base R Functions
# David Moore
# 10-3-2019

# The 'gsub' function is for substituting
# character strings with other character strings

gsub("r", "R", "response")
(Column_Names <- paste("response", 1:15, sep = "_"))
gsub("response", "yield", Column_Names)
gsub("_", "", Column_Names)
gsub("[.]", "_", colnames(iris)) # A simple period
# doesn't work here because of regular expressions.

# The 'rle' (run-length encoding) function

A <- c(1:(26 * 5))
B <- rep(LETTERS, each = 5)
C <- rnorm((5 * 26), 0, 0.1) + sin((1:(26 * 5)) / pi)
df1 <- data.frame(A, B, C)
plot(C ~ A, df1)

# What if we want to know how many time periods there are
# that contain consecutive values in column 'C' that are
# all less than -0.5?

df1$D <- df1$C < (-0.5)
rle(df1$D)

# Unfortunately, this is in list form, but we can
# convert it into a data frame if we need to:

str(rle(df1$D))
(rle_df <- data.frame(lengths = rle(df1$D)$lengths, values = rle(df1$D)$values))

# '&' (and) and '|' (or) logical operators and the
# 'any' and 'all' functions

vector1 <- c(T, T, T, F, T, T, F)
vector2 <- c(T, T, T, T, T, T, T)
vector3 <- c(F, F, F, F, F, F, F)
vector4 <- c(T, T, F, F, T, F, T)

# '&' and '|' work between vectors:

vector1
vector4
vector1 & vector4
vector1 | vector4

# 'any' and 'all' are similar, but they work within vectors:

vector1
any(vector1)
vector2
any(vector2)
vector3
any(vector3)

# The 'with' function makes it so that you don't have to
# keep typing out the data frame name every time you want
# to use a column from that data frame:

# We know these work:

lm(mtcars$mpg ~ mtcars$cyl * mtcars$wt)
lm(mpg ~ cyl * wt, data = mtcars)

# Here's another shortcut:

with(mtcars, lm(mpg ~ wt * cyl))

# One more example:

(iris$Sepal.Ratio <- with(iris, Sepal.Width / Sepal.Length))

# 'interaction.plot' can be used to investigate interactions:

View(npk)
with(npk, anova(lm(yield ~ N * P + block)))

# Though the 2-way interaction of 'N' and 'P' isn't significant,
# we're still going to check it out (just for fun).

with(npk, interaction.plot(N, P, yield))
with(npk, interaction.plot(P, N, yield))

# Finding how many times one number goes into another one,
# finding remainders, rounding, etc.

# What is 5 divided by 3?

5 / 3

# How many times does 3 go into 5?

5 %/% 3

# What is the remainder when 5 is divided by 3?

5 %% 3

# Here's a practical example of why you might
# want to use the above functions:

# You have columns 'response_1', 'response_2,
# 'response_3', etc., up to 'response_15'

(Column_Names <- paste("response", 1:15, sep = "_"))

# These responses occur in groups of 3:
# responses 1 through 3 were measured from
# tree 1, responses 4 through 6 were measured
# from tree 2, etc.

Number_of_Responses_per_Tree <- 3

# We want to add tree number to the column names.

# First, let's extract the response numbers:

(Response_Numbers <- as.numeric(gsub("response_", "", Column_Names)))

# To generate tree numbers, we'll use the '%/%
# function as above:

Response_Numbers %/% Number_of_Responses_per_Tree

# It looks like we're off a little - we should start
# with 3 consecutive ones and end with three consecutive
# fives. So, let's try doing the same thing, but we'll
# start with 'Response_Numbers + 2' instead of
# 'Response_Numbers':

(Response_Numbers + 2) %/% Number_of_Responses_per_Tree

# That seemed to work. Let's save that output as an object.

Tree_Numbers <- (Response_Numbers + 2) %/% Number_of_Responses_per_Tree

(New_Column_Names <- paste(Column_Names, "tree", Tree_Numbers, sep = "_"))

# We also want to add tree height in. The first measurement
# from each tree was taken at the base, the second was taken
# from a height of 5 m, and the third was taken in the
# canopy.

# We can use the '%%' function to help with this:

Response_Numbers %% Number_of_Responses_per_Tree

# Again, we're off a little.

(Response_Numbers - 1) %% Number_of_Responses_per_Tree

# One more step....

((Response_Numbers - 1) %% Number_of_Responses_per_Tree) + 1
Canopy_Positions_Numeric <- ((Response_Numbers - 1) %% Number_of_Responses_per_Tree) + 1

# Great. Now, we can set all of the '1's equal to
# 'ground', all of the '2's equal to 'midcanopy',
# and all of the '3's equal to 'upper_canopy'.

(Canopy_Positions <- ifelse(Canopy_Positions_Numeric == 1, 'ground', ifelse(Canopy_Positions_Numeric == 2, 'midcanopy', 'upper_canopy')))

# Okay; we are at the last step.

(Final_Column_Names <- paste(New_Column_Names, Canopy_Positions, sep = "_"))

# Rounding

round(3.4)
round(3.6)
round(3.5)
ceiling(3.4)
floor(3.4)
pi
round(pi, digits = 2) # For the 'round'
# function, the 'digits' argument specifies
# how many decimal places to round to.
signif(pi, digits = 3) # For the 'signif'
# function, the 'digits' argument specifies
# how many significant digits to round to.
signif(10000.34, digits = 3)

# 'cut'

# We wish to sort up our vector of responses
# into 'low', medium', and 'high'.

npk$yield

# 'low' yields are below 50 and 'high' yields
# are above 60.

cut(npk$yield, c(0, 50, 60, 100), c('low', 'medium', 'high'))

# We could also split these yields up by terciles.

Terciles <- quantile(npk$yield, probs = c(0, 1/3, 2/3, 1))
cut(npk$yield, c(Terciles[1], Terciles[2], Terciles[3], Terciles[4]), c('low', 'medium', 'high'))

# 'cut' is a little more efficient that nesting
# 'ifelse' functions when you're dealing with numeric
# data.

# What if you're working with non-numeric data?

# The 'switch' statement

switch (1, "cat", "dog", "horse")
switch (2, "cat", "dog", "horse")
switch (3, "cat", "dog", "horse")
switch (4, "cat", "dog", "horse")
x <- 2
switch (x, "cat", "dog", "horse")

# Like an 'if' statement, 'switch' does not
# work on vectors.

y <- 1:3
switch(y, "cat", "dog", "horse")

# Though 'switch' does not work in this
# instance, we can still convert a vector
# of numbers into the desired character
# strings all at once, though. Let's turn
# our numeric vector into levels of a factor.

y <- as.factor(y)
str(y)
levels(y) <- c("cat", "dog", "horse")
y

z <- c(1, 3, 3, 2, 3, 1, 2, 1, 3)
z <- as.factor(z)
levels(z) <- c("cat", "dog", "horse")
z
