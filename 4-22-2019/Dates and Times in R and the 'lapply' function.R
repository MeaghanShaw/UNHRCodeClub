
# R Coding Club
# 4-22-2019
# Presenter: David Moore

# Working with POSIXct dates and times
Time <- "4/5/2018, 5:00:02 PM"
class(Time)
New_Time <- as.POSIXct(Time, format = "%m/%d/%Y, %H:%M:%S %p")
class(New_Time)
New_Time

# Filling in missing dates and times
Sys.time() # This will give you the current date and time (from your computer's clock)
A <- seq(from = Sys.time(), length.out = 100, by = "15 min")
B <- seq.int(1, to = length(A), by = 1)
df30 <- data.frame(A, B)
View(df30)
Rows_to_Remove <- c(10:12, 35, 39)
df30 <- df30[-Rows_to_Remove, ]
df30_corrected_time <- as.data.frame(seq(as.POSIXct(df30$A[1]), as.POSIXct(df30$A[nrow(df30)]), by = "15 min"))

View(df30_corrected_time)
df30_fixed <- merge(df30_corrected_time, df30, by = "A", all = TRUE) # why the error?
names(df30_corrected_time)

colnames(df30_corrected_time) <- c("A")
df30_fixed <- merge(df30_corrected_time, df30, by = "A", all = T)
which(is.na(df30_fixed$B))
which(is.na(df30_fixed$B)) == Rows_to_Remove


# Using 'lapply' with lists of dataframes

# Create a list to work with that has columns of different lengths but with the same names and types of data
df1 <- data.frame(A = rnorm(25, 5, 3), B = rcauchy(25, 100, 5), C = LETTERS[1:25], D = rnorm(25), E = seq(from = Sys.time(), length.out = 25, by = "15 min"))
df2 <- data.frame(A = rnorm(35, 5, 3), B = rcauchy(35, 100, 5), C = LETTERS[c(1:25, 1:10)], D = rnorm(35), E = seq(from = Sys.time(), length.out = 35, by = "15 min"))
df3 <- data.frame(A = rnorm(45, 5, 3), B = rcauchy(45, 100, 5), C = LETTERS[c(1:25, 1:20)], D = rnorm(45), E = seq(from = Sys.time(), length.out = 45, by = "15 min"))
df4 <- data.frame(A = rnorm(55, 5, 3), B = rcauchy(55, 100, 5), C = LETTERS[c(1:25, 1:5, 1:25)], D = rnorm(55), E = seq(from = Sys.time(), length.out = 55, by = "15 min"))
my_list <- list(df1, df2, df3, df4)
names(my_list) <- c("df1", "df2", "df3", "df4")
my_list

# 'lapply' works on lists (i.e., the first argument you provide to that function is a list) and it returns lists as well
lapply(my_list, class)
lapply(my_list, length)
lapply(my_list, ncol)
lapply(my_list, nrow)

# Using this type of syntax, how can you get R to return a list containing vectors that contain the column names of each dataframe?
lapply(my_list, colnames)

# Using the 'lapply' function with custom-built functions to do more complex things
lapply(my_list, mean)
# Column means
lapply(my_list, function(x) {mean(x$A)})
lapply(my_list, function(x) {mean(x[, c(1, 2, 4)])})

# Re-ordering columns
lapply(my_list, function(x) {x[, c(5, 3, 1, 2, 4)]})
lapply(my_list, function(x) {x[, c("E", "C", "A", "B", "D")]})

# Column means for all columns (we need to nest one 'lapply' function within another to do it efficiently!)
lapply(my_list, function(x) {data.frame(mean_A = mean(x$A), mean_B = mean(x$B), mean_D = mean(x$D))})
lapply(my_list, function(x) {data.frame(lapply(x[, c("A", "B", "D")], function(y) {mean(y)}))})

# See below for the most efficient methods for extracting all numeric columns and calculating their means
lapply(lapply(my_list, function(x) {x[ , lapply(x, is.numeric) == TRUE]}), function(y) {data.frame(t(colMeans(y)))})
lapply(lapply(my_list, function(x) {x[ , sapply(x, class) == "numeric"]}), function(y) {data.frame(t(colMeans(y)))})
lapply(lapply(my_list, function(x) {Filter(is.numeric, x)}), function(y) {data.frame(t(colMeans(y)))})
lapply(lapply(my_list, function(x) {x[ , unlist(lapply(x, is.numeric))]}), function(y) {data.frame(t(colMeans(y)))})
lapply(lapply(my_list, function(x) {x[ , lapply(x, is.numeric) == TRUE]}), function(y) {data.frame(lapply(y, mean))})
lapply(lapply(my_list, function(x) {x[ , sapply(x, class) == "numeric"]}), function(y) {data.frame(lapply(y, mean))})
lapply(lapply(my_list, function(x) {Filter(is.numeric, x)}), function(y) {data.frame(lapply(y, mean))})
lapply(lapply(my_list, function(x) {x[ , unlist(lapply(x, is.numeric))]}), function(y) {data.frame(lapply(y, mean))})


# Using the 'Map' function to View many dataframes at once ('lapply' does not work with the 'View' function)
Map(View, my_list, title = names(my_list))
# Here, I'm passing an argument to the 'Map' function that actually is not an argument for the 'Map' function
# ('title' is an argument for the 'View' function)
# This is what the '...' means in a function's help file - you can pass arguments to a function that will be used
# internally by functions within the function you're actually using
