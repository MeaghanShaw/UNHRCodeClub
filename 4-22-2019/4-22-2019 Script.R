
# Using 'lapply' with lists of dataframes

# Create a list to work with that has columns of different lengths but with the same names and types of data
df1 <- data.frame(A = rnorm(25, 5, 3), B = rcauchy(25, 100, 5), c = LETTERS[1:25], D = rnorm(25), E = seq(from = Sys.time(), length.out = 25, by = "15 min"))
df2 <- data.frame(A = rnorm(35, 5, 3), B = rcauchy(35, 100, 5), c = LETTERS[c(1:25, 1:10)], D = rnorm(35), E = seq(from = Sys.time(), length.out = 35, by = "15 min"))
df3 <- data.frame(A = rnorm(45, 5, 3), B = rcauchy(45, 100, 5), c = LETTERS[c(1:25, 1:20)], D = rnorm(45), E = seq(from = Sys.time(), length.out = 45, by = "15 min"))
df4 <- data.frame(A = rnorm(55, 5, 3), B = rcauchy(55, 100, 5), c = LETTERS[c(1:25, 1:5, 1:25)], D = rnorm(55), E = seq(from = Sys.time(), length.out = 55, by = "15 min"))
my_list <- list(df1, df2, df3, df4)
names(my_list) <- c("df1", "df2", "df3", "df4")
