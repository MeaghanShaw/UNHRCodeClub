
# Regular Expressions
# David Moore
# 10/24/2019

?regex

# Using quotation marks inside of quotation marks:
# you can use single quotes inside double quotes
# and vice versa
phrase_1 <- "he said, 'go home'"
phrase_2 <- '"go home," he said'
# If you want to use double quotes inside of double
# quotes (or single quotes inside single quotes),
# though, you need to use a backslash to escape
# the quotes on the inside:
phrase_3 <- '\'go home,\' he said'
# You need to do something similar to find
# quotation marks inside of a character string:
vector_1 <- c(phrase_1, phrase_2, "cat", phrase_3)
grep("\'", vector_1)

# Using the '|' (logical 'or') inside a character string:
colnames(mtcars)
grep("mpg|cyl|hp", colnames(mtcars))

# Extracting character strings that contain a certain pattern
# if and only if it occurs at the beginning of a character
# string
vector_2 <- c("Jean Claude", "Claude Debussy", "Michael Jordan", "Jordan, Michael")
grep("Claude", vector_2)
grep("^Claude", vector_2)

# Extracting character strings that contain a certain pattern
# if and only if it occurs at the end of a character string
grep("Jordan", vector_2)
grep("Jordan$", vector_2)

state.name
# Find all the states that end with 'e'.

# Find character strings that contain a certain pattern
# if and only if the pattern occurs at the beginning or
# end of a word within the character string (not
# necessarily only at the beginning or end of the entire
# character string)
vector_3 <- c("oh me oh my", "hello", "oleo", "how ostensible", "Groton, Massachusetts")
# All of these character strings contain an 'o', so
# if I search for just 'o', it will return all positions:
grep("o", vector_3)
# What if I'm just interested in words that begin with 'o'?
grep("\\<o", vector_3)
grep("\\bo", vector_3)
# Or that end in 'o'?
grep("o\\>", vector_3)
grep("o\\b", vector_3)
# Or that either begin or end with 'o'?
grep("o\\b|\\bo", vector_3)
# Think of '\\b' as marking the point at which a word
# begins or ends. Thus, to find something that starts
# a word, you put the pattern you're searching for
# after the '\\b'.

vector_3a <- c("h", "hh", "hhh", "hah")
# Which vector contains something other than an 'h'?
grep("[^h]", vector_3a)

# If we want to find something that doesn't contain
# a certain pattern, we usually cant use the '[^]'
# syntax:
grep("[^Massachusetts]", vector_3)
# Why doesn't this work?
# Because the 5th element in 'vector_3' contains
# 'Groton, ' in addition to 'Massachusetts', so
# the 'grep' function was able to match the first
# portion of the 'Groton, Massachusetts' element
# since it doesn't contain 'Massachusetts'.

# Searching for things that don't include a certain
# pattern (i.e., the pattern 'el' in the previous vector):
vector_3
# Option 1 (this option doesn't use regular expressions):
which(!grepl("el", vector_3))
grepl("el", vector_3)
!grepl("el", vector_3)
# Option 2 (using lookarounds):
grep("^((?!el).)*$", vector_3, perl = T)

# Lookarounds
# Which elements of 'vector_3' contain an 'e' that's
# followed (eventually, or at some point) by an 'h'?
# Option 1 (using lookarounds):
grep("e(?=.*h)", vector_3, perl = T)
# Option 2 (using other regular expressions):
grep("e.*h", vector_3)
# Note that we need to set the 'perl' argument to 'T' to
# use lookarounds.
# Which elements of 'vector_3' contain an 'e' that isn't
# followed by an 'h'?
# Option 1 (using lookarounds):
grep("e(?!.*h)", vector_3, perl = T)
# Option 2 (using other regular expressions):
grep("e[^h.]*$", vector_3)
# Which elements of 'vector_3' contain an 'e' that's
# preceded by an 'h'?
#grep("(?<=h)[^h]*e", vector_3, perl = T)
# Option 1 (using lookarounds):
grep("(?<=h).*e", vector_3, perl = T)
# Option 2 (using other regular expressions):
grep("h.*e", vector_3)
# Which elements of 'vector_3' contain an 'e' that isn't
# preceded by an 'h'?
# Option 1 (using lookarounds):
grep("^[^h]*(?<!h)[^h]*e", vector_3, perl = T)
# Option 2 (using other regular expressions):
grep("^[^h]*e", vector_3)
# In most cases (like the ones above), using
# other regular expressions instead of lookarounds
# is more intuitive and also more efficient, but
# there are certain times where lookarounds are
# the best (and only) option.

# Find character strings that contain a certain pattern
# if and only if the pattern doesn't occur at the beginning
# or end of a word
grep("\\Be", vector_3) # This matches 'e's that aren't at the
# beginning of a word
grep("e\\B", vector_3) # This matches 'e's that aren't at the
# end of a word
grep("e\\B|\\Be", vector_3) # This doesn't work
intersect(grep("\\Be", vector_3), grep("e\\B", vector_3)) # This
# finds the ones that have an 'e' that isn't at the beginning
# or the end of a word
# There is a more concise and efficient way to do this using
# regular expressions. You need to use the equivalent of a
# logical 'and' statement to do it: you need to find things
# for which both '\\Be' AND 'e\\B' are true.
grep("(?=.*\\Be)(?=.*e\\B)", vector_3, perl = T)
# Another example of this: which state(s) contain both 'Ne'
# and 'am'?
grep("(?=.*Ne)(?=.*am)", state.name, perl = T)
# New Hampshire: 'Ne' is before 'am'
grep("Ne.*am", state.name)
state.name[grep("(?=.*Ne)(?=.*am)", state.name, perl = T)]
# The order doesn't matter:
state.name[grep("(?=.*am)(?=.*Ne)", state.name, perl = T)]
# If order did matter and we knew the order, we could do this:
grep("Ne.*am", state.name)

# Extracting periods
vector_4 <- c("Pre.Paid", "Pre_Paid", "Pre Paid")
grep(".", vector_4)
# Note: the period is a special regex character that represents
# all characters, so we need to escape it if it's what we're
# after.
grep("[.]", vector_4)
grep("\\.", vector_4)

# Extracting all elements containint punctuation:
grep("[[:punct:]]", vector_4)

# Extracting things that appear zero, one, or two or more times
vector_5 <- c("aba", "abb", "aab", "aaa", "bba", "bbb", "bab", "baa")
grep("a{2}", vector_5) # This returns character strings that contain
# 'aa'. It includes ones that have three or more consecutive 'a's too
# though.
# What if we only want character strings that have 2 consecutive 'a's
# but no more?
grep("a{2}", vector_5)
grep("a{3,}", vector_5)
setdiff(grep("a{2}", vector_5), grep("a{3,}", vector_5))
# The more efficient way, again using the lookaround (and the logical
# 'and') method:
grep("(?=.*a{2})(?=.*[^a{3,}])", vector_5, perl = T)

(vector_6 <- c(letters, as.character(0:9), LETTERS))
grep("[[:digit:]]", vector_6)
# We can search for ranges of letters and/or digits:
grep("[3-6]", vector_6)
grep("[C-X]", vector_6)
# If we put more than one thing between square brackets,
# R interprets this as being a logical 'or' statement.
# For example, if we want to look for uppercase or
# lowercase letters ranging from c to x, we could do:
grep("[C-Xc-x]", vector_6)
# An alternative to this last example:
grep("[C-X]", vector_6, ignore.case = T)

# Another example of using the square brackets as part
# of a logical 'or' statement:
new_vector <- c("Harold", "Herold", "Hirold")
grep("H[ea]rold", new_vector)

# You can either use the 'ignore.case' argument or you can
# use regular expressions to get both uppercase and lowercase
# letters.
vector_7 <- c("Mississippi", "mississippi", "MISSISSIPPI", "Texas", "Louisiana", "MISSissiPPI")
grep("M", vector_7)
grep("m", vector_7)
union(grep("M", vector_7), grep("m", vector_7))
sort(union(grep("M", vector_7), grep("m", vector_7)))
grep("[Mm]", vector_7)
grep("[[:lower:]]", vector_7)
grep("[[:upper:]]", vector_7)
# How can you find if something only contains uppercase letters?
# Option 1:
grep("^[[:upper:]]+$", vector_7)
# In the above method, we start at the beginning of a string ('^')
# and look for uppercase things ('[[:upper:]]') that occur one or more
# times ('+') and that go all the way to the end of the string ('$').
# Option 2:
toupper(vector_7)
vector_7 == toupper(vector_7)
which(vector_7 == toupper(vector_7))
vector_7[which(vector_7 == toupper(vector_7))]

# What if I want to extract a portion of a character string?
vector_8 <- c("56774:H34J", "66900:JK8L", "99213:S2DD")
# I only want the stuff before the colon, not including
# the colon:
gsub(":[[:alnum:]]+$", "", vector_8)
# I only want the stuff after the colon, including the
# colon:
# Option 1:
gsub("\\d{5}", "", vector_8)
# Option 2:
gsub("[[:digit:]]{5}", "", vector_8)

# What if we're looking for things in a specific order?
vector_9 <- c("abcdde", "accb", "bddfza", "zyxwdcba")
# I want to extract character strings that have an 'a'
# before a 'b".
grep("a.*b", vector_9)

# Another useful base R function for working with character strings:
regexpr("ana", state.name)
# What does the 'regexpr' function tell you?
regexpr("ana", state.name) != -1
which(regexpr("ana", state.name) != -1)
state.name[which(regexpr("ana", state.name) != -1)]
