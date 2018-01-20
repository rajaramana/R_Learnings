##################################### Robust functions

# We will learn about robust functions, they are easy use and are reliable. They either retain the correct result or they will fail with a clear error message. 


# what do these calls do?
df[,vars]
subset(df, x==y)
data.frame(x = 'a')

# These functions look simple, but have hidden dips which contribute to the failure of the function. These problem occur due to fundamental tension in R. R is used both as an interactive environment and a programming language. when doing an interactive analaysis, you iterate and check result one by one. On the other hand, functions for programming should be robust. They are three main clauses of functions that are ued indirectly can cause errors while writing functions.

# 1. Type unstable functions.
# One time they might return vector, other time a data frame

# 2. Non standard evaluation
# Is a very important part of R magic, helps to use important packages like dplyr and ggplot2. It introduces ambiguite which we need to be very careful of.

# 3. Hidden Arguments
# can effect certain functions. The most notorious of these is strings as vectors

# We will learn to write functions that throw an informative messages when we encounter suprising results. 

# 1. stopifnot(is.character(x))
stopifnot(is.character(x))
# The arguments for stop if not is logical expressions. 

if(condition){
	stop('Error', call. = FALSE)
}

# If any false an error is thrown.
if(!is.character(x)){
stop('x should be a vector', call. = FALSE)
}

# Stop is a great way of adding quick checks to a function. Usually error messages are not friendly, adding stop helps you to inform better about these error messages. If a condition is true, exit the value with an error message. We say a problem has occurred.

# Important call. says that the call to the function should not be part of the error message 
# call. = FALSE is the setting we always prefer 

if(!is.character(x)){
	stop("`x` should be a character vector", call. = FALSE)
}


##################################### 
# Error is better than a surprise
#####################################

# An error is better than a surprise. Recall our both_na() function from Chapter 2, that finds the number of entries where vectors x and y both have missing values:

both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}

# We had an example where the behavior was a little surprising:

x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)
both_na(x, y)

# The function works and returns 3, but we certainly didn't design this function with the idea that people could pass in different length arguments.
# Using stopifnot() is a quick way to have your function stop, if a condition isn't meant. stopifnot() takes logical expressions as arguments and if any are FALSE an error will occur.

# Define troublesome x and y
x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)

both_na <- function(x, y) {
  # Add stopifnot() to check length of x and y
  stopifnot(length(x) == length(y))
  stopifnot(sum(is.na(x) & is.na(y)))
}

# Call both_na() on x and y
both_na(x, y)


#######################################
# An informative error is even better
#######################################

# An informative error is even better. Using stop() instead of stopifnot() allows you to specify a more informative error message. Recall the general pattern for using stop() is:

if (condition) {
  stop("Error", call. = FALSE)
}

# Writing good error messages is an important part of writing a good function! We recommend your error tells the user what should be true, not what is false. For example, here a good error would be 
# "x and y must have the same length", rather than the bad error "x and y don't have the same length".

# Replace condition with a logical statement that evaluates to TRUE when x and y have different lengths.
# Change the error message to "x and y must have the same length".
# Run the call to both_na() to verify it returns a more informative error.

# Define troublesome x and y
x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)

both_na <- function(x, y) {
  # Replace condition with logical
  if (length(x) != length(y)) {
    # Replace "Error" with better message
    stop("x and y must have the same length", call. = FALSE)
  }  
  
  sum(is.na(x) & is.na(y))
}

# Call both_na() 
both_na(x, y)


##################################################
# A different kind of surprise: side effects
##################################################

# Side effects describe the things that happen when you run a function that alters the state of your R session. If foo() is a function with no side effects (a.k.a. pure), then when we run x <- foo(), the only change we expect is that the variable x now has a new value. No other variables in the global environment should be changed or created, no output should be printed, no plots displayed, no files saved, no options changed. We know exactly the changes to the state of the session just by reading the call to the function.

# Can you identify which of these functions doesn't have side effects?

show_missings <- function(x) {
  n <- sum(is.na(x))
  cat("Missing values: ", n, "\n", sep = "")
  x
}

replace_missings <- function(x, replacement) {
  x[is.na(x)] <- replacement
  x
}

plot_missings <- function(x) {
  plot(seq_along(x), is.na(x))
  x
}

exclude_missings <- function() {
  options(na.action = "na.exclude")
}


# correct answer replace_missings
# Correct, great job! Of course functions with side effects are crucial for data analysis. You need to be aware of them, and deliberate in their usage. It's ok to use them if the side effect is desired, but don't surprise users with unexpected side effects.


### New Video
############################################# Unstable types

# The type of object cannot be predicted without knowing what the inputs are
# Type inconsistent = The type of return objects depend on the input
# They are hard to program
# Suprises occur when you've used a type-inconsistent function inside your own function
# Sometimes lead to hard to decipher error messages


# What will df[1,] return?
# it returns 1st row of the df. 95% of the time we are right. But if df happens to be a single column dataframe then we get back a vector with just the first value of the df. This is the type inconsistent function

# Think that we have written a lastrow function, which fetches the last row
last_row(df)
# if it gives an integer then it is type inconsistency. 
# The solution is unfortunately depends on the problem function
# The argument drop control this behavior
# Setting argument drop = FALSE, helps forces single record subsetting to be type consistent
# Another way can be to use the data frame as a list

# Solution
last_row <-  function(df){
	df[nrow(df), , drop  = FALSE]
}

# Use drop = FALSE: df[x, , drop = FALSE]
# Subset the data frame like a list: df[x]


# What to do?
# Write your own functions to be type stable.
# Learn common type -inconsistent function is sapply [, sapply
# Avoid type inconsistent in your own function
# All functions in purrr are type consistent
# Build a vocabulary of type consistent functions

# sapply is another common culprit. sapply() is another common offender returning unstable types. The type of output returned from sapply() depends on the type of input.
# Consider the following data frame and two calls to sapply():

df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

A <- sapply(df[1:4], class) 
B <- sapply(df[3:4], class)

# What type of objects will be A and B be?
# A will be a list, B will be a character matrix.

######################### Using purrr solves the problem

# This unpredictable behaviour is a sign that you shouldn't rely on sapply() inside your own functions. So, what do you do? Use alternate functions that are type consistent! And you already know a whole set: the map() functions in purrr. In this example, when we call class() on the columns of the data frame we are expecting character output, so our function of choice should be: map_chr():

df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

A <- map_chr(df[1:4], class) 
B <- map_chr(df[3:4], class)

# Except that gives us errors. This is a good thing! It alerts us that our assumption (that class() would return purely character output) is wrong. Let's look at a couple of solutions. First, we could use map() instead of map_chr(). Our result will always be a list, no matter the input.

# sapply calls
A <- sapply(df[1:4], class) 
B <- sapply(df[3:4], class)
C <- sapply(df[1:2], class) 

# Demonstrate type inconsistency
str(A)
str(B)
str(C)

# Use map() to define X, Y and Z
X <- map(df[1:4], class) 
Y <- map(df[3:4], class)
Z <- map(df[1:2], class)

# Use str() to check type consistency
str(X)
str(Y)
str(Z)


###################################
# A type consistent solution
####################################

# A type consistent solution
# If we wrap our solution into a function, we can be confident that this function will always return a list because we've used a type consistent function, map():

col_classes <- function(df) {
  map(df, class)
}

# But what if you wanted this function to always return a character string?
# One option would be to decide what should happen if class() returns something longer than length 1. For example, we might simply take the first element of the vector returned by class().
# Assign the body of our previous function to the variable class_list.
# Use map_chr() along with the numeric subsetting shortcut, to extract the first element from every item in class_list. Run the final three lines to verify our new function always returns a character vector.

col_classes <- function(df) {
  # Assign list output to class_list
  class_list <- map(df, class)
  
  # Use map_chr() to extract first element in class_list
  map_chr(class_list, 1)
}

# Check that our new function is type consistent
df %>% col_classes() %>% str()
df[3:4] %>% col_classes() %>% str()
df[1:2] %>% col_classes() %>% str()


col_classes <- function(df) {
  class_list <- map(df, class)
  
  # Add a check that no element of class_list has length > 1
  if (any(map_dbl(class_list, length) > 1)) {
    stop("Some columns have more than one class", call. = FALSE)
  }
  
  # Use flatten_chr() to return a character vector
  flatten_chr(class_list)
}

# Check that our new function is type consistent
df %>% col_classes() %>% str()
df[3:4] %>% col_classes() %>% str()
df[1:2] %>% col_classes() %>% str()


# New Video
#####################################
# Non Standard Evaluation
#####################################

# Another class of functions that are source of surprises is the non-standard function. These are the ones that use the non standard evaluations. 
# These functions do not use the usual look up rules for variables
# Non standard evaluation functions are great for data analysis, because they save typing

subset(mtcars, displ > 40) # this works, but you just say displ > 40 it throws an error, the data is not in the global level. Need to refer them by table

# Other NSE (non standard evaluation functions)
library('ggplot2')
ggplot(mpg, aes(displ, cty)) + geom_point()

library(dplyr)
filter(mtcars, displ > 40)

# using non standard evaluation function inside your own function can cause surprises
# Avoid using non-standard evaluation functions inside your functions
# Or learn the surprising cases and protect against them


######################
# Programming with NSE
######################

# Let's take a look at a function that uses the non-standard evaluation (NSE) function filter() from the dplyr package:

big_x <- function(df, threshold) {
  dplyr::filter(df, x > threshold)
}


# This big_x() function attempts to return all rows in df where the x column exceeds a certain threshold. Let's get a feel for how it might be used.

# We've placed diamonds_sub, a 20 row subset of the diamonds data from the ggplot2 package in your workspace. Use big_x() to find all rows in diamonds_sub where the x column is greater than 7.

# Use big_x() to find rows in diamonds_sub where x > 7
big_x(diamonds_sub, 7)

###############################
# When things go wrong
###############################

# Now, let's see how this function might fail. There are two instances in which the non-standard evaluation of filter() could cause surprising results:

# The x column doesn't exist in df. There is a threshold column in df.
# Let's illustrate these failures. In each case we'll use big_x() in the same way as the previous exercise, so we should expect the same output. However, not only do we get unexpected outputs, there is no indication (i.e. error message) that lets us know something might have gone wrong.

# Create a variable x and give it the value 1.
# Use big_x() to find all rows in diamonds_sub where the x column is greater than 7.
# Create a threshold column in diamonds_sub with the value 100.
# Use big_x() to find all rows in diamonds_sub where the x column is greater than 7.

# Remove the x column from diamonds
diamonds_sub$x <- NULL

# Create variable x with value 1
x <- 1

# Use big_x() to find rows in diamonds_sub where x > 7
big_x(diamonds_sub, x > 7)

# Create a threshold column with value 100
diamonds_sub$threshold <- 100

# Use big_x() to find rows in diamonds_sub where x > 7
big_x(diamonds_sub, x > 7)


################################
# What to do?
#################################

# To avoid the problems caused by non-standard evaluation functions, you could avoid using them. In our example, we could achieve the same results by using standard subsetting (i.e. []) instead of filter(). For more insight into dealing with NSE and how to write your own non-standard evaluation functions, we recommend reading Hadley's vignette on the topic. Also, programming with the NSE functions in dplyr will be easier in a future version.

# http://rpubs.com/hadley/157957

# If you do need to use non-standard evaluation functions, it's up to you to provide protection against the problem cases. That means you need to know what the problem cases are, to check for them, and to fail explicitly.

# To see what that might look like, let's rewrite big_x() to fail for our problem cases.

# Write a check for each of the following:
# If x is not in names(df), stop with the message "df must contain variable called x". If threshold is in names(df), stop with the message "df must not contain variable called threshold". Remember to use the argument call. = FALSE in each call to stop() so that the call is not a part of the error message.

big_x <- function(df, threshold) {
  # Write a check for x not being in df
  if(!'x' %in% names(df)){
    stop('df must contain variable called x', call. = FALSE)
  }
  
  
  
  # Write a check for threshold being in df
    if('threshold' %in% names(df)){
    stop('df must not contain variable called threshold', call. = FALSE)
  }
  
  
  
  dplyr::filter(df, x > threshold)
}


################################
# Hidden Arguments
################################

# Pure functions
# 1. Their output only depends on their inputs
# 2. They don't affect the outside world except through their return value
# 3. Hidden arguments are function inputs that may be different for different users or sessions.
# 4. Common example arguments default that depend on the global options

# Viewing global options
# global options are the settings that affect your entire R session.
# options() will get the list of values of your global option

# Getting and setting options
# To get a value of an option, we use git option function
getOption("digits")
# 7 # How many digits to be shown when printing the numeric value

# To set the global options, use options function and set the argument
options(digits = 8) 

# To read about the ome of the commmon options read the wiki
?options

# A function should not affect the world which is outside its return value.
# A return value should never depend on a global option
# Legitimate to control side effects by global options


####################################
# A hidden dependence
####################################
 
# A classic example of a hidden dependence is the stringsAsFactors argument to the read.csv() function (and a few other data frame functions.) When you see the following code, you don't know exactly what the result will be:

pools <- read.csv("swimming_pools.csv")

# That's because if the argument stringsAsFactors isn't specified, it inherits its value from getOption("stringsAsFactors"), a global option that a user may change. Just to prove that this is the case, let's illustrate the problem.

# Use read.csv() as above to read in swimming_pools.csv and save it to the variable pools.
# Examine the structure of pools and notice how the columns Name and Address are factors.
# Change the global stringsAsFactors option to FALSE: options(stringsAsFactors = FALSE).
# Use read.csv() as above to read in the same CSV file to the variable pools2.
# Examine the structure of pools2, and notice how the columns Name and Address are now characters.

# Read in the swimming_pools.csv to pools
pools <- read.csv("swimming_pools.csv")

# Examine the structure of pools
str(pools)

# Change the global stringsAsFactor option to FALSE
options(stringAsFactors = FALSE)

# Read in the swimming_pools.csv to pools2
pools2 <- read.csv("swimming_pools.csv")

# Examine the structure of pools2
str(pools2)


##################################
# Legitimate use of options
##################################

# In general, you want to avoid having the return value of your own functions depend on any global options. That way, you and others can reason about your functions without needing to know the current state of the options.
# It is, however, okay to have side effects of a function depend on global options. For example, the print() function uses getOption("digits") as the default for the digits argument. This gives users some control over how results are displayed, but doesn't change the underlying computation.

# Let's take a look at an example function that uses a global default sensibly. The print.lm() function has the options digits with default max(3, getOption("digits") - 3).


# We've fit a regression model of fuel efficiency on weight using the mtcars dataset.
#Use summary() to take a look at the fitted regression model. Pay particular attention to number of decimal places reported.
#Set the global digits option to 2: options(digits = 2).
#Take another look at the fitted model using summary(). Notice the number of decimal places has changed, but there is no change to the underlying fit object.

# Fit a regression model
fit <- lm(mpg ~ wt, data = mtcars)

# Look at the summary of the model
summary(fit)

# Set the global digits option to 2
options(digits = 2)

# Take another look at the summary
summary(fit)


#########################
# Wrap up
#########################

#### Write a function, if you copy pasted two times, its time to write a function
# Solve a simple problem, before writing a function
# A good function is both correct and understandable


#### Functional programming
# Do not write for loops yourselves, use functions that use for loop (purrr) 
# remove duplicated code, by using the purrr function from the purrr package


##### Unsual inputs and outputs
# Deal with failure using safely()
# Iterate over two or more arguments
# Iterate functions for their side effects


##### Functions that are robust and do not surprise
# Use stop() and stopifnot() to fail early
# Avoid using type inconsistent functions in your own functions
# Avoid non-standard evaluation functions in your own functions
# Never rely on global options for computational details

###################
# Final thoughts
####################
# Your job is not to write an elegant code but to understand your data
# solve most pressing problem
# never feel bad about using the for loops
# Get a function that works right, for the easiest 80% of the problem
# In time you'll learn how to get to 99% with minimal extra effort 
# Concise and elegant code is something to strive towards!

