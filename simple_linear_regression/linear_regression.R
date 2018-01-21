# Linear Regression

# Linear regression is used to predict the value of an outcome variable Y 
# based on one or more input predictor variables X.
# The aim is to establish a linear relationship (a mathematical formula) 
# between the predictor variable(s) and the response variable, 
# so that, we can use this formula to estimate the value of the response Y, 
# when only the predictors (Xs) values are known.

library("ggplot2")
library("dplyr")

# Importing the training dataset
train <- read.csv("C:/Users/lvuser/Documents/GitHub/R_Learnings/simple_linear_regression/data/train.csv", stringsAsFactors = FALSE)

# Checking for NA's and missing values
# column y has 1 missing value
# Adding the average value of y to the missing value
# Lets check the correlation with mean of y and removing the na from y
missing_values <- map_chr(train, function(x){sum(is.na(x))})
train[which(is.na(train$y)),] <- as.numeric(train %>% summarise(mean(y, na.rm=TRUE)))


# Check for the outliers
# Both the plots show that they are no outliers in the data
# Distribution is not skewed
train %>% ggplot(mapping = aes(x = x, y = y, colour = 'red')) + geom_point()
train %>% ggplot(mapping = aes(x = 1, y = x)) + geom_boxplot()
train %>% ggplot(mapping = aes(x = 1, y = y)) + geom_boxplot()



# Get the correlation between the two variables
# Correlation is a statistical measure that suggests the level of linear dependence between two variables,
# that occur in pair. 
# Its value is between -1 to +1
# Above 0 is positive correlation i.e. X is directly proportional to Y.
# Below 0 is negative correlation i.e. X is inversly proportional to Y.
# Value 0 suggests weak relation.
cor(train$x, train$y)
# The number is 0.99533 which is very high. That means x is a very good predictor of y


# Fitting a simple linear regression model
model1 <- lm(y ~ x, data = train)


# In Linear Regression, the Null Hypothesis is that the coefficients associated with the variables is equal to zero. 
# The alternate hypothesis is that the coefficients are not equal to zero 
# (i.e. there exists a relationship between the independent variable in question and the dependent variable).
# P value has 3 stars which means x is of very high statistical significance.
# P value is less than 0. Genraaly below 0.05 is considered good.
# R-Squared tells us is the proportion of variation in the dependent (response) variable that has been explained by this model.
# R square is 0.99 which shows very good variation between dependent variable(y) and independent variable(x).


# Visualizing the training set results
train %>% ggplot() + geom_point(mapping = aes(x = x, y = y, colour = 'red')) + geom_line(mapping = aes(x = x, y = model1$fitted.values, colour = 'blue')) +  ggtitle('X vs Y (Training set)') + labs(x ='X', y = 'Y')

# Reading the test dataset
test <- read.csv("C:/Users/lvuser/Documents/GitHub/R_Learnings/simple_linear_regression/data/test.csv", stringsAsFactors = FALSE)


# predicting the test data set
# Gives the y value from the new model
y_predicted <- predict(model1, newdata = test)


# plot the graph
test %>% ggplot() + geom_point(mapping = aes(x = x, y = y, colour = 'red')) + geom_line(mapping = aes(x = x, y = y_predicted , colour = 'blue')) +  ggtitle('X vs Y (Test set)') + labs(x ='X', y = 'Y')




