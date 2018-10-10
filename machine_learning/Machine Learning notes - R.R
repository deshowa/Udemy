### Udemy Machine Learning Class

#################################################
### Section 5 - Lecture 46- Multiple Linear Regression in R- Step 1
################################################

dataset = read.csv("Machine Learning A-Z/Part 2 - Regression/Section 5 - Multiple Linear Regression/50_startups.csv")

head(dataset)

### step 1 - prepare the data for the multiple regression:

# the state variable is categorical, so we need to modify for the machine learning equations, must encode

dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1, 2, 3))

# inspect:

head(dataset)


# split the dataset into training and test:

library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split==TRUE)
test_Set = subset(dataset, split ==FALSE)

nrow(dataset) == nrow(training_set) + nrow(test_Set) # test the data split


### step 2 - fitting the multiple regression:

# remember, you can press F1 in order to get the info for the formula:
regressor = lm(formula = Profit ~ R.D.Spend + Administration + State, data = training_set)
# alternative way to write the formula:
regressor = lm(formula = Profit ~ . , data = training_set)

### step 3 - look at the output of the regression:

summary(regressor) # notice that R already took care of creating the dummy variables for the state variable since it is a factor


### step 4 - final model

# after looking at the results, it appearsthat RD spend is the only significant variable; therefore, teacher says to eliminate all but RD spend
# would probably actually do recursive/backward elim to secure (probably next video)


#################################################
### Section 5 - Lecture 49- Multiple Linear Regression in R- Step 3
################################################


# now, we need to predict the test set results:

y_pred = predict(regressor, test_Set)

# compare:

as.data.frame(cbind(y_pred ,test_Set$Profit))


#################################################
### Section 5 - Lecture 50- Backward Elimination Homework- prep
################################################


# we built the model previously, but what if looking at some variables separately provided better predictions

# simpler than python - will run and copy and paste a bunch of times:

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State , data = dataset) # using entire dataset to have complete information for which variables are significant (correlation based)

summary(regressor)

# state2 looks like it has absolutely no effect on profit; state3 although lower, will not become signif if State2 removed
# so remove both State2 and State3:

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend , data = dataset) # using entire dataset to have complete information for which variables are significant (correlation based)

summary(regressor)

#################################################
### Section 5 - Lecture 51- Backward Elimination Homework - Answer
################################################

# let's work through doing a complete backward elimination:


# started here:
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend , data = dataset) # using entire dataset to have complete information for which variables are significant (correlation based)

summary(regressor)

# now, remove 'administration' :

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend , data = dataset) # using entire dataset to have complete information for which variables are significant (correlation based)

summary(regressor)

# so, now choice could be arbitrary- since p-value is 0.06, and we are shooting for 0.05, could be subjective or based on another criterion


# here is the formula for what we just did:

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)


#################################################
### Section 6 - Lecture 61- Polynomial Regression in R - step 1
################################################

# These are for where there is not a linear relationship between the variables, which is what we will cover in the future



dataset = read.csv("Machine Learning A-Z/Part 2 - Regression/Section 6 - Polynomial Regression/Position_Salaries.csv")
View(dataset)

# hiring new employee who was making $160K at previous company, let's see if it is truth or bluff from information of other company salaries

# need to exclude position in the model ; level = x, salary = y

dataset = dataset[2:3]

# train/test split - we will not split into training and test sets because of the small size of the dataset
# feature scaling - we will not need this since we are just using position type component and transformaing into other Xs

#################################################
### Section 6 - Lecture 62- Polynomial Regression in R - step 2
################################################

# will build a linear regression and polynomial regression model for this to prove that poly is better

lin_reg = lm(formula = Salary ~ Level, data = dataset )

summary(lin_reg)

# polynomial regression:

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3

poly_reg = lm(formula = Salary ~ Level + Level2 + Level3, data  = dataset)


summary(poly_reg)

#################################################
### Section 6 - Lecture 63- Polynomial Regression in R - step 3
################################################

# visualizing the regression results:

library(ggplot2)

ggplot(data = dataset, aes(x = Level, y  = Salary)) + geom_point(colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') + 
  xlab('Level') +
  ylab('Salary')

# visualizing the polynomial regression results:

ggplot(data = dataset, aes(x = Level, y  = Salary)) + geom_point(colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') + 
  xlab('Level') +
  ylab('Salary')


# let's add an additional degree to improve the model:

dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ Level + Level2 + Level3 + Level4, data = dataset )

# visualizing polynomial regression results #2 : 

ggplot(data = dataset, aes(x = Level, y  = Salary)) + geom_point(colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression #2)') + 
  xlab('Level') +
  ylab('Salary')


#################################################
### Section 6 - Lecture 64- Polynomial Regression in R - step 4
################################################

# predicting the result based on the employee's years of experience:


# linear regression prediction:

y_pred_lin = predict(lin_reg, data.frame(Level = 6.5)) # predicted $330K  - much higher than the $160K EE said they received

# polynomial regression prediction:

y_pred_poly = predict(poly_reg, data.frame(Level = 6.5, 
                                           Level2 = 6.5^2, 
                                           Level3 = 6.5^3, 
                                           Level4 = 6.5^4))

#################################################
### Section 6 - Lecture 65- Regression Template in R
################################################

# Regression Template:

dataset = read.csv('Position Salaries.csv')
dataset = dataset[2:3]

# just adding more to visualization:

library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = data.frame(Level = x_grid))),
             colour = 'blue') +
  ggtitle('Truth or bluff (Regression Model') +
  xlab('Level') + 
  ylab('Salary')


#################################################
### Section 7 - Lecture 70- SVR in R
################################################


# using the regression template

dataset = read.csv("Machine Learning A-Z/Part 2 - Regression/Section 7 - Support Vector Regression_SVR/Position_Salaries.csv")
View(dataset)

# Fitting the SVR to the dataset

# using the e1071 package:
install.packages('e1071')
library(e1071)

regressor = svm(formula = Salary ~ Level, data = dataset, type = 'eps-regression') # using the Gaussian kernel

# let's predict the result:

y_pred = predict(regressor, data.frame(Level = 6.5))
print(y_pred)


# let's see the result graphically:

library(ggplot2)

ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')

# remember that the CEO salary is anomalous


