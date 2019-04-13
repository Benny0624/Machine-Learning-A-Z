# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor <- lm(formula = Profit ~ ., data = training_set)

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)

#Building the Optimal Model using Backward Elimination
regressor <- lm(formula = Profit ~ R.D.Spend + Administration +
                    Marketing.Spend + State, data = dataset)
summary(regressor)
regressor <- lm(formula = Profit ~ R.D.Spend + Administration +
                    Marketing.Spend, data = dataset)
summary(regressor)
regressor <- lm(formula = Profit ~ R.D.Spend +
                    Marketing.Spend, data = dataset)
summary(regressor)
regressor <- lm(formula = Profit ~ R.D.Spend , data = dataset)
summary(regressor)

#Backward Elimination Auto(p-value)
BackwardElim <- function(x, sl){
    numVars <- length(x)
    for (i in c(1:numVars)) {
        regressor <- lm(formula = Profit ~., data = x)
        regressor_Summary <- summary(regressor)
        maxVar <- max(regressor_Summary$coefficients[c(2:numVars), 4])
        if(maxVar > sl){
            j <- as.integer(which.max(regressor_Summary$coefficients[c(2:numVars), "Pr(>|t|)"]))
            x <- x[, -j]
        }
        numVars <- numVars - 1
    }
    return(summary(regressor))
}

BackwardElim(dataset, SL)

x <- summary(regressor)
as.integer(which.max(x$coefficients[,"Pr(>|t|)"]))

