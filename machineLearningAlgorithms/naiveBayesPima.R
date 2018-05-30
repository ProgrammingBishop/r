# Dataset: https://www.kaggle.com/uciml/pima-indians-diabetes-database

library(data.table)
library(ggplot2)
library(lattice)
library(caret)

# Prima Indians dataset
# =======================================================================================================================================
df <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data')
set.seed(498)


# Methods
# =======================================================================================================================================
# Seperate data into training and test sets
data.partition <- function()
{
    split      <- createDataPartition(df$V9, p = 0.80, list = FALSE)
    data.split <- list("test"  = df[ -split, ], 
                       "train" = df[  split, ])
    return(data.split)
}

# Seperate training class by label (positive = 1, negative = 0)
class.partition <- function(data.split)
{
    splitter    <- data.split$train$V9 > 0
    class.split <- list("Positive" = data.split$train[ , 1:8][ splitter, ],
                        "Negative" = data.split$train[ , 1:8][!splitter, ])
    return(class.split)
}

# Get the training parameters for training set for each class label (m and sd)
get.parameters <- function(class.split)
{
    parameters <- list("Positive_Mean" = sapply(class.split$Positive, mean, na.rm = TRUE),
                       "Negative_Mean" = sapply(class.split$Negative, mean, na.rm = TRUE),
                       "Positive_SD"   = sapply(class.split$Positive, sd, na.rm   = TRUE),
                       "Negative_SD"   = sapply(class.split$Negative, sd, na.rm   = TRUE))
    return(parameters)
}

# Perform dnorm() on each instance
perform.gpdf <- function(attribute, mean, sd) {
    return ( dnorm(attribute, mean, sd, log  = TRUE) ) 
}

# Get the probability that each instance would be labeled for each class
get.gpdf <- function(set, parameters)
{
    # Parameter matrices to hold Gaussian Probability Density values for each instance's attributes
    pos.parameters <- matrix( 0, nrow = nrow(set), ncol = ncol(set[ , 1:8]) )
    neg.parameters <- matrix( 0, nrow = nrow(set), ncol = ncol(set[ , 1:8]) )
    
    # For every instance
    for (row in seq_along(1:nrow(set) ) )
    {
        # and for every attribute for each instance
        for (col in seq_along(1:ncol(set[ , 1:8]) ) )
        {
            # Calculatew the Gaussian Probability Density attribute for the negative class
            neg.parameters[as.integer(row), as.integer(col)] <- 
                perform.gpdf(set                      [[as.integer(row), as.integer(col)]],
                             parameters$Negative_Mean [[as.integer(col)]], 
                             parameters$Negative_SD   [[as.integer(col)]])
            
            # Calculatew the Gaussian Probability Density attribute for the positive class
            pos.parameters[as.integer(row), as.integer(col)] <- 
                perform.gpdf(set                      [[as.integer(row), as.integer(col)]],
                             parameters$Positive_Mean [[as.integer(col)]], 
                             parameters$Positive_SD   [[as.integer(col)]])
        }
    }
    
    # Return Gaussian Probability Density probabilities
    return ( list("Positive_Probs" = rowSums(pos.parameters),
                  "Negative_Probs" = rowSums(neg.parameters)) )
}

# Make predictions based on each instance's probability to be in each class
make.prediction <- function(gpdf, data.split)
{
    class.prediction <- gpdf$Positive_Probs  > gpdf$Negative_Probs
    matches          <- class.prediction    == data.split$V9
    accuracy         <- sum(matches) / ( sum(matches) + sum(!matches) )
    
    return(accuracy)
}



# Global variables
# =======================================================================================================================================
# Vectors to hold the partition scores for each iteration
train.score <- array(dim = 10)
test.score  <- array(dim = 10)



# Naive Bayes algorithm
# =======================================================================================================================================
# Repeatedly partition the training data (80%) and report accuracy
for (iteration in 1:10) 
{
    data.split             <- data.partition()                              # Return $train set and $test set
    class.split            <- class.partition(data.split)                   # Return $positive and $negative for train set
    parameters             <- get.parameters(class.split)                   # Return the mean and sd for the training set
    gpdf.train             <- get.gpdf(data.split$train, parameters)        # Return Gaussian Probability Density function for all data instance for each class
    gpdf.test              <- get.gpdf(data.split$test, parameters)         # Evaluate test set with the training set's parameters
    train.score[iteration] <- make.prediction(gpdf.train, data.split$train) # Return the accuracy of the training set's predictions
    test.score[iteration]  <- make.prediction(gpdf.test, data.split$test)   # Return the accuracy of the test set's predictions using the training parameters
}