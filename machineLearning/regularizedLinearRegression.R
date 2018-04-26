# Libraries
# ====================================================================================================
library(lattice)           # For caret library
library(ggplot2)           # For caret library
library(caret)             # For createDataPartition
library(glmnet)            # For cv.glmnet

# Set Up Environment
setwd("/Users/Sbishop/Desktop/week06/")
set.seed(498)

# 4 Figures ~ 2 Rows & 2 Columns
par( mfrow = c( 2, 2 ) )


# Load in Data
# ====================================================================================================

default <- as.data.frame( read.csv('tracks.csv', header = TRUE, na.strings = c("") ) ) 

# Eliminate Unnecessary Row and Column
default <- default[-1, -1]


# Test Data and Train Data Partition
# ====================================================================================================

# Create Data Partition Test/Train ~ 20/80
partition  <- createDataPartition(default$Y, p = 0.80, list = FALSE)
split.data <- list("test"  = default[ -partition, ], 
                   "train" = default[  partition, ]) 


# Logistic Regression glmnet()
# ====================================================================================================

# Logistic Regression with glmnet() on Train Set
logistic.model <- function(alpha.value)
{
    model.cv <- cv.glmnet( x            = data.matrix(        split.data$train[ , -24] ), 
                           y            = data.matrix( factor(split.data$train[ ,  24] ) ),
                           type.measure = 'class',
                           nfolds       = 30,
                           family       = 'binomial',
                           alpha        = alpha.value )
    
    return(model.cv)
}

# Assure Features Compatible for Predict (No Factors)
make.numeric <- apply( split.data$test[ , 1:23], c(1, 2), as.integer )

# Build Classifier with Regression Schemes
models <- list ( 'ridge' = logistic.model(0),
                 'lasso' = logistic.model(1),
                 'net'   = logistic.model(.5) )

# Plots for Missclassification Error
plot(models$ridge, main = 'Ridge Regression\n')
plot(models$lasso, main = 'Lasso Regression\n')
plot(models$net,   main = 'Net Regression\n')

# Predictions for Each Regrssion Scheme
prediction.ridge <- predict( object = models$ridge, newx = make.numeric, type = 'class' )
prediction.lasso <- predict( object = models$lasso, newx = make.numeric, type = 'class' )
prediction.net   <- predict( object = models$net,   newx = make.numeric, type = 'class' )

# Confusion Matrix for Each Regression Scheme
accuracy.ridge <- confusionMatrix( prediction.ridge, split.data$test$Y )
accuracy.lasso <- confusionMatrix( prediction.lasso, split.data$test$Y )
accuracy.net   <- confusionMatrix( prediction.net,   split.data$test$Y )





