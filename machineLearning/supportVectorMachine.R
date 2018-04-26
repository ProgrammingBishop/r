# Dataset: https://archive.ics.uci.edu/ml/datasets/Adult

library(dplyr)
library(klaR)
library(lattice)
library(ggplot2)
library(caret)
library(gdata) # For unmatrix()

# ========================================================================
# Read in the Dataset

setwd("/Users/Sbishop/Desktop/GitHub/R/appliedMachineLearning/week03/")
set.seed(498) # Set seed so we can repeat analysis with same values when partitioning

# Get test.csv and adult.csv ~ Need to merge the datasets and then perform the splits (@392 in Piazza)
adult.data <- read.csv('data/adult.data', header = FALSE)
adult.test <- read.csv('data/adult.test', header = FALSE)

# Remove the first row of adult.test as it is NA
adult.test <- adult.test[c(2:nrow(adult.test)), c(1:ncol(adult.test))] 

# Combine the two data sets into 1
df <- rbind.data.frame(adult.data, adult.test)



# ========================================================================
# Using Continuous Variables

#sapply(as.numeric) because rbind coerced into characters
features <- df[ c(1, 3, 5, 11, 12, 13) ] %>% sapply(as.numeric)
labels   <- df[ , 15]



# ========================================================================
# Rescale the Labels

# Remove the '.' from the test portion of the labels
labels <- sapply( labels, function(x) { substr(x, 1, 5) } )

# Convert values to -1 for under or equal 50k and 1 for over 50k
labels <- sapply( labels, function(x) { ifelse(x == " <=50", -1, 1) } )



# ========================================================================
# Normalization of the Data

tr.mean       <- apply(features, 2, mean, na.rm = T)
tr.sd         <- apply(features, 2, sd,   na.rm = T)  
x.mean        <- t( t(features) - tr.mean )
features.norm <- t( t(x.mean)   / tr.sd   )



# ========================================================================
# Creating Cross Validation

# 80% Train Set
df.part <- createDataPartition(y = labels, p = 0.8, list = FALSE)
train.x <- features.norm[df.part, ] 
train.y <- labels       [df.part  ]

# Partition Remaining Data
remain.x     <- features.norm[-df.part, ]
remain.y     <- labels       [-df.part  ]
remain.split <- createDataPartition(y = remain.y, p = 0.5, list = FALSE)

# 10% Validation Set
valx <- remain.x[remain.split, ]
valy <- remain.y[remain.split  ]

# 10% Test Set
testx <- remain.x[-remain.split, ]
testy <- remain.y[-remain.split  ]



# ======================================================
# Loop Parameters

reg.const <- c(0.0001, 0.001, 0.01, 0.1, 1)
batch     <- 1 
steps     <- 300
eval.step <- 30
epochs    <- 50

# Results for Each lamda Every 30 Steps
magnitude.df           <- as.data.frame(matrix(0, nrow = (epochs * steps / 30), ncol = length(reg.const))) 
colnames(magnitude.df) <- as.character(reg.const) # Make columns the lamda values

accuracy.df            <- as.data.frame(matrix(0, nrow = (epochs * steps / 30), ncol = length(reg.const))) 
colnames(accuracy.df)  <- as.character(reg.const) # Make columns the lamda values



# ======================================================
# Methods

# Returns:
    # Held Out Set of 50 for Epoch's Evaluation; 
    # Training Set Excluding Held Out Set
get.epoch.sample <- function(x, y)
{
    # Held Out Set to Evaluate
    val.instances <- matrix(0, nrow = 50, ncol = 6)
    val.classes   <- matrix(0, nrow = 50, ncol = 1) 
    
    # Untocuhed Set to Train ~ (Return Set Excluding Rows of Held Out Set)
    train.instances <- x 
    train.classes   <- y
    
    for(row in 1:nrow(val.classes))
    {
        random.sample <- sample(1:length(y), 1)
        
        val.instances[row, ] <- x[random.sample, ]
        val.classes  [row  ] <- y[random.sample  ]
        
        train.instances <- train.instances[ -random.sample, ]
        train.classes   <- train.classes  [ -random.sample  ]
    }
    
    return( list("val.instances"   = val.instances, 
                 "val.classes"     = val.classes,
                 "train.instances" = train.instances,
                 "train.classes"   = train.classes) )
}


get.accuracy <- function(x, y, a, b)
{
    accuracy <- 0
    
    # For Every Row in Epoch Train Set
    for(row in 1:nrow(y))
    {
        # Calculate sign(ax + b) (Result should be -1 or 1)
        result <- sign( sum(a * x[row, ]) + b )
        
        # Compare the result with y
        if ( result == y[row] ) 
        {
            accuracy <- accuracy + 1
        }
    }
    
    return( accuracy / nrow(x) )
}


# Calculate Gradient Descent and Updates the Step
update.step <- function(epoch.sample, step.size, a, b)
{
    for (cur.batch in 1:batch)
    {
        # Get Random Item from Epoch Train Set
        r  <- sample( 1:nrow(epoch.sample$train.instances), 1 )
        rx <- epoch.sample$train.instances[r, ]
        ry <- epoch.sample$train.classes  [r  ]
        
        # Compute gradient; Update the Model
        gamma  <- sum(a * rx) + b
        result <- gamma * ry
        
        if (result >= 1) 
        {
            a <- a - (step.size * cur.lamda * a)
            b <- b
        }
        
        else 
        {
            a <- a - (step.size * ( (cur.lamda * a) - (ry * rx) ) )
            b <- b + (step.size * ry) 
        }
    }
    
    return( list("a" = a,
                 "b" = b) )
}


# Train the SVM with SGD
execute.epoch <- function(x, y, cur.lamda, batch, steps, eval.step, epoch, acc.df, mag.df)
{
    a        <- 0
    b        <- 0
    plot.num <- 1
    
    # For each Epoch
    for (cur.epoch in 1:epochs)
    {
        step.size <- 1 / ( (0.01 * cur.epoch) + 50 ) 
        
        # Get 50 Random Samples from Train for this Epoch ~ (Evaluate training on this set; don't touch)
        #(Page 34 Procedure 3.3 Bullet 2)
        epoch.sample <- get.epoch.sample(x, y) 
        
        # For each Step
        for (cur.step in 1:steps)
        {
            step.update = update.step(epoch.sample, step.size, a, b)
            
            a <- step.update$a
            b <- step.update$b
            
            if (cur.step %% 30 == 0)
            {
                # Calculate Accuracy on Held Out Set
                accuracy <- get.accuracy(epoch.sample$val.instances, epoch.sample$val.classes, a, b)
                
                # Store Accuracy to Accuracy Data Frame
                acc.df[ plot.num, as.character(cur.lamda) ] <- accuracy
                
                # Add magnitude of 'a' to Magnitude Matrix
                mag.df[ plot.num, as.character(cur.lamda) ] <- sqrt( sum(a * a) )
                
                plot.num = plot.num + 1
            }
        }
    }
    
    return( list( "accuracy.df"  = acc.df[ , as.character(cur.lamda)],
                  "magnitude.df" = mag.df[ , as.character(cur.lamda)],
                  "a" = a,
                  "b" = b) )
}



# ======================================================
# Train SVM: SGD ~ To fit an SVM with stochastic gradient descent

for (cur.lamda in reg.const)
{
    execution <- execute.epoch(train.x, train.y, cur.lamda, batch, steps, eval.step, epochs, accuracy.df, magnitude.df)
    
    accuracy.df [ , as.character(cur.lamda)] <- execution$accuracy.df
    magnitude.df[ , as.character(cur.lamda)] <- execution$magnitude.df
}



# ======================================================
# Display Plots

# Plot Held Out Accuracy Curves
visual <- matplot(accuracy.df, type = 'l', col = 1:5, xlab = 'Steps', ylab = "HoA", ylim = c(0, 1)) 
visual <- legend("bottomright", legend = c('1e-4', '1e-3', '1e-2', '1e-1', '1'), col = 1:5, pch = 1, lty = 'solid', lwd = 1) 

# Plot Magnitude Curves
visual <- matplot(magnitude.df, type = 'l', col = 1:5, xlab = 'Steps', ylab = "Magnitude") 
visual <- legend("bottomright", legend = c('1e-4', '1e-3', '1e-2', '1e-1', '1'), col = 1:5, pch = 1, lty = 'solid', lwd = 1) 



# ======================================================
# Select Best lamda and train the training and validation set

# ----------------
# Best Lamda: 1e-3

# Lamda equal to 1e-2, 1e-1, and 1 produce smaller magnitudes than 1e-3 and 1e-4. 
# If gamma_(i) and y_(i) have the same sign the classification will be correct, 
# but points nearby may be misclassified because the current point is close to the decision boundary.

# We want a high magnitude because we can assume cost of 0.
# This intuition is based on the current point being far enough away from the decision boundary 
# that nearby points are more likely to be classified correctly.

# Between 1e-3 and 1e-4, I will choose lamda of 1e-3 as my regularization constant because, 
# based on the plots, that accuracy seems more consistent.
# ----------------

# Merge Train and Validation
train.x.90 <- rbind(train.x, valx)
# unlist(train.y.90 because merge made as list)
train.y.90 <- rbind(as.matrix(train.y), as.matrix(valy)) %>% unmatrix()

# Results for Regularization Constant Every 30 Steps
magnitude.90.df <- as.data.frame(matrix(0, nrow = (epochs * steps / 30), ncol = 1)) 
accuracy.90.df  <- as.data.frame(matrix(0, nrow = (epochs * steps / 30), ncol = 1)) 

# Train with the 90% Data Set and the selected lamda
execution.90 <- execute.epoch(train.x.90, train.y.90, 0.001, batch, steps, eval.step, epochs, accuracy.90.df, magnitude.90.df)



# ======================================================
# Evaluate on the Untouched Test Set

# With a set.seed(498) the accuracy is 77.21%
accuracy.final <- get.accuracy(testx, as.matrix(testy), execution.90$a, execution.90$b)
accuracy.final
