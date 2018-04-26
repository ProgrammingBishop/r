# Dataset: http://yann.lecun.com/exdb/mnist/

library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(naivebayes)
library(imager)
library(h2o)

setwd('/Users/andre/Desktop/GitHub/R/appliedMachineLearning/week02/mnist')
set.seed(498)

# Methods
# =====================================================================================

# Author:         David Dalpiaz
# Author Profile: https://gist.github.com/daviddalpiaz
# File Name:      load_MNIST.R
# Code Type:      Source Code
# Code URL:       https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706

# Row is instance of number; Col is pixel (28 * 28 = 728)
load_image_file = function(filename) {
    ret = list()
    f   = file(filename, 'rb')
    
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    
    n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    x    = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
    
    close(f)
    
    matrix(x, ncol = nrow * ncol, byrow = TRUE)
}

# Author:         David Dalpiaz
# Author Profile: https://gist.github.com/daviddalpiaz
# File Name:      load_MNIST.R
# Code Type:      Source Code
# Code URL:       https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706

# Col 729 represents the label for each instance
load_label_file = function(filename) {
    f = file(filename, 'rb')
    
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    
    n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
    
    close(f)
    
    y
}

# Function to crop matrix next.row to be within a 20 * 20 strectched bounding box
make.bbox <- function(next.row)
{
    make.img   <- as.cimg (next.row, 28, 28)
    cropping   <- autocrop(make.img, color = 0)
    border.box <- resize  (cropping, size_x = 20, size_y = 20, interpolation_type = 1)
}

# Function name to columns
name.columns <- function(the.bbox)
{
    col.names <- array()
    
    for (col in seq_along( 1:(ncol(the.bbox) - 1) ) )
    {
        col.names[as.integer(col)] <- paste("Pixel", as.character(col), sep = "") 
    }
    col.names[ncol(the.bbox)] <- "label"
    
    return(col.names)
}



# Preprocess the MNIST Training Dataset for stretched 20 * 20 Bounding Box
# =====================================================================================

# Load in the features and labels for the train dataset
train      <- load_image_file("ubyte/train-images-idx3-ubyte")
train.bbox <- matrix(0, nrow = nrow(train), ncol = 20 * 20)

for (row in seq_along(1:nrow(train)))
{
    next.row <- matrix(train[as.integer(row), ], 28, 28)
    
    # Genrate a Bounding Box
    next.row    <- as.cimg(next.row)
    next.row    <- threshold(next.row, thr = 127)
    create.bbox <- make.bbox(next.row)
    
    # Unwrap into Bounding Box Dataset
    train.bbox[as.integer(row), ] = as.vector(create.bbox)
}

# Append labels column to the features matrix
train                <- cbind(train, labels = as.factor(load_label_file("ubyte/train-labels-idx1-ubyte")) )
train.bbox           <- cbind(train.bbox, y = as.factor(load_label_file("ubyte/train-labels-idx1-ubyte")) )
colnames(train.bbox) <- name.columns(train.bbox)



# Preprocess the MNIST Test Dataset for stretched 20 * 20 Bounding Box
# =====================================================================================

# Load in the features and labels for the test dataset
test      <- load_image_file("ubyte/t10k-images-idx3-ubyte")
test.bbox <- matrix(0, nrow = nrow(test), ncol = 20 * 20)

for (row in seq_along(1:nrow(test)))
{
    next.row <- matrix(test[as.integer(row), ], 28, 28)
    
    # Genrate a Bounding Box
    next.row    <- as.cimg(next.row)
    next.row    <- threshold(next.row, thr = 127)
    create.bbox <- make.bbox(next.row)
    
    # Unwrap into Bounding Box Dataset
    test.bbox[as.integer(row), ] = as.vector(create.bbox)
}

# Append labels column to the features matrix
test                <- cbind(test, labels = as.factor(load_label_file("ubyte/t10k-labels-idx1-ubyte")))
test.bbox           <- cbind(test.bbox, y = as.factor(load_label_file("ubyte/t10k-labels-idx1-ubyte")) )
colnames(test.bbox) <- name.columns(test.bbox)



# Naive Bayes ~ Gaussian and Bernoulli for Untouched and Stretched Bounded Box
# =====================================================================================

# Bernoulli ~ Categorical input 
train.categ <- as.data.frame(train)
test.categ  <- as.data.frame(test)

train.bbox.categ <- as.data.frame(train.bbox)
test.bbox.categ  <- as.data.frame(test.bbox)

for (col in seq_along(1:ncol(train.categ)))
{
    train.categ[ , as.integer(col)] <- factor(train.categ[ , as.integer(col)], levels=c(0,1))
    test.categ [ , as.integer(col)] <- factor(test.categ [ , as.integer(col)], levels=c(0,1))
}

for (col in seq_along(1:ncol(train.bbox.categ)))
{
    train.bbox.categ[ , as.integer(col)] <- factor(train.bbox.categ[ , as.integer(col)], levels=c(0,1))
    test.bbox.categ [ , as.integer(col)] <- factor(test.bbox.categ [ , as.integer(col)], levels=c(0,1))
}


    # Untouched
model.bern.u <- naive_bayes(train.categ[ , -785], train[ , 785], laplace = 1)
pred.bern.u  <- predict(model.bern.u, test.categ[ , -785])

confusionMatrix(pred.bern.u, test[ , 785])

    # Stretched Bounding Box
model.bern.bbox <- naive_bayes(train.bbox.categ[ , -401], train.bbox[ , 400], laplace = 1)
pred.bern.bbox  <- predict(model.bern.bbox, test.bbox.categ[ , -401])

confusionMatrix(pred.bern.bbox, test.bbox[ , 400])



# Gaussian ~ Use non-categorical input
    # Untouched
model.gauss.u <- naiveBayes(train[ , -785],
                            as.factor(train[ , 785]),
                            na.action = na.omit)

pred.gauss.u <- predict(model.gauss.u, test[ , -785], na.rm = TRUE)
confusionMatrix(pred.gauss.u, test[ , 785])

    # Stretched Bounding Box 
model.gauss.bbox <- naiveBayes(train.bbox[ , -401],
                               as.factor(train.bbox[ , 401]),
                               na.action = na.omit)

pred.gauss.bbox <- predict(model.gauss.bbox, test.bbox[ , -401], na.rm = TRUE)
confusionMatrix(pred.gauss.bbox, test.bbox[ , 401])



# Random Forest
# =====================================================================================

# Initiate h2o and Obtain h2o Objects
h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()

train.h2o         <- h2o.importFile(path = normalizePath("csv/mnist_train.csv"))
train.h2o[ , 785] <- as.factor(train.h2o[ , 785])

train.bbox.h2o         <- h2o.importFile(path = normalizePath("csv/mnist_train_bbox.csv"))
train.bbox.h2o[ , 401] <- as.factor(train.bbox.h2o[ , 401])

test.h2o         <- h2o.importFile(path = normalizePath("csv/mnist_test.csv" ))
test.h2o[ , 785] <- as.factor(test.h2o[ , 785])

test.bbox.h2o         <- h2o.importFile(path = normalizePath("csv/mnist_test_bbox.csv" ))
test.bbox.h2o[ , 401] <- as.factor(test.bbox.h2o[ , 401])



# Method to Perform Cross Validation via Random Forest
calc.random.forest <- function(train.h2o, test.h2o, num.tree, depth, test.labels) 
{
    model <- h2o.randomForest( training_frame = train.h2o, validation_frame = test.h2o,
                               x = 1:(ncol(train.h2o) - 1), y = ncol(train.h2o), 
                               ntrees = num.tree, max_depth = depth, seed = 498 )
    
    pred.rf    <- h2o.predict( model, test.h2o[ , 1:(ncol(train.h2o) - 1)] )
    pred.rf.df <- as.data.frame(pred.rf) 
    
    confusionMatrix( pred.rf.df[ , 1], test.labels[ , ncol(test.h2o)] )
}



# Random Forest Implementation via h2o for the Untouched Dataset
# =====================================================================================

# Depth 4
# -------------------------------------------------------------------------------------
# 10 Trees 
calc.random.forest(train.h2o, test.h2o, 10, 4, test)

# 20 Trees
calc.random.forest(train.h2o, test.h2o, 20, 4, test)

# 30 Trees
calc.random.forest(train.h2o, test.h2o, 30, 4, test)
    

# Depth 8
# -------------------------------------------------------------------------------------
# 10 Trees 
calc.random.forest(train.h2o, test.h2o, 10, 8, test)

# 20 Trees 
calc.random.forest(train.h2o, test.h2o, 20, 8, test)

# 30 Trees 
calc.random.forest(train.h2o, test.h2o, 30, 8, test)
    

# Depth 16
# -------------------------------------------------------------------------------------
# 10 Trees 
calc.random.forest(train.h2o, test.h2o, 10, 16, test)

# 20 Trees 
calc.random.forest(train.h2o, test.h2o, 20, 16, test)

# 30 Trees 
calc.random.forest(train.h2o, test.h2o, 30, 16, test)



# Random Forest Implementation via h2o for the Stretched Bounding Box Dataset
# =====================================================================================

# Depth 4
# -------------------------------------------------------------------------------------
# 10 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 10, 4, test.bbox)

# 20 Trees
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 20, 4, test.bbox)

# 30 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 30, 4, test.bbox)

    
# Depth 8
# -------------------------------------------------------------------------------------
# 10 Trees
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 10, 8, test.bbox)

# 20 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 20, 8, test.bbox)

# 30 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 30, 8, test.bbox)


# Depth 16
# -------------------------------------------------------------------------------------
# 10 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 10, 16, test.bbox)

# 20 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 20, 16, test.bbox)

# 30 Trees 
calc.random.forest(train.bbox.h2o, test.bbox.h2o, 30, 16, test.bbox)



# CSV Output 
# =====================================================================================

# Test CSV Output
write.csv(test,      file="csv/mnist_test.csv", row.names=FALSE)
write.csv(test.bbox, file="csv/mnist_test_bbox.csv", row.names=FALSE)

# Train CSV Output
write.csv(train,      file="csv/mnist_train.csv", row.names=FALSE)
write.csv(train.bbox, file="csv/mnist_train_bbox.csv", row.names=FALSE)