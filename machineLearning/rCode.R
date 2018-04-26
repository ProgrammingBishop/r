# Libraries
# ====================================================================================================

library(Thermimage) # For mirror.matrix()
library(imager)     # For threshold()



# Setup Environment
# ====================================================================================================

setwd('/Users/Sbishop/Desktop/week8/rCode')
Sys.setenv( TZ = 'US' )
set.seed(498)



# Methods
# =====================================================================================

# Read Features
# --------------------------------------------------
# Author:         David Dalpiaz
# Author Profile: https://gist.github.com/daviddalpiaz
# File Name:      load_MNIST.R
# Code Type:      Source Code
# Code URL:       https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706
load.image.file = function(filename) {
    ret = list()
    f   = file(filename, 'rb')
    
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    
    n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    x    = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
    
    close(f)
    
    return( matrix(x, ncol = nrow * ncol, byrow = TRUE) )
}


# Read Labels
# --------------------------------------------------
# Author:         David Dalpiaz
# Author Profile: https://gist.github.com/daviddalpiaz
# File Name:      load_MNIST.R
# Code Type:      Source Code
# Code URL:       https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706
load.label.file = function(filename) {
    f = file(filename, 'rb')
    
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    
    n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
    
    close(f)
    
    return( y )
}


# Plot MNIST Number
plot.black.white <- function( data )
{
    data <- image( mirror.matrix(data), col = gray(12:1/12) )
}


# Map Values to -1 or 1
binarize <- function( data )
{
    data <- data / 255
    data <- apply( data, c(1, 2), function(x) { ifelse( x >= .5, 1, -1 ) })
    return( data )
}


# Create Noise in Train Data
create.noise <- function( train, noise.row, noise.col )
{
    flipped <- train
    
    # For Every Column in Current Row Pairs
    for ( col in 1:ncol(noise.row) )
    {
        print( paste(noise.row[ , col], noise.col[ , col], sep = " ") )
        # Add Noise to Train Set by Flipping Value
        flipped[ noise.row[ , col], noise.col[ , col] ] <- flipped[ noise.row[ , col], noise.col[ , col] ] * -1
    }
    
    return( flipped )
}


# Get Neighbors of pi_i
get.neighbors <- function( coords )
{
    # Store of Neighbors
    find.neighbors <- vector( mode = 'numeric', 0 )
    
    # Find Neighbors of Current pi_i
    for ( neighbor in 1:length(find.neighbors) )
    {
        if ( !is.null(coords$Image[ coords$Row - 1, coords$Column ]) && coords$Row - 1 != 0 ) 
        { find.neighbors[1] <- coords$Image[ coords$Row - 1, coords$Column ] } 
        
        if ( !is.null(coords$Image[ coords$Row + 1, coords$Column ]) && coords$Row + 1 != 0 ) 
        { find.neighbors[2] <- coords$Image[ coords$Row + 1, coords$Column ] } 
        
        if ( !is.null(coords$Image[ coords$Row, coords$Column - 1 ]) && coords$Column - 1 != 0 )
        { find.neighbors[3] <- coords$Image[ coords$Row, coords$Column - 1 ] } 
        
        if ( !is.null(coords$Image[ coords$Row, coords$Column + 1 ]) && coords$Column + 1 != 0 ) 
        { find.neighbors[4] <- coords$Image[ coords$Row, coords$Column + 1 ] } 
    } 
    
    # Only Return Found Neighbors
    return( na.omit(find.neighbors)[ 1:length( na.omit(find.neighbors) ) ] )
}


# Perform Mean-Field Inference for Each Row/Image via update.order
mean.field.inference <- function( pi_i, x_j, neighbors )
{
    # Summations for Numerator and Denominator
    first.numerator.component   <- lapply( neighbors, function(x) {  theta.hh * ( (2 * x) - 1 ) } )
    first.denominator.component <- lapply( neighbors, function(x) { -theta.hh * ( (2 * x) - 1 ) } )
    
    # Get Numerator and Denominator
    numerator   <- exp( do.call( sum, first.numerator.component ) + ( theta.hx * ( x_j ) ) )
    denominator <- numerator + exp( do.call( sum, first.denominator.component ) + ( -theta.hx * ( x_j ) ) )
    
    return( numerator / denominator )
}


# Transpose Initial Q Matrix
get.transpose <- function(Q.matrix)
{
    transpose.matrix <- Q.matrix
    
    for ( row in 1:nrow(Q.matrix) )
    {
        for ( col in 1:ncol(Q.matrix) )
        {
            transpose.matrix[row, col] <- Q.matrix[col, row]
        }
    }
    
    return( transpose.matrix )
}


# Get the Energy Value using Energy Formula
calculate.energy <- function(Q.matrix, X.matrix)
{
    # Find Log[Q]
    log.Q <- apply( Q.matrix, c(1, 2), function(x) { calculate.log.Q(x) } )
    
    # Vectors of PHX Results
    left.HX  <- vector( mode = 'numeric', (nrow(Q.matrix) * ncol(Q.matrix)) )
    right.HX <- vector( mode = 'numeric', (nrow(Q.matrix) * ncol(Q.matrix)) )
    
    # Populate PHX Vectors for Every Element in Q.matrix
    for ( col in 0:(ncol(Q.matrix) - 1) )
    {
        for( row in 0:(nrow(Q.matrix) - 1) )
        {
            # Get Left Component Using the r * c Element in Q.matrix
            left.HX [ ( row + col * ncol(Q.matrix) ) + 1 ] <- calculate.left.HX ( Q.matrix, col + 1, row + 1 ) 
            
            # Get Right Component Using the r * c Element in Q.matrix and in X.matrix
            right.HX[ ( row + col * ncol(Q.matrix) ) + 1 ] <- calculate.right.HX( Q.matrix[col + 1, row + 1], X.matrix[col + 1, row + 1] )
        }
    }
    
    # Calculate and Return the Energy
    e <- sum(log.Q) - ( sum(left.HX) + sum(right.HX) )
    
    return( e )
}


# Calculate log[Q] Component of Energy Equation
calculate.log.Q <- function(q_i)
{
    (      q_i  * log(      q_i  + 10^-10 ) ) +
    ( (1 - q_i) * log( (1 - q_i) + 10^-10 ) )
}


# Calculate Left Component for log P[H, X] in Energy Equation
calculate.left.HX <- function(Q, row, col)
{
    # Get Neighbors of pi_i
    neighbors <- get.neighbors( list( 'Image' = Q, 'Row' = row,  'Column' = col ) )
    
    # Calculate Left
    left <- lapply( neighbors, function(x) { 0.8 * ( (2 * x - 1) * (2 * Q[row, col] - 1) ) } )
    
    # Return the Sum to Vector Index in left.HX
    return( do.call( sum, left ) )
}


# Calculate Right Component for log P[H, X] in Energy Equation
calculate.right.HX <- function(q_i, x_j) 
{
    # Return to Vector Index in right.HX
    return( 2 * (2 * q_i - 1) * x_j )
}


# Identify the Hidden Variable
compute.final <- function(h) 
{
    if ( h > .5 ) { h <- 1 }
    else          { h <- 0 } 
    
    return(h)
}


# Begin Building Matrices for Denoised Images and Energy
perform.calculations <- function(Q, image, noise.row, noise.col, energy)
{
    # For 10 Iterations
    for ( iteration in 1:10 )
    {
        # Update q_i in Order Specified
        for ( col in 1:ncol(noise.row) )
        {
            # Get Neighbors of pi_i
            neighbors <- get.neighbors( list( 'Image' = Q, 'Row' = noise.row[ , col], 'Column' = noise.col[ , col] ) )
            
            # Update q_i
            Q[ noise.row[ , col], noise.col[ , col] ] <-
                mean.field.inference( Q    [ noise.row[ , col], noise.col[ , col] ], # Current pi_i
                                      image[ noise.row[ , col], noise.col[ , col] ], # Current x_j
                                      neighbors )                                    # Neighbors pi_j
        }
        
        # Get Energy for Image Iteration
        energy.matrix[energy, iteration + 1] <<- calculate.energy( Q, image )
    }
    
    return( Q )
}



# Load in Data
# =====================================================================================

# Train
train.x <- load.image.file( "../mnist/train-images-idx3-ubyte" )
train.y <- as.factor( load.label.file("../mnist/train-labels-idx1-ubyte") )

# Test
test.x <- load.image.file("../mnist/t10k-images-idx3-ubyte")
test.y <- as.factor( load.label.file("../mnist/t10k-labels-idx1-ubyte") )



# Obtaining the Dataset & Adding Pre-Determined Noise to Dataset
# =====================================================================================

# Load Noise CSV
noise <- read.csv( '../suplement/NoiseCoordinates.csv' )
noise <- noise[ , -1]
noise <- noise + 1

machine.x <- list( 'image1'  = t( create.noise( binarize( t( matrix( train.x[ 1, ], nrow = 28, ncol = 28 ) ) ), noise[1,  ], noise[2,  ] ) ),
                   'image2'  = t( create.noise( binarize( t( matrix( train.x[ 2, ], nrow = 28, ncol = 28 ) ) ), noise[3,  ], noise[4,  ] ) ),
                   'image3'  = t( create.noise( binarize( t( matrix( train.x[ 3, ], nrow = 28, ncol = 28 ) ) ), noise[5,  ], noise[6,  ] ) ),
                   'image4'  = t( create.noise( binarize( t( matrix( train.x[ 4, ], nrow = 28, ncol = 28 ) ) ), noise[7,  ], noise[8,  ] ) ),
                   'image5'  = t( create.noise( binarize( t( matrix( train.x[ 5, ], nrow = 28, ncol = 28 ) ) ), noise[9,  ], noise[10, ] ) ),
                   'image6'  = t( create.noise( binarize( t( matrix( train.x[ 6, ], nrow = 28, ncol = 28 ) ) ), noise[11, ], noise[12, ] ) ),
                   'image7'  = t( create.noise( binarize( t( matrix( train.x[ 7, ], nrow = 28, ncol = 28 ) ) ), noise[13, ], noise[14, ] ) ),
                   'image8'  = t( create.noise( binarize( t( matrix( train.x[ 8, ], nrow = 28, ncol = 28 ) ) ), noise[15, ], noise[16, ] ) ),
                   'image9'  = t( create.noise( binarize( t( matrix( train.x[ 9, ], nrow = 28, ncol = 28 ) ) ), noise[17, ], noise[18, ] ) ),
                   'image10' = t( create.noise( binarize( t( matrix( train.x[10, ], nrow = 28, ncol = 28 ) ) ), noise[19, ], noise[20, ] ) ),
                   'image11' = t( create.noise( binarize( t( matrix( train.x[11, ], nrow = 28, ncol = 28 ) ) ), noise[21, ], noise[22, ] ) ),
                   'image12' = t( create.noise( binarize( t( matrix( train.x[12, ], nrow = 28, ncol = 28 ) ) ), noise[23, ], noise[24, ] ) ),
                   'image13' = t( create.noise( binarize( t( matrix( train.x[13, ], nrow = 28, ncol = 28 ) ) ), noise[25, ], noise[26, ] ) ),
                   'image14' = t( create.noise( binarize( t( matrix( train.x[14, ], nrow = 28, ncol = 28 ) ) ), noise[27, ], noise[28, ] ) ),
                   'image15' = t( create.noise( binarize( t( matrix( train.x[15, ], nrow = 28, ncol = 28 ) ) ), noise[29, ], noise[30, ] ) ),
                   'image16' = t( create.noise( binarize( t( matrix( train.x[16, ], nrow = 28, ncol = 28 ) ) ), noise[31, ], noise[32, ] ) ),
                   'image17' = t( create.noise( binarize( t( matrix( train.x[17, ], nrow = 28, ncol = 28 ) ) ), noise[33, ], noise[34, ] ) ),
                   'image18' = t( create.noise( binarize( t( matrix( train.x[18, ], nrow = 28, ncol = 28 ) ) ), noise[35, ], noise[36, ] ) ),
                   'image19' = t( create.noise( binarize( t( matrix( train.x[19, ], nrow = 28, ncol = 28 ) ) ), noise[37, ], noise[38, ] ) ),
                   'image20' = t( create.noise( binarize( t( matrix( train.x[20, ], nrow = 28, ncol = 28 ) ) ), noise[39, ], noise[40, ] ) ) )



# Building Boltzman Machine for Denoising Images Using Mean-Field Inference
# =====================================================================================

# Load Update Order
update.order <- read.csv( '../suplement/UpdateOrderCoordinates.csv' )
update.order <- update.order[ , -1]
update.order <- update.order + 1

# Set Initial Theta
theta.hh <- 0.8 # Edge Between Hidden Nodes
theta.hx <- 2 # Edge Between Hidden Node and Known Node

# Load Initial Q Matrix
init.Q <- read.csv( '../suplement/InitialParametersModel.csv', header = FALSE )
Q      <- get.transpose(init.Q)



# Run Mean-Field Inference for 10 Iterations ~ Q Updated in order displayed in update.order
# =====================================================================================

# Store Q Matrix for Each Image
Q.store <- list( 'image1'  = Q, 'image2'  = Q, 'image3'  = Q, 'image4'  = Q, 'image5'  = Q, 
                 'image6'  = Q, 'image7'  = Q, 'image8'  = Q, 'image9'  = Q, 'image10' = Q,
                 'image11' = Q, 'image12' = Q, 'image13' = Q, 'image14' = Q, 'image15' = Q, 
                 'image16' = Q, 'image17' = Q, 'image18' = Q, 'image19' = Q, 'image20' = Q )

# Track the Energy and Denoised Image Matrices
energy.matrix <- matrix( 0, ncol =  11, nrow = 20 )

# Get the Initial Energy Values for First 10 Images
for ( i in 1:nrow(energy.matrix) )
{
    energy.matrix[i, 1] <- calculate.energy( Q.store[[i]], machine.x[[i]] )   
}

# Perform Calculations for each Image
calculations <- list( 'image1'  = perform.calculations( Q.store$image1,  t( machine.x$image1 ), update.order[1,  ], update.order[2,  ],  1 ), 
                      'image2'  = perform.calculations( Q.store$image2,  t( machine.x$image2 ), update.order[3,  ], update.order[4,  ],  2 ), 
                      'image3'  = perform.calculations( Q.store$image3,  t( machine.x$image3 ), update.order[5,  ], update.order[6,  ],  3 ), 
                      'image4'  = perform.calculations( Q.store$image4,  t( machine.x$image4 ), update.order[7,  ], update.order[8,  ],  4 ), 
                      'image5'  = perform.calculations( Q.store$image5,  t( machine.x$image5 ), update.order[9,  ], update.order[10, ],  5 ), 
                      'image6'  = perform.calculations( Q.store$image6,  t( machine.x$image6 ), update.order[11, ], update.order[12, ],  6 ), 
                      'image7'  = perform.calculations( Q.store$image7,  t( machine.x$image7 ), update.order[13, ], update.order[14, ],  7 ), 
                      'image8'  = perform.calculations( Q.store$image8,  t( machine.x$image8 ), update.order[15, ], update.order[16, ],  8 ), 
                      'image9'  = perform.calculations( Q.store$image9,  t( machine.x$image9 ), update.order[17, ], update.order[18, ],  9 ), 
                      'image10' = perform.calculations( Q.store$image10, t( machine.x$image10), update.order[19, ], update.order[20, ], 10 ),
                      'image11' = perform.calculations( Q.store$image11, t( machine.x$image11), update.order[21, ], update.order[22, ], 11 ), 
                      'image12' = perform.calculations( Q.store$image12, t( machine.x$image12), update.order[23, ], update.order[24, ], 12 ), 
                      'image13' = perform.calculations( Q.store$image13, t( machine.x$image13), update.order[25, ], update.order[26, ], 13 ), 
                      'image14' = perform.calculations( Q.store$image14, t( machine.x$image14), update.order[27, ], update.order[28, ], 14 ), 
                      'image15' = perform.calculations( Q.store$image15, t( machine.x$image15), update.order[29, ], update.order[30, ], 15 ), 
                      'image16' = perform.calculations( Q.store$image16, t( machine.x$image16), update.order[31, ], update.order[32, ], 16 ), 
                      'image17' = perform.calculations( Q.store$image17, t( machine.x$image17), update.order[33, ], update.order[34, ], 17 ), 
                      'image18' = perform.calculations( Q.store$image18, t( machine.x$image18), update.order[35, ], update.order[36, ], 18 ), 
                      'image19' = perform.calculations( Q.store$image19, t( machine.x$image19), update.order[37, ], update.order[38, ], 19 ), 
                      'image20' = perform.calculations( Q.store$image20, t( machine.x$image20), update.order[39, ], update.order[40, ], 20 ) )



# Get Denoised Matrix for Images
# =====================================================================================

# Matrix to Store Denoised Images
denoise.matrix <- matrix( 0, nrow = 28 )

# For Every Q Matrix, Binarize Pixels and Bind to Master Matrix
for ( current.image in 1:length(calculations) )
{
    reconstructed  <- apply( matrix( unlist(calculations[[current.image]]), ncol = 28, nrow = 28 ), c(1, 2), function(x) { compute.final(x) } )
    denoise.matrix <- cbind(denoise.matrix, reconstructed)
}

# Eleminate Placeholder Column
denoise.matrix <- denoise.matrix[ , -1]



# Example Answers
# =====================================================================================

sample.denoise <- read.csv( '../suplement/SampleDenoised.csv', header = FALSE )
sample.energy  <- read.csv( '../suplement/EnergySamples.csv',  header = FALSE )



# Output .CSV
# =====================================================================================

write.table(denoise.matrix[ ,281:560],  sep=",", file = "../csv/denoised.csv", row.names = FALSE, col.names = FALSE)
write.table(energy.matrix [11:12, 1:2], sep=",", file = "../csv/energy.csv",   row.names = FALSE, col.names = FALSE)
