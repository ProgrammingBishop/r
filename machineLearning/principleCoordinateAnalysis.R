# Dataset: https://www.cs.toronto.edu/~kriz/cifar.html

library(pracma) # For dot()

# Working Directory Changed Relevant to Current User
setwd('/Users/Sbishop/Desktop/GitHub/R/appliedMachineLearning/week04')



# Global Variables
# ==================================================

# Category List for Loop / Indexing
categories <- c('airplane', 'automobile', 'bird', 'cat', 'deer', 'dog', 'frog', 'horse', 'ship', 'truck')

# List of Matrices Split from Original Dataset
images <- list('airplane'   = matrix(0, 3072, 6000), 
               'automobile' = matrix(0, 3072, 6000), 
               'bird'       = matrix(0, 3072, 6000), 
               'cat'        = matrix(0, 3072, 6000), 
               'deer'       = matrix(0, 3072, 6000), 
               'dog'        = matrix(0, 3072, 6000), 
               'frog'       = matrix(0, 3072, 6000), 
               'horse'      = matrix(0, 3072, 6000), 
               'ship'       = matrix(0, 3072, 6000), 
               'truck'      = matrix(0, 3072, 6000))

# Temporary Matrix for Intermediate Calculations
temp.matrix <- matrix(0, 1, 6000)

# List of Mean Images Derived from Each Image Category
mean.images <- list('airplane'   = matrix(0, 3072, 1), 
                    'automobile' = matrix(0, 3072, 1), 
                    'bird'       = matrix(0, 3072, 1), 
                    'cat'        = matrix(0, 3072, 1), 
                    'deer'       = matrix(0, 3072, 1), 
                    'dog'        = matrix(0, 3072, 1), 
                    'frog'       = matrix(0, 3072, 1), 
                    'horse'      = matrix(0, 3072, 1), 
                    'ship'       = matrix(0, 3072, 1), 
                    'truck'      = matrix(0, 3072, 1))

# List of All PCAs Derived from Each Category
pca <- vector( 'list', length = 10 )



# Methods
# ==================================================

# Plot Image
# ------------------------------
# Author:    Eric Huber
# Code Type: Source Code
# Code URL:  https://piazza.com/class/jchzguhsowz6n9?cid=436

drawImage <- function(image) {
    r <- image[   1:1024]
    g <- image[1025:2048]
    b <- image[2049:3072]
    
    image.matrix      <- rgb(r, g, b, maxColorValue = 255)
    dim(image.matrix) <- c(32, 32)
    image.matrix      <- t(image.matrix)
    
    grid.raster(image.matrix, interpolate = T)
}
# ------------------------------


# Reconstruct Image: Summation_(j = 1)^r [ ( u^T_j dot ( x_i - mean(x) ) ) * u_j ]
dot.prod.sums <- function(U, centered.image) 
{
    # Placeholder Column Updated Each r Iterations
    dot.summation <- matrix(0, 3072, 1)
    
    # Get Scaled Eigen Vector
    for ( j in 1:ncol(U) )
    {
        dot.summation[ , 1] <- 
            dot.summation[ , 1] + ( dot( t( U[ , j] ), centered.image[ , 1] ) * U[ , j] )
    }
    
    return( dot.summation )
}


# Reconstruct Images Given a Number of Principle Components
image.reconstruction <- function(cur.image, mean.image, U)
{
    # Subtract Mean Image for the Original Image
    centered.image <- cur.image - mean.image
    
    # Reconstruct the Image
    return( mean.image + dot.prod.sums( U, centered.image ) ) 
}


# Bar Plot for Error List Argumenet
error.bar.plot <- function(error.option, main.label)
{
    return(
        barplot( t(as.matrix(unlist(option1.error))), 
                 names.arg = c('airplane', 'automobile', 'bird',  'cat',  'deer', 
                               'dog'     , 'frog'      , 'horse', 'ship', 'truck'),
                 col = c('#e7eeca', '#cbdb9c', '#a2d396', '#8ecb83', '#5aba56',
                         '#388e43', '#407c3b', '#44935f', '#268257', '#096c42'),
                 border = NA,
                 main   = main.label,
                 xlab   = 'Image',
                 ylab   = 'Error',
                 las    = 2,
                 beside = TRUE )
    )
}


# Scatter Plot for Distances
perform.plot <- function(data.values, xmin, xmax, ymin, ymax, main.label)
{
    plot(data.values, xlim = c(xmin, xmax), ylim = c(ymin, ymax), pch = 16, las = 2,
         col = c('#e7eeca', '#cbdb9c', '#a2d396', '#8ecb83', '#5aba56',
                 '#388e43', '#407c3b', '#44935f', '#268257', '#096c42'),
         xlab = '', ylab = '', main = main.label)
    
    text( data.values[ , 1], data.values[ , 2], 
          c('airplane', 'automobile', 'bird',  'cat',  'deer', 
            'dog'     , 'frog'      , 'horse', 'ship', 'truck'),
          cex = 0.6, pos = 2 )
}



# Load Data / Seperate Into Categories
# ==================================================

# ------------------------------
# Author:         Matt (Slightly Modified for Assignment)
# Author Profile: https://stackoverflow.com/users/604003/matt
# Code Type:      Source Code
# Code URL:       https://stackoverflow.com/a/39672323

labels     <- read.table("data/batches.meta.txt")
features   <- matrix(0, 3072, 60000)
classes    <- matrix(0,    1, 60000)
num.images <- 10000

# Read all .bin Files into Data
for (cur.file in 1:6) 
{
    to.read <- file(paste("data/data_batch_", cur.file, ".bin", sep = ""), "rb")
    
    for(cur.img in 1:num.images) 
    {
        l <- readBin(to.read, integer(), size = 1, n = 1, endian = "big")
        f <- as.integer(readBin(to.read, raw(), size = 1, n = 3072, endian = "big"))
        c <- num.images * (cur.file - 1) + cur.img
        
        features[ , c] <- f
        classes [ , c] <- l + 1
    }
    
    close(to.read)
    remove(l, f, cur.file, cur.img, c, to.read)
}
# ------------------------------



# Seperate Dataset, Find Mean Images, & Get PCAs for Each Category
# ==================================================

# 1) Split Dataset into Seperate Categories 
# 2) Get Mean Image for Each Category
# 3) Get the PCAs for Each Category ~ Get All PCAs
for ( category in 1:length(categories) )
{
    images[[category]]      <- as.matrix( features[ , which(classes[1, ] == category) ] )             # 1
    mean.images[[category]] <- as.matrix( rowMeans( features[ , which(classes[1, ] == category) ] ) ) # 2
    pca[[category]]         <- prcomp( t(images[[category]]), retx = TRUE, center = TRUE)             # 3
}



# PART I ~ Calculate Error Derived from Unused PCA Values
# ////////////////////////////////////////////////////////////////////////////////////////////////////

# Option 1 ~ Sum of Unused Eigenvalues
# --------------------------------------------------

# List to Hold Error Per Category
option1.error <- list('airplane' = 0, 'automobile' = 0, 'bird'  = 0, 'cat'  = 0, 'deer'  = 0, 
                      'dog'      = 0, 'frog'       = 0, 'horse' = 0, 'ship' = 0, 'truck' = 0)

# Total Error for Category = ( Sum Unused Eigenvalues ) / ( Sum All Eigenvalues )
for ( category in 1:length(option1.error) )
{
    option1.error[[category]] <- 
        # $sdev^2 is the Eigenvalue
        sum( pca[[category]]$sdev[21:3072]^2 ) 
}


# Option 2 ~ Image Reconstruction
# --------------------------------------------------

# Find the Mean Error For Each Category
option2.error <- list('airplane' = 0, 'automobile' = 0, 'bird'  = 0, 'cat'  = 0, 'deer'  = 0, 
                      'dog'      = 0, 'frog'       = 0, 'horse' = 0, 'ship' = 0, 'truck' = 0)

# Foe Every Category
for ( category in 1:length(categories) )
{
    # For Every Image in Current Category
    for ( cur.image in 1:ncol(images[[category]]) )
    {
        # Reconstruct Image Using Designated Principle Components
        reconstruct.image <- 
            image.reconstruction( images[[category]][ , cur.image], mean.images[[category]], pca[[category]]$rotation[ , 1:20] )
        
        # Get the Per-Pixel Difference Squared Column Vector
        per.pixel.diff <- ( images[[category]][ , cur.image] - reconstruct.image )^2
        
        # Sum Over the Whole Image ~ Scaler Value
        temp.matrix[ , cur.image] <- colSums(per.pixel.diff) / 3072
    }
    
    # Record the Error for Current Category
    option2.error[[category]] <- rowMeans(temp.matrix) / 6000
}



# Plot Error of Low-Dimensional Representation
# ==================================================

# Plot Error for Option 1 & 2
option1.plot <- error.bar.plot(option1.error, 'Error from Low-Dimensional Representation (Option 1)')
as.matrix(option1.error)

option2.plot <- error.bar.plot(option2.error, 'Error from Low-Dimensional Representation (Option 2)')
as.matrix(option2.error)


# PART II ~ Principal Coordinate Analysis
# ////////////////////////////////////////////////////////////////////////////////////////////////////

mean.matrix     <- matrix( unlist( mean.images ), 3072, 10 )
distance.matrix <- dist( t(mean.matrix), method = 'euclidean', upper = TRUE, diag = TRUE, p = 2 )
pca.coords      <- cmdscale( distance.matrix, k = 2 )   

# Display Distance Martrix
distance.matrix

# Plot Distances
perform.plot(pca.coords, -1100, 1300, -700, 700, 'Distances for Mean Images')



# PART III ~ Define the Class Similarities
# ////////////////////////////////////////////////////////////////////////////////////////////////////

# Matrix of Reconstruction Error
matrix.of.errors <- matrix(0, nrow = 10, ncol =  10)

# For Every Category
for ( category in 1:length(categories) )
{
    # And For Every PCA Set from Each Category
    for ( cur.pca in 1:length(categories) )
    {
        # Reconstruct Each Image of that Category for Each PCA
        for ( cur.image in 1:ncol(images[[category]]) )
        {
            # Get the Reconstructed Low-Dimensional Representation Column Vector
            reconstruct.image <- 
                image.reconstruction( images[[category]][ , cur.image], mean.images[[category]], pca[[cur.pca]]$rotation[ , 1:20] )
            
            # Get the Per-Pixel Difference Squared Column Vector
            per.pixel.diff <- ( images[[category]][ , cur.image] - reconstruct.image )^2
            
            # Sum Over the Whole Image ~ Scaler Value
            temp.matrix[ , cur.image] <- colSums(per.pixel.diff) / 3072
        }
        
        # Add Component to the Final Error Matrix
        matrix.of.errors[category, cur.pca] <- rowMeans(temp.matrix) / 6000
    } 
}

# Build the Distant Matrix
diff.dist.mat  <- matrix(0, nrow = 10, ncol = 10)
upper.triangle <- 1

# For Every Column
for ( col in 1:ncol(diff.dist.mat) )
{
    # For Every Row in Upper Triangle
    for ( row in 1:upper.triangle ) {
         # 1/2 * ( Error(Category 1 Given Category 2's PCs) + Error(Category 2 Given Category 1's PCs) )
         get.distance <- ( matrix.of.errors[col, row] + matrix.of.errors[row, col] ) / 2
         
         # Add to Upper Triangle and Lower Triangle Position
         diff.dist.mat[col, row] <- get.distance
         diff.dist.mat[row, col] <- get.distance
     } 
    
    # Increase the Number of Rows for the Next Columns Upper Diagonal Index
    upper.triangle <- upper.triangle + 1
}

# Display the Distance Matrix
diff.dist.mat

# Plot Distances
distance.matrix2 <- dist( diff.dist.mat, method = 'euclidean', upper = TRUE, diag = TRUE, p = 2 )
pca.diff.coords  <- cmdscale( distance.matrix2, k = 2 ) 

perform.plot(pca.diff.coords, -.125, .1, -.04, .04, 'Distances for Differences in Mean Images')
