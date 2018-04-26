# Libraries
# ====================================================================================================

library(jpeg)        # For readJPEG() 
library(imager)      # For load.image()
library(matrixStats) # For logSumExp()
library(pracma)      # For dot()
library(matrixStats) # For logSumExp()



# Methods
# ====================================================================================================

# Convert JPEG to Matrix: Each Row = Pixel; Each Column = Channel
image.data.frame <- function(jpeg.obj) 
{
    jpeg.obj <- data.frame( "Red"   = matrix(jpeg.obj[ , , 1], ncol = 1), 
                            "Green" = matrix(jpeg.obj[ , , 2], ncol = 1), 
                            "Blue"  = matrix(jpeg.obj[ , , 3], ncol = 1) )
    
    # Convert to 0 - 255 Scale
    jpeg.obj <- scale.image(jpeg.obj)
    
    return(jpeg.obj)
}


# Save JPG Image & Plot
output.image <- function(img.col, img.row, image, filename, resolution) 
{
    # Unscale Image
    jpeg.array <- unscale.image(image)
    
    # Seperate RGB Channels
    r <- jpeg.array[ , 1]
    g <- jpeg.array[ , 2]
    b <- jpeg.array[ , 3]
    
    # Create 3D Array
    jpeg.array <- array( c( r, g, b ), dim = c( img.row, img.col, 3 ) )
    
    # Save the Image
    writeJPEG( image = jpeg.array, target = filename, quality = resolution )
    
    # Plot Image
    jpeg.image <- load.image(filename)
}


# Scale Image Data Frame from 0 - 1 to 0 - 255
scale.image <- function(image) 
{
    return( apply( image, 2, function(x) { floor( x * 255 ) } ) )
}


# Unscale Image Data Frame from 0 - 255 to 0 - 1
unscale.image <- function(image)
{
    return( apply( image, 2, function(x) { x / 255 } ) )
}


# Get Number Points for Each Cluster Center
find.pi <- function(cluster.points)
{
    # Matrix to Store Output
    proportion <- matrix( 0, ncol = 1, nrow = length( unique(cluster.points) ) )
    
    # Get Count per Center
    for ( i in 1:nrow(proportion) )
    {
        proportion[i, ] <- sum( cluster.points == i ) / length(cluster.points)
    }
    
    return(proportion)
}


# E-Step
e.step <- function(df, mu, pi)
{
    # Get Euclidean Distances and Closest Cluster for Each Pixel
    min.distance <- find.small.distance( df, mu )
    
    # Matrix: Rows = Pixels; Columns = Cluster
    w <- matrix( 0, nrow = nrow(df), ncol = (nrow(mu)) )
    
    for ( j in 1:ncol(w) )
    {
        w[ , j] <- pi[j, ] * exp( -1/2 * ( min.distance$distance[ , j] - 
                                           min.distance$distance[cbind(seq_along(min.distance$closest), 
                                                                       min.distance$closest)] ) )
    }
    
    # Divide by Denominator
    denominator <- rowSums(w)
    w <- apply( w, 2, function(x) { x / denominator } )
    
    return(w)
}


# Get Euclidean Distances and Closest Cluster for Each Pixel
find.small.distance <- function(df, mu)
{
    # Matrix to Hold Square Distances
    distances <- matrix( 0, nrow = nrow(df), ncol = nrow(mu) )
    
    # For Every Pixel
    for ( i in 1:nrow(df) )
    {
        # For Every Cluster Center
        for ( j in 1:nrow(mu) )
        {
            # Find Distance Between Pixel and Cluster Center
            distances[i, j] <- dot( (df[i, ] - mu[j, ]), t(df[i, ] - mu[j, ]) )
        }
    }
    
    # Find Index of Closest Center
    closest.center <- apply( distances, 1, which.min )
    
    return( list( 'distance' = distances,
                  'closest'  = closest.center ) )
}


# M-Step
m.step <- function(df, w)
{
    new.mu <- matrix( 0, nrow = ncol(w), ncol = 3 )
    new.pi <- matrix( 0, nrow = ncol(w), ncol = 1 )
    
    for ( j in 1:ncol(w) )
    {
        new.mu[j, ] <- colSums( df * w[ , j] ) / sum( w[ , j] )
        new.pi[j, ] <- sum( w[ , j] ) / nrow(df)
     }
    
    return( list( 'mu' = new.mu,
                  'pi' = new.pi ) )
}


# Vector of Mean Distances
is.converged <- function(old, new)
{
    distance <- matrix( 0, nrow = nrow(old), ncol = 1 )
    
    for ( i in 1:nrow(old) )
    {
        distance[i, ] <- dot( (old[i, ] - new[i, ]), t(old[i, ] - new[i, ]) )
    }
    
    return( sum(distance) / nrow(distance) )
}


# Load Data
# ====================================================================================================

# Initial Seed
# set.seed(498)
setwd('/Users/Sbishop/Desktop/hw7')

# Read JPEG Files
raw.JPEG <- list( 'guppy'      <- readJPEG('imageData/guppy.jpg'),      # 640 * 480 = 307200
                  'strelitzia' <- readJPEG('imageData/strelitzia.jpg'), # 600 * 399 = 239400
                  'sunset'     <- readJPEG('imageData/sunset.jpg') )    # 600 * 330 = 198000


# Convert JPEG OBJ into Channel Data Frame
images.df <- list( 'guppy.df'      = image.data.frame(raw.JPEG[[1]]),
                   'strelitzia.df' = image.data.frame(raw.JPEG[[2]]),
                   'sunset.df'     = image.data.frame(raw.JPEG[[3]]) )





# K-Means for All Images Using 3 Different Segmentation Parameters
# ====================================================================================================

# Perform K-Means
thetas <- list( 'guppy'      = list( '10' = kmeans( x = images.df[[1]], centers = 10, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '20' = kmeans( x = images.df[[1]], centers = 20, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '50' = kmeans( x = images.df[[1]], centers = 50, algorithm = "Hartigan-Wong", iter.max = 100 ) ),
                'strelitzia' = list( '10' = kmeans( x = images.df[[2]], centers = 10, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '20' = kmeans( x = images.df[[2]], centers = 20, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '50' = kmeans( x = images.df[[2]], centers = 50, algorithm = "Hartigan-Wong", iter.max = 100 ) ),
                'sunset'     = list( '10' = kmeans( x = images.df[[3]], centers = 10, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '20' = kmeans( x = images.df[[3]], centers = 20, algorithm = "Hartigan-Wong", iter.max = 100 ),
                                     '50' = kmeans( x = images.df[[3]], centers = 50, algorithm = "Hartigan-Wong", iter.max = 100 ) ) )



# Perform EM and Reconstruction for All in thetas
# ====================================================================================================

# For Every Image ( 9 Iterations)
for ( cur.img in 1:length(thetas) )
{
    # And for Every K-Means Result
    for ( segment in 1:length(thetas) )
    {
        # Get Initial Values for Theta
        theta.mu <- thetas[[cur.img]][[segment]]$centers
        theta.pi <- find.pi(thetas[[cur.img]][[segment]]$cluster)
        
        # Get Updated Values for Theta
        new.mu <- matrix( 0, nrow = nrow(theta.mu), ncol = 3 )
        new.pi <- matrix( 0, nrow = nrow(theta.pi), ncol = 1 )
        
        difference <- .Machine$double.xmax
        steps <- 1
        
        # EM Algorithm Until Convergence
        while( abs(is.converged(theta.mu, new.mu) - difference) >= 1e-7 )
        {
            # Track Progess
            print(steps)
            steps <- steps + 1
            
            difference <- is.converged(theta.mu, new.mu)
            print(difference)
            
            # Perform E-Step
            e <- e.step( images.df[[cur.img]], theta.mu, theta.pi )
            
            # Perform M-Step
            m <- m.step( images.df[[cur.img]], e) 
            
            # Update Previous Step
            new.mu <- theta.mu
            new.pi <- theta.pi
            
            # Update Current Step
            theta.mu <- m$mu
            theta.pi <- m$pi
        }
        
        # Identify Mean Pixels for Original Image
        reconstructed <- find.small.distance( images.df[[cur.img]], theta.mu )
        new.image     <- images.df[[cur.img]]
        
        # For Every Row in Original Image
        for ( i in 1:nrow(new.image) )
        {
            # Replace Pixel with Mean Pixel
            new.image[i, ] <- new.mu[ reconstructed$closest[i], ]
        }
        
        # Reconstuct Image for Output
        output.image( ncol(raw.JPEG[[cur.img]]), 
                      nrow(raw.JPEG[[cur.img]]), 
                      new.image, 
                      paste('outputImages/', cur.img, segment, '.jpg', sep = ""), .5)
    }
}



# See if Variation Based on K-Means Start Location Using Test Image (Sunset)
# ====================================================================================================

# For Five Iterations
for ( i in 1:5 )
{
    # Get Random CLuster Location
    rm( .Random.seed, envir = globalenv() )
    random <- ceiling( runif( 1, 1, 2000 ) )
    set.seed( random )
    
    # Perform K-Means at Location
    location <- kmeans( x = images.df[[3]], centers = 20, algorithm = "Hartigan-Wong", iter.max = 100 )
    
    # Get Initial Values for Theta
    theta.mu <- location$centers
    theta.pi <- find.pi(location$cluster)
    
    # Get Updated Values for Theta
    new.mu <- matrix( 0, nrow = nrow(theta.mu), ncol = 3 )
    new.pi <- matrix( 0, nrow = nrow(theta.pi), ncol = 1 )
    
    difference <- .Machine$double.xmax
    steps <- 1
    
    # EM Algorithm Until Convergence
    while( abs(is.converged(theta.mu, new.mu) - difference) >= 1e-7 )
    {
        # Track Progess
        print(steps)
        steps <- steps + 1
        
        difference <- is.converged(theta.mu, new.mu)
        print(difference)
        
        # Perform E-Step
        e <- e.step( images.df[[3]], theta.mu, theta.pi )
        
        # Perform M-Step
        m <- m.step( images.df[[3]], e)
        
        # Update Previous Step
        new.mu <- theta.mu
        new.pi <- theta.pi
        
        # Update Current Step
        theta.mu <- m$mu
        theta.pi <- m$pi
    }
    
    # Identify Mean Pixels for Original Image
    reconstructed <- find.small.distance( images.df[[3]], theta.mu )
    new.image     <- images.df[[3]]
    
    # For Every Row in Original Image
    for ( j in 1:nrow(new.image) )
    {
        # Replace Pixel with Mean Pixel
        new.image[j, ] <- new.mu[ reconstructed$closest[j], ]
    }
    
    # Reconstuct Image for Output
    output.image( ncol(raw.JPEG[[3]]), nrow(raw.JPEG[[3]]), new.image, paste('outputImages/sunsetVariation', i, '.jpg', sep = ""), .5)
}
