# MLR
# ------------------------------------------------------------------------------------------

# Build lm model
model <- lm( response ~ predictor1 + predictor2, data )

# Start parameters
n <- numOfData
p <- length( coef( model ) )
X <- cbind( rep( 1, n ), data$p1 + data$p2 )
y <- data$response

# Get parameters
beta.hat <- solve( t(X) %*% X ) %*% t(X) %*% y
y.hat    <- X %*% beta.hat
e        <- y - y.hat

# Get residual standard error
se.sqr.y <- sqrt( ( y - y.hat ) ^ 2 ) / ( n - p )
se.sqr.e <- sqrt( ( t(e) %*% e ) / ( n - p ) )
se.sqr.y == se.sqr.e

# Doing the following is a way to not have to hard code (copy/paste) 
    # output into the written report
# $$
# R^2 = `r summary(model)$r.squared` 
# $$



# Simulation
# ------------------------------------------------------------------------------------------

# Set up true parameter values
n      <- 100
p      <- 3
beta.0 <- 5
beta.1 <- -2
beta.2 <- 6
sigma  <- 4
 
# Create X matrix
x0 <- rep( 1, n )
x1 <- sample( seq( 1, 10, length = n ) )
x2 <- sample( seq( 1, 10, length = n ) )
X  <- cbind( x0, x1, x2 )
C  <- solve( t(X) %*% X ) # solve() gets the matrix inverse

# Perform simulation
y          <- rep( 0, n )
num.sims   <- 10000

sim.beta.hat.2 <- function() 
{
    epsilon       <- rnorm( n, mean = 0, sd = sigma )
    y             <- ( beta.0 * x0 ) + ( beta.1 * x1 ) + ( beta.2 * x2 ) + epsilon
    fit           <- lm( y ~ x1 + x2 )
    coef( fit )[3]
}

# Repeatedly run code
beta.2.hat.alt <- replicate( n = num.sims, sim.beta.hat.2() )

hist( beta.2.hat.alt, prob = TRUE, breaks = 20,
      xlab = expression( hat(beta)[2] ), main = "", border = "dodgerblue" )
curve( dnorm( x, mean = beta.2, sd = sqrt( sigma ^ 2 * C[ 2 + 1, 2 + 1 ] ) ),
       col = "darkorange", add = TRUE, lwd = 3 )


# Intervals for MLR
# ------------------------------------------------------------------------------------------

# Interval for the params
confint( model, level )

# Make predictions
new.model <- data.frame( predictor1 = c( num, num ), predictor2 = c( num, num ) )
predict( model, new.data, interval = "confidence", level )
predict( model, new.data, interval = "prediction", level )

# When making predicitons, be aware of hidden extrapolation

# Creating Confidence intervals
est   <- summary(model)$coef["", "Estimate"]
se    <- summary(model)$coef["", "Std. Error"]
n     <- nrow(data)
p     <- length( coef( model ) ) # Number of Beta Params
df    <- n - p
alpha <- ( 1 - level ) / 2
crit  <- abs( qt( alpha, df ) )

conf.int <- c( est - crit * se, est + crit * se )




