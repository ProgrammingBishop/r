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