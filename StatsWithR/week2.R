# ====================================================================================================
# Normal Distribution
# ====================================================================================================

# Can also vectorize with vector ov values to get a vector of results

# P[ X < value ] ~ Probability 
# ----------------------------------------
# pnorm( x, mean, sd ) 
    # x is value
    # variance in sd^2
    # defaults to standardized normal distribution


# Height of Curve ~ Density
# ----------------------------------------
# dnorm( x, mean, sd )
    # x is value


# Interval ~ Probability
# ----------------------------------------
# diff( pnorm( c(x1, x2), mean, sd ) )


# Above ~ Probability
# ----------------------------------------
# pnorm( x, mean, sd, lower.tail = FALSE ) 


# P[ X < value ] ~ Value Below
# ----------------------------------------
# qnorm( p, mean, sd )


# P[ X < value ] ~ Value Above
# ----------------------------------------
# qnorm( p, mean, sd, lower.tail = FALSE )


# Outside Interval
# ----------------------------------------
# 1 - diff( pnorm( c(x1, x2), mean, sd ) )


# Generate Random Distribution
# ----------------------------------------
# rnorm( numObs, mean. sd )


    


# ====================================================================================================
# Simple Linear Regression
# ====================================================================================================


# Predict Y
# ----------------------------------------
# predict( lmModel, newData )
    # New data needs to be formatted similar to the data used for lm()


# SLR
# ----------------------------------------
# lm( y ~ x, data )
# $coefficients
# $fitted.values
# $residuals


# The above lm() function is equivelant to the below calculations


# Least Squares
# ----------------------------------------
# Sxy = sum( ( x - mean(x) ) * ( y - mean(y) ) )
# Sxx = sum( ( x - mean(x) ) ^ 2 )
# Syy = sum( ( y - mean(y) ) ^ 2 )

# beta_hat_1 = Sxy / Sxx
# beta_hat_0 = mean(y) - beta_hat_1 * mean(x)



