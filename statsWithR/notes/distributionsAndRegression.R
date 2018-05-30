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

# SLR
# ----------------------------------------
# lm( y ~ x, data )
# $coefficients
# $fitted.values
# $residuals


# The above lm() function gives access to the below calculations


# Fitted
# ----------------------------------------
# y_hat = beta_hat_0 + ( beta_hat_1 * x )


# Residual
# ----------------------------------------
# e_i = y_i - y_hat_i
    # data[ which( data$column == number ), ] to find residual
    # aboveResult - ( beta_hat_0 + ( beta_hat_1 * x ) ) will yield residual 


# Predict Y
# ----------------------------------------
# predict( lmModel, newData )
    # New data needs to be formatted similar to the data used for lm()


# SLR Model
# ----------------------------------------
# Y_i = beta_0 + ( beta_1 * x_i ) + e_i
# e_i ~ N( mean = 0, sd^2 )


# Least Squares
# ----------------------------------------
# Sxy = sum( ( x - mean(x) ) * ( y - mean(y) ) )
# Sxx = sum( ( x - mean(x) ) ^ 2 )
# Syy = sum( ( y - mean(y) ) ^ 2 )

# beta_hat_1 = Sxy / Sxx                      ~ Slope
# beta_hat_0 = mean(y) - beta_hat_1 * mean(x) ~ Intercept


# Check if value is in vector (if interpolation is true)
# ----------------------------------------
# number %in% unique(data$column)
# min( data$column ) < number & number < max( data$column )


# Residual Standard Error (RSE)
# ----------------------------------------
# s_e^2 = sum( e^2 ) / ( n - 2 )
# s_e   = sqrt( s_e^2 )


# Variance Decomposition
# ----------------------------------------
# SST   = sum( ( y - mean(y) ) ^ 2 )
# SSReg = sum( ( y_hat - mean(y) ) ^ 2 )
# SSE   = sum( ( y - y_hat ) ^ 2 )

# SSReg + SSE = SST
# S_e^2 = SSE / ( n- 2 )


# R^2
# ----------------------------------------
# R^2 = SSReg / SST or
# R^2 = 1 - ( SSE / SST )




# ====================================================================================================
# Simulation
# ====================================================================================================

# Assume the Model
# ----------------------------------------
# Y|X ~ N( beta_0 + ( beta_1 * x ), sd^2 )
# Estimates for above params
    # beta_hat_0, 
    # beta_hat_1, 
    # s_e^2 = sd^2


# Example
# ----------------------------------------
# Y|X ~ N( mean = 5 - 2x, var = sd^2 )
    # Y = 5 - 2x + e
    # e ~ N( mean = 0, var = 9 )
    # Obtain beta_0 and beta_1

# Generate Data
    # x_vals = seq( from 0, to = 10, length.out = numObs )
    # y_vals = beta_0 + ( beta_1 * x_vals ) + epsilon

# Generate Errors
    # Set Seed
    # epsilon = rnorm( n = numObs, mean = 0, sd )

# Fit model
    # fit = lm( y_vals ~ x-vals )
    # fit$coefficient
    # plot( y_vals ~ x-vals )
    # abline( fit )


# Sim Function
# ----------------------------------------
# sim.slr <- function( x, beta_0, beta_1, sigma ) 
# {
    # n = length(x)
    # epsilon = rnorm( n, mean = 0, sd = sigma )
    # y = beta_0 + ( beta_1 * x ) + epsilon
    # data.frame( predictor = x, response = y )
# }

# sim.data = sim.slr( x = x_vals, beta_0, beta_1, sigma )
# sim.fit = lm( response ~ predictor, data = sim.data )

# plot( response ~ predictor, data ~ sim.data,
    # pch = 20,
    # cex = 2,
    # col )

# abline( sim.fit, lwd = 3, lty = 1, col )
# abline ( beta_0, beta_1, lwd = 3, lty = 2, col )


( -1.5 + ( 2.3 * 10 ) ) - ( -1.5 + ( 2.3 * 9 ) )
