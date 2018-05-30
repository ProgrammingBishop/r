# Simulating Sampling Distributions
# ------------------------------------------------------------------------------------------

set.seed(420)

sample.size <- 100
x           <- seq( -1, 1, length = sample.size )
Sxx         <- sum( ( x - mean(x) ) ^ 2 )

beta_0 <- 3
beta_1 <- 6
sigma <- 2

var_beta_1_hat <- (sigma ^ 2) / Sxx
var_beta_0_hat <- (sigma ^ 2) * ( (1 / sample.size) + (mean(x) ^2 / Sxx) )

num.samples <- 10000
beta_0_hats <- rep( 0, num.samples )
beta_1_hats <- rep( 0, num.samples ) 

for ( i in 1:num.samples ) 
{
    espilon        <- rnorm( sample.size, mean = 0, sd = sigma )
    y              <- beta_0 + ( beta_1 * x ) + espilon
    model          <- lm(y ~ x)
    beta_0_hats[i] <- coef(model)[1]
    beta_1_hats[i] <- coef(model)[2] 
}

hist( beta_1_hats, prob = TRUE, breaks = 20,
      xlab = expression( hat(beta)[1] ) )

curve( dnorm( x, mean = beta_1, sd = sqrt( var_beta_1_hat ) ),
       col = 'blue', add = TRUE, lwd = 3 )


plot( cumsum( beta_1_hats ) / ( 1:length(beta_1_hats) ), type = 'l',
      ylim = c( 5.95, 6.05 ) )

abline( h = 6, col = 'darkorange', lwd = 2 )


# Interval Estimates in R
# ------------------------------------------------------------------------------------------


# Hypothesis Testing in R
# ------------------------------------------------------------------------------------------
