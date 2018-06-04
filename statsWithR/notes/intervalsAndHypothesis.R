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

stop.dist.model <- lm( dist ~ speed, data = cars )

# Create Confidence Intervals for Regression Parameters
confint( stop.dist.model, level = .99 )
confint( stop.dist.model, level = .99 )[ 2, 2 ]

# Create Confidence Intervals for Specific Regression Parameters
confint( stop.dist.model, param = "(Intercept)", level = .99 )
confint( stop.dist.model, param = "speed", level = .99 )

# Store Estimate
beta.1.hat <- coef( stop.dist.model )[2]

# Store Standard Error
beta.1.hat.se <- summary( stop.dist.model )$coefficients[ 2, 2 ]

# Verify Standard Error
Sxx <- sum( ( cars$speed - mean( cars$speed ) ) ^ 2 )
s.e <- summary( stop.dist.model )$sigma
s.e / sqrt( Sxx )

# Calculate Crit Value for 2-Sided
( 1 - .99 ) / 2
crit <- qt( 0.995, df = nrow(cars) - 2 )

# Estimate - margin, estimate + margin
c( beta.1.hat - crit * beta.1.hat.se, beta.1.hat + crit * beta.1.hat.se )

# qt() and pt()
    # quantile and probability functions
pt( qt( 0.995, df = nrow(cars) - 2 ), df = nrow(cars) - 2 )

# alpha / 2
1 - pt( qt( 0.995, df = nrow(cars) - 2 ), df = nrow(cars) - 2 )

# New data with x = 5 and x = 21
new.speeds <- data.frame( speed = c( 5, 21 ) )

# Point Estimates
predict( stop.dist.model, newdata = new.speeds )

# CI for mean response
predict( stop.dist.model, newdata = new.speeds,
         interval = c( "confidence" ), level = .99 )

# PI for mean response
predict( stop.dist.model, newdata = new.speeds,
         interval = c( "prediction" ), level = .99 )

# Creating CI and PI bands
speed.grid <- seq( min( cars$speed ), max( cars$speed ), by = 0.01 )

dist.ci.band <- predict( stop.dist.model, newdata = data.frame( speed = speed.grid ),
                         interval = c( "confidence" ), level = .99 )

dist.pi.band <- predict( stop.dist.model, newdata = data.frame( speed = speed.grid ),
                         interval = c( "prediction" ), level = .99 )

plot( dist ~ speed, data = cars,
      pch = 20, cex = 2, col = "grey",
      ylim = c( min( dist.pi.band ), max( dist.pi.band ) ) )
abline( stop.dist.model, lwd = 5, col = "darkorange" )

lines( speed.grid, dist.ci.band[ , "lwr" ], col = "dodgerblue", lwd = 3, lty = 2 )
lines( speed.grid, dist.ci.band[ , "upr" ], col = "dodgerblue", lwd = 3, lty = 2 )
lines( speed.grid, dist.pi.band[ , "lwr" ], col = "dodgerblue", lwd = 3, lty = 2 )
lines( speed.grid, dist.pi.band[ , "upr" ], col = "dodgerblue", lwd = 3, lty = 2 )

points( mean( cars$speed ), mean( cars$dist ), pch = '+', cex = 3 )


# Hypothesis Testing in R
# ------------------------------------------------------------------------------------------

# Store test statistics
beta.hat.0.t <- summary( stop.dist.model )$coefficients["(Intercept)", "t value"]
beta.hat.1.t <- summary( stop.dist.model )$coefficients["speed", "t value"]

# Verify P-Value in 2 ways
2 * pt( -abs( beta.hat.1.t ), df = nrow(cars) - 2 )
2 * pt( abs( beta.hat.1.t ), df = nrow(cars) - 2, lower.tail = FALSE )

# SLR Function
sim.slr <- function( x, beta_0, beta_1, sigma )
{
n = length(x)
    epsilon = rnorm( n, mean = 0, sd = sigma )
    y = beta_0 + ( beta_1 * x ) + epsilon
    data.frame( predictor = x, response = y )
}

x = seq( 1, 20, length.out = 21 )
sim.data <- sim.slr( x = x, beta_0 = 2, beta_1 = 0, sigma = 1 )
sim.fit <- lm( response ~ predictor, data = sim.data )
summary( sim.fit )$coefficients[ "predictor", "Pr(>|t|)" ]

plot( response ~ predictor, data = sim.data, pch = 20, col = "grey", cex = 1.5 )
abline( sim.fit, col = "darkorange", lwd = 3 )
abline( 2, 0, lwd = 3, lty = 2, col = "dodgerblue" )



# Examples
# ------------------------------------------------------------------------------------------

# Consider a random variable X, t distribution with 7 df 
# Calculate P[X > 1.3]
pt(q = 1.3, df = 7, lower.tail = FALSE, log.p = FALSE)

# Consider a random variable Y, t distribution with 99 df
# Find cc such that P[X > c] = 0.025P
qt(p = 0.025, df = 9, lower.tail = FALSE, log.p = FALSE)

# What is the length of a 90% confidence interval for \beta_1β 1
slr.fit = lm( Girth ~ Height, data = trees ) 
beta.1 <- slr.fit$coefficients[2]

int <- confint( slr.fit, param = beta.1, level = .90 )  
abs(int[2,1] - int[2, 2])

# Calculate a 95% confidence interval for the mean tree girth of a tree that is 79 feet tall.
slr.fit = lm( Girth ~ Height, data = trees )

new.tree <- data.frame( Height = c( 79 ) )

predict( slr.fit, newdata = new.tree,
         interval = c( "confidence" ), level = .95 )

