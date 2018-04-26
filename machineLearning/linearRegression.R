# Dataset: https://archive.ics.uci.edu/ml/datasets/Abalone

# Libraries
# ====================================================================================================
library(glmnet)



# Setup Plot Preferences
# ====================================================================================================

# 4 Figures ~ 2 Rows & 2 Columns
par( mfrow = c( 2, 2 ) ) 



# Load Dataset
# ====================================================================================================

setwd("/Users/Sbishop/Desktop/aml498/hw5/rCode")

# Weight is in grams, and measurements are in millimeters
abalone           <- as.data.frame( read.csv('../data/abalone.csv') )
colnames(abalone) <- c( 'sex',            'length',         'diameter',     'height', 'whole_weight', 
                        'shucked_weight', 'viscera_weight', 'shell_weight', 'rings' )



# 7.11 
# ====================================================================================================

# A ~ Build linear regression predicting age from measurements (ignore gender)
# --------------------------------------------------

# Ignore Gender
abalone.no.gender <- abalone[ , -1]

# Dependent Variable
dependent <- stack( abalone.no.gender, select = 8 )

# Explanatory Variables
explanatory <- stack( abalone.no.gender, select = 1:7 )

# Linear Model DF
abalone.dfA <- data.frame( dependent   = dependent  [ , c('values')],
                           explanatory = explanatory[ , c('values')])

# Regression Plot
plot( x = abalone.dfA$explanatory, y = abalone.dfA$dependent, 
      xlab = 'Explanatory', ylab = 'Age', main = 'Regressions for Abalone Age (No Gender)' )
abline( 8.771181, 3.037610 )

# Linear Model
abalone.lmA <- lm( dependent ~ explanatory, data = abalone.dfA )

# Plot Residuals
plot( x = abalone.lmA$fitted.values, y = abalone.lmA$residuals, 
      xlab = "Fitted", ylab = "Residuals", main = "Abalone Residuals Against Fitted (No Gender)" ) 
abline( 0, 0 )


# B ~ Build linear regression predicting age from measurements (including gender)
# --------------------------------------------------

# Factor Gender [ M = 1, F = -1, I = 0 ]
gender.factor  <- factor( abalone$sex, labels = c('-1', '0', '1') )
gender.factor  <- as.numeric( levels( gender.factor ) )[gender.factor]
abalone.factor <- data.frame( gender.factor, abalone[ , 2:9] )

# Dependent Variable
dependent <- stack( abalone.factor, select = 9 )

# Explanatory Variables
explanatory <- stack( abalone.factor, select = 1:8 )

# Linear Model DF
abalone.dfB <- data.frame( dependent   = dependent  [ , c('values')],
                           explanatory = explanatory[ , c('values')])

# Regression Plot
plot( x = abalone.dfB$explanatory, y = abalone.dfB$dependent, 
      xlab = 'Explanatory', ylab = 'Age', main = 'Regressions for Abalone Age (Gender)' )
abline( 9.466112, 1.369257 )

# Linear Model
abalone.lmB <- lm( dependent ~ explanatory, data = abalone.dfB )

# Plot Residuals
plot( x = abalone.lmB$fitted.values, y = abalone.lmB$residuals, 
      xlab = "Fitted", ylab = "Residuals", main = "Abalone Residuals Against Fitted (Gender)" ) 
abline( 0, 0 )


# C ~ Build linear regression predicting **log** of age from measurements (ignore gender)
# --------------------------------------------------

# Dataset with Log Age
abalone.no.gender.log <- data.frame( abalone.no.gender[1:7], log(abalone$rings) )

# Dependent Variable
dependent <- stack( abalone.no.gender.log, select = 8 )

# Explanatory Variables
explanatory <- stack( abalone.no.gender.log, select = 1:7 )

# Linear Model DF
abalone.dfC <- data.frame( dependent   = dependent  [ , c('values')],
                           explanatory = explanatory[ , c('values')])

# Regression Plot
plot( x = abalone.dfC$explanatory, y = abalone.dfC$dependent, 
      xlab = 'Explanatory', ylab = 'Age', main = 'Regressions for Abalone Age (No Gender/Log)' )
abline( 2.1164585, 0.3376514 )

# Linear Model
abalone.lmC <- lm( dependent ~ explanatory, data = abalone.dfC )

# Plot Residuals
plot( x = abalone.lmC$fitted.values, y = abalone.lmC$residuals, 
      xlab = "Fitted", ylab = "Residuals", main = "Abalone Residuals Against Fitted (No Gender/Log)" ) 
abline( 0, 0 )


# D ~ Build linear regression predicting **log** age from measurements (including gender)
# --------------------------------------------------

# Dataset with Log Age
abalone.gender.log <- data.frame( gender.factor, abalone[2:8], log(abalone$rings) )

# Dependent Variable
dependent <- stack( abalone.gender.log, select = 9 )

# Explanatory Variables
explanatory <- stack( abalone.gender.log, select = 1:8 )

# Linear Model DF
abalone.dfD <- data.frame( dependent   = dependent  [ , c('values')],
                           explanatory = explanatory[ , c('values')])

# Regression Plot
plot( x = abalone.dfD$explanatory, y = abalone.dfD$dependent, 
      xlab = 'Explanatory', ylab = 'Age', main = 'Regressions for Abalone Age (Gender/Log)' )
abline( 2.1932668, 0.1534851 )

# Linear Model
abalone.lmD <- lm( dependent ~ explanatory, data = abalone.dfD )

# Plot Residuals
plot( x = abalone.lmD$fitted.values, y = abalone.lmD$residuals, 
      xlab = "Fitted", ylab = "Residuals", main = "Abalone Residuals Against Fitted (Gender/Log)" ) 
abline( 0, 0 )


# E ~ Which regression would you use to determine age of abalone and why?
# --------------------------------------------------



# F ~ Can you improve the regression by using a regularizer?
# --------------------------------------------------

# Plot A ~ Build linear regression predicting age from measurements (ignore gender)
abalone.regularized.A <- cv.glmnet( x = as.matrix( abalone.no.gender[ , 1:7], nrow = 4177, ncol = 7),
                                    y = as.matrix( abalone.no.gender[ ,   8], nrow = 4177, ncol = 1) )
plot( abalone.regularized.A , main = 'Regularization Ignoring Gender\n' )

# Plot B ~ Build linear regression predicting age from measurements (including gender)
abalone.regularized.B <- cv.glmnet( x = as.matrix( abalone.factor[ , 1:8], nrow = 4177, ncol = 8),
                                    y = as.matrix( abalone.factor[ ,   9], nrow = 4177, ncol = 1) )
plot( abalone.regularized.B , main = 'Regularization With Gender\n' )

# Plot C ~ Build linear regression predicting **log** of age from measurements (ignore gender)
abalone.regularized.C <- cv.glmnet( x = as.matrix( abalone.no.gender.log[ , 1:7], nrow = 4177, ncol = 7),
                                    y = as.matrix( abalone.no.gender.log[ ,   8], nrow = 4177, ncol = 1) )
plot( abalone.regularized.C , main = 'Regularization Ignoring Gender (log)\n' )

# Plot D ~ Build linear regression predicting **log** age from measurements (including gender)
abalone.regularized.D <- cv.glmnet( x = as.matrix( abalone.gender.log[ , 1:8], nrow = 4177, ncol = 8 ), 
                                    y = as.matrix( abalone.gender.log[ ,   9], nrow = 4177, ncol = 1 ) )
plot( abalone.regularized.D , main = 'Regularization With Gender (log)\n' )