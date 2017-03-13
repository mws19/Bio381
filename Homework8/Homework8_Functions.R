### Functions for Statistical Analysis - Homework 8
## March 8, 2017
## MWS

#################################################################################
# Function: Linear Regression
# Description: 
# input: continuous x & y variables
# output: abline & scatterplot
# ------------------------------------------------------------------------------

linReg <- function(x=runif(100,min=0,max=100),y=runif(100,min=0,max=50)){
  linRegMod <- lm(y~x)
  linRegOut <- c(slope=summary(linRegMod)$coefficients[2,1],pValue=summary(linRegMod)$coefficients[2,4])
  return(linRegOut)}

linReg()

#################################################################################
# Function: Logistic Regression
# input: 
# output: 
# ------------------------------------------------------------------------------

#################################################################################
# Function: ANOVA
# input: 
# output: 
# ------------------------------------------------------------------------------

#################################################################################
# Function: Contingency Table
# input: 
# output: 
# ------------------------------------------------------------------------------
