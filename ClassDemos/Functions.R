# Working with functions
# 2 March 2016
# MWS

# everything in R is a function
sum(3,2) # "prefix" function
3+2 # "operator" is a function
`+`(3,2) # "infix" function
y <- 3
`<-`(yy,3) # infix
print(yy)

# to see the contents of a function, print it
print(read.table)
sum(3,2)
print(sum)  # Primitive function in C
sum()


#######################################################################################
# FUNCTION: HardyWeinberg
# input: p, an allelic frequency (0,1)
# output: p and three genotype frequencies AA, AB, BB
#--------------------------------------------------------------------------------------
HardyWeinberg <- function(p=runif(1)) {
  q <- 1-p
  fAA <- p^2
  fAB <- 2*p*q
  fBB <- q^2
  
  vecOut <- signif(c(p=p,AA=fAA,AB=fAB,BB=fBB),digits=3)
  return(vecOut)
}
########################################################################################
HardyWeinberg()
pp <- 0.7
HardyWeinberg(p=pp)
HardyWeinberg(1.2)

# Use multiple return() statements to generate different possible outcomes

#######################################################################################
# FUNCTION: HardyWeinberg2
# input: p, an allelic frequency (0,1)
# output: p and three genotype frequencies AA, AB, BB
#--------------------------------------------------------------------------------------
HardyWeinberg2 <- function(p=runif(1)) {
  if(p>1.0 | p<0.0) {
    return("Function failure: p must be >= 0 and <=1")
  }
  q <- 1-p
  fAA <- p^2
  fAB <- 2*p*q
  fBB <- q^2
  
  vecOut <- signif(c(p=p,AA=fAA,AB=fAB,BB=fBB),digits=3)
  return(vecOut)
}
########################################################################################
HardyWeinberg2()
HardyWeinberg2(0.5)
HardyWeinberg2(1.2)
z <- HardyWeinberg2(1.2)

#######################################################################################
# FUNCTION: HardyWeinberg3
# input: p, an allelic frequency (0,1)
# output: p and three genotype frequencies AA, AB, BB
#--------------------------------------------------------------------------------------
# Use stop function for true error trapping
HardyWeinberg3 <- function(p=runif(1)) {
  if(p>1.0 | p<0.0) {
    stop("Function failure: p must be >= 0 and <=1")
  }
  q <- 1-p
  fAA <- p^2
  fAB <- 2*p*q
  fBB <- q^2
  
  vecOut <- signif(c(p=p,AA=fAA,AB=fAB,BB=fBB),digits=3)
  return(vecOut)
}
########################################################################################
HardyWeinberg3(1.2)

# Scoping in functions
# Global variables: not in functions, visible to all parts of code, declared in main body
# Local variables are visible only within function - created in function of passed through as parameter

myFunc <- function(a=3,b=4) {
  z <- a + b
  return(z)
}

myFunc()
print(a)

myFuncBad <- function(a=3) {
  z <- a+b
  return(z)
}
myFuncBad()
b <- 10
myFuncBad()

myFuncOK <- function(a=3) {
  bb <- 100
  z <- a + bb
  return(z)
}
myFuncOK()
print(bb)

# Simple linear regression function

########################################################################################
# FUNCTIOn: fitLinear
# fits a simple OLs regression
# inputs: numeric vector of predictor (x) and response (y)
# outputs: slope and p-value
#---------------------------------------------------------------------------------------

fitLinear <- function(x=runif(10),y=runif(10)){
  myMod <- lm(y~x)
  myOut <- c(slope=summary(myMod)$coefficients[2,1]
             pValue=summary(myMod)$coefficients[2,4])
  plot(x=x,y=y)
  return(myOut)
}

########################################################################################
# FUNCTIOn: fitLinear2
# fits a simple OLs regression
# inputs: numeric vector of predictor (x) and response (y)
# outputs: slope and p-value
#---------------------------------------------------------------------------------------

fitLinear2 <- function(x=NULL,y=NULL){
  if(is.null(x) & is.null(y)) {
    x <- runif(20)
    y <- 0.5 +2*x + rnorm(n=20, mean=0, sd=0.2)
  }
  myMod <- lm(y~x)
  myOut <- c(slope=summary(myMod)$coefficients[2,1],
             pValue=summary(myMod)$coefficients[2,4])
  plot(x=x,y=y)
  return(myOut)
}
fitLinear2()

# passing a parameter list with do.call
z <- c(runif(99),NA)
mean(z)
mean(x=z,na.rm=TRUE)
mean(x=z,na.rm=TRUE,trim=0.05)
parList <- list(x=z,na.rm=TRUE,trim=0.05)
do.call(mean,parList)
