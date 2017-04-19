## Care and Feeding of for loops
# March 30, 2017
# MWS

# simple example

myDat <- round(runif(10),digits=2)
for (i in seq_along(myDat)) {
 cat("loop number = ",i,
     "vector element=",myDat[i], "\n")
}

# don't but unncessary things in loops
myDat <- vector(mode="numeric",length=10)
for (i in seq_along(myDat)) {
 myDat[i] <- round(runif(1),digits=2)
 cat("loop number = ",i,
     "vector element=",myDat[i], "\n")
}

# do not change object dimensions in a loop
# (c,rbind,cbind,list)

myDat <- runif(1)
for (i in 2:10) {
  temp <- round(runif(1),digits=2)
  myDat <- c(myDat,temp)
  cat("loop number = ",i,
      "vector element=",myDat[i], "\n")
}
  
# do not use a loop if you don't need to
myDat <- 1:10
for(i in seq_along(myDat)){
  myDat[i] <- myDat[i] + myDat[i]^2
  cat("loop number = ",i,
      "vector element=",myDat[i], "\n")
}

z <- 1:10
z <- z + z^2
z

# be aware of i versus z[i]
z <- c(10,2,4)

for (i in seq_along(z)) { 
  cat("i=",i,"z[i]=",z[i],"\n")
}

# use next in the loop to skip certain elements
z <- 1:20

# can we operate on the odd-number elements?

for (i in seq_along(z)) {
  if (i %% 2==0) next
  print(i)
}

# a faster better way to do this
z <- 1:20
zsub <- z[z %% 2!=0]
z %% 2!=0
zsub <- z[z %% 2!=0]
zsub

for ( i in seq_along(zsub)) {
  cat("i=",i,"zsub[i]=",zsub[i],"\n")
}

###################################################################################
# FUNCTION: RanWalk
# stochastic random walk
# input: times = number of time steps
#        n1 = initial population size
#       lambda = finite rate of increase
#       noiseSD = sd of normal distribution with mean=0
# output: vector n with population sizes > 0
#         until extinction NA
#--------------------------------------------------------------------------------------

library(tcltk)

RanWalk <- function(times=100,
                    n1=50,
                    lambda=1.001,
                    noiseSD=10) {
n <- rep(NA,times)
n[1] <- n1
noise <- rnorm(n=times,mean=0,sd=noiseSD)

for (i in 1:(times-1)) {
  n[i+1] <- lambda*n[i] + noise[i]
  if(n[i+1] <= 0){
    n[i+1] <- NA
    cat("Population extinction at time",
        i-1, "\n")
    tkbell()
    break} #end of conditional statement
  
} # end of for loop
return(n)
} # end of function
head(RanWalk())
plot(RanWalk(lambda=1,noiseSD=5),type="o")

# Using double for loops
# loop over rows
m <- matrix(round(runif(20),digits=3),nrow=5)
for (i in 1: nrow(m)) {
  m[i,] <- m[i,]+i
}

# loop over columns
m <- matrix(round(runif(20),digits=3),nrow=5)
print(m)

for (j in 1:ncol(m)) {
  m[,j] <- m[,j] + j
}
print(m)

# double loop over rows and columns
m <- matrix(round(runif(20),digits=3),nrow=5)
for (i in 1:nrow(m)){
  for (j in 1:ncol(m)){
    m[i,j] <- m[i,j]+i+j
  } # end of the column loop j
} # end of row loop i
print(m)

# write functions to sweep over (systematically put together different combinations of variables)model parameters
###################################################################################################
# FUNCTIOn: SpeciesAreaCurve
# power functions for S and A
# input: A is a vector of island area
#         c is the intercept constant
#         z is the slope constant
# output: S, a vector of predicted species richness
#-----------------------------------------------------------------------------------------------------
SpeciesAreaCurve <- function(A=1:5000,
                             c=0.5,
                             z=0) {
  
S <- c*(A^z)
  return(S)
}
head(SpeciesAreaCurve)


###################################################################################################
# FUNCTIOn: SpeciesAreaPlot
# plot the species area curve
# input: A = vector or island areas
#       c= single paramteter for slope
# output: smoothed graph with parameters shown
#-----------------------------------------------------------------------------------------------------
SpeciesAreaPlot  <- function(A=1:50000,
                             c=0.5,
                             z=0.25) {
plot(x=A,y=SpeciesAreaCurve(A,c,z),
     type="l",xlab="Island Area",
    ylab = "S",ylim=c(0,1000))
  mtext(paste("c=c",c,"z= ",z),cex=0.7)
  return()
  
}
SpeciesAreaPlot()

# now sweep and built a grid of plot!

# global variables
cPars <- c(100,150,175)
zPars <- c(0.10,0.16,0.26,0.3)
par(mfrow=c(3,4)) # changes the graphics pallet to produce a set of plots 3 rows and 4 columns

for (i in seq_along(cPars)) { 
  for (j in seq_along(zPars)) {
    SpeciesAreaPlot(c=cPars[i],z=zPars[j])
  }
}

# looping with for
cutPoint <- 0.0001
z <- NA
ranData <- runif(100)
for (i in seq_along(ranData)){
  z <- ranData[i]
  if (z < cutPoint) break
}
print(z)

# looping with while
z <- NA
cycleNumber <- 0
while (is.na(z) | z >= cutPoint) {
  z <- runif(1)
  cycleNumber <- cycleNumber + 1
}
print(z)
print(cycleNumber)

# looping with repeat
z <- NA
cycleNumber <- 0
repeat {
  z <- runif(1)
  cycleNumber <- cycleNumber + 1
  if (z <= cutPoint) break
}
print(z)
print(cycleNumber)

# expand.grid to create a dataframe with all parameter combinations

expand.grid(cPars,zPars)
expand.grid(c=cPars,z=zPars)
