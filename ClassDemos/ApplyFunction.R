# Various apply functions
# April 20, 2017
# MWS

# The apply function
m <- matrix(1:12,3,4,byrow=TRUE)
print(m)

myCV <- function(x=runif(5)) {
  x <- sd(x)/mean(x)
  return(x)
}

# get cvs of rows with a for loop
myOut <- vector(mode="numeric",length=nrow(m))

for (i in 1:nrow(m)) {
  myOut[i] <- myCV(m[i,])
}
print(myOut)

# solve with apply function
mO <- apply(m,1,myCV)
print(mO)

# solve for columns
m0 <- apply(m,2,myCV)
print(m0)

# apply across both indices simultaneously
m0 <- apply(m,c(1,2),myCV)
print(m0)

# use myCV for entire matrix
m0 <- myCV(m)
print(m0)

# use anonymous function
m0 <- apply(m,1, function(x) sd(x)/mean(x))
print(m0)

# problems with matrix dimensions
# a simple rescaling of the column totals
m0 <- apply(m,2,function(x) x/sum(x))
print(m0)

# now try over rows
m0 <- t(apply(m,1,function(x) x/sum(x)))
print(m0)

# rescale over entire matrix
m0 <- t(apply(m,c(1,2),function(x) x/sum(x)))
print(m0)

# vectorize this operation
m0 <- m/sum(m)
print(m0)

# reshuffle matrix rows as in EcoSimR sim2
m0 <- t(apply(m,1, function(x) sample(x)))
print(m0)

# reshuffle columns
m0 <- (apply(m,2, function(x) sample(x)))
print(m0)

# reshuffle all elements
m0 <- sample(m)
print(m0)

m0 <- matrix(sample(m),3,4,byrow=T)
print(m0)

m0 <- m
print(m0)
m0[,] <- sample(m)
print(m0)

# what if the output differs in length each time
print(m)
m0 <- apply(m,c(1,2), function(x) runif(x))
print(m0)
str(m0)
print(m0[1,2])
print(m0[[2]])

# replicate function
myOut <- matrix(data=0,3,5)

# fill with a double for loop
for (i in 1:nrow(myOut)) {
  for (j in 1:ncol(myOut)) {
    myOut[i,j] <- runif(1)
  }
}
print(myOut)

myOut <- matrix(data=runif(15),3,5)
print(myOut)

# with replicate
m0 <- replicate(5, 100 + runif(3), simplify=FALSE)
print(m0)

# create different structure
m0 <- replicate(5, 100+ runif(sample.int(10,1)),simplify=TRUE)
print(m0)

# The lapply function
# use with operations on the columns of a data frame
d <- read.table(file="antcountydata.csv",
                header=T,
                sep=",",
                stringsAsFactors = FALSE)
str(d)

# with a for loop, get mean of vars 5 and 6
myMeans <- vector(mode="numeric",length=2)

z <- 0 # have to create counter variable; uggh!

for (i in 5:6){
  z <- z + 1
  myMeans[z] <- mean(d[[i]])
}

print(myMeans)

# do this much easier with lapply
myMeans <- lapply(d[c(5,6)],mean)
print(myMeans)
unlist(myMeans)

# sapply is the same but gives output as vector
myMeans <- sapply(d[c(5,6)],mean)
print(myMeans)

# aggregate function to split a list into groups
# and apply this function to each group
myMeans <- aggregate(d[c(5,6)],by=list(d$ecoregion),mean)
print(myMeans)

# set up parameter sweep for species-area model
# first illustrate with for loop

# global variables
c <- c(0.1, 0.2, 0.5)
z <- c(0.16, 0.26)
A <- c(1,10,100,1000)
noise <- c(0,0.01,0.1)
nrep <- 100 # must be of length 1 to use the Map function

# set up parameter/output grid
modelSum <- expand.grid(nrep=nrep,c=c,z=z,A=A,noise=noise)
modelSum$meanS <- NA
modelSum$sdS <- NA
print(modelSum)
# create simple Species Area calculator function
SAcalc <- function(c=0.1,z=0.16,A=100,noise=0.1) {
  S <- c*(A)^z + rnorm(n=1,mean=0,sd=noise)
  return(S)
}
SAcalc()
# cycle through parameters with a for loop
for (i in 1:nrow(modelSum)) {
  pars <- list(modelSum[i,2],
               modelSum[i,3],
               modelSum[i,4],
               modelSum[i,5])
  temp <-replicate(n=modelSum[i,1],do.call(SAcalc,pars))
  modelSum$meanS[i] <- mean(temp)
  modelSum$sdS[i]<- sd(temp)
}  
print(modelSum) 

# illustrate the basic map function
Map(SAcalc,modelSum$c,modelSum$z,modelSum$A,modelSum$noise)

# now repeat model calculations using only map, no for loops
temp <- replicate(nrep,unlist(Map(SAcalc,modelSum$c,modelSum$z,modelSum$A,modelSum$noise)))
modelSum$meanS[i] <- mean(temp)
modelSum$sdS[i]<- sd(temp)
print(modelSum) 
