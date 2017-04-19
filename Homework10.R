# Homework 10- For Loops
# April 5, 2017

## Number 1

v <- rep(0:9,each=5)
print(v)
w <- vector(mode="numeric")

for (i in seq_along(v)){
  if(v[i]==0) w[i] <- 1 else
   w[i] <- 0
}

print(sum(w))


## Number 2

length(v[v==0])

## Number 3

###################################################################################################
# FUNCTIOn: MaxDiff
# takes a numeric vector and returns as output the maximum difference between all possible pairs of elements
# input: numeric vector x
# output: maximum difference y
#-----------------------------------------------------------------------------------------------------

MaxDiff <- function(x=rep(-5:5,each=1)) {
   diff <- c(dist(x))
  return(max(diff))
}

MaxDiff()

### Number 4
MaxDiff1 <- function(x=rep(-5:5,each=1)){
  for (i in seq_along(x)){
    diff <- c(dist(x))
  }
  return(c(max(diff)))
}
MaxDiff1()

# Number 4 - Alex's Solution
X =rnorm(20,mean=0,sd=0)

max_diff = function(X){
  
  # method 1
  pairs = expand.grid(X,X)
  
  # method 2 with loops
  cols = rep(0,length(X)^2)
  
  x_pairs = data.frame(cols,cols)
counter = 0
for(i in 1:length(X)){
  
  for(j in 1:length(X)){
    counter=counter+1
    iter_pairs=c(X[i],X[j])
    x_pairs[counter,]=iter_pairs
  }
}
diff=abs(x_pairs[,1]-x_pairs[,2])
max_difference = max(diff,na.rm=T)
return(max_difference)

}

max_diff()




  