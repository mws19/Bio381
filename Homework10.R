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
  mat <- matrix(nrow=length(x),ncol=length(x))
  rownames(mat) <- x
  colnames(mat)<- x
    for (i in seq_along(x)){
    for (j in seq_along(x)){
     mat[i,j]<- x[i]-x[j]
    }
  }
  z <- c(mat)
  x <- which(mat==max(abs(mat)), arr.ind=TRUE)
  w<-colnames(mat)[x[1,1]]
  t<-rownames(mat)[x[1,2]]
  return(list(c(w,t),x,max(abs(z))))
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

# Number 6 - Matrix multiplication
mat1 <- matrix(data=1:100,10,10,byrow=T)
mat2 <- matrix(data=101:200,10,10,byrow=T)

m1 = matrix(c(1:6),2,3, byrow = T)
m2 = matrix(c(7:12),3,2, byrow = T)

MultMat = function(m1=matrix(data=1:25,nrow=5,ncol=5,byrow=T),m2=t(m1)){
  m = matrix(0, nrow(m1), ncol(m2))
  if (dim(m1) != dim(m2)){
    cat("input matrices are of different dimensions", "\n")
  } else {
    for (i in 1:nrow(m1)){
      for (j in 1:ncol(m2)){
        cell_val = sum(m1[i,]*m2[,j])
        m[i,j] = cell_val
      }
    }
  }
  return(m)
}

c1 <- matrix(data=1:4,2,2,byrow=T)
MultMat()


M1 = matrix(c(1:6),2,3, byrow = T)
M2 = matrix(c(7:12),3,2, byrow = T)

MatMult = function(m1=matrix(data=1:10,nrow=2,ncol=5),m2=t(m1)){
   m = matrix(0, nrow(m1), ncol(m2))
  if (nrow(m1) != ncol(m2)){
    cat("input matrices are not correct dimensions", "\n")
  } else {
    for (nr in 1:nrow(m1)){
      for (nc in 1:ncol(m2)){
        cell_val = sum(m1[nr,]*m2[,nc])
        m[nr,nc] = cell_val
      }
    }
  }
  return(m)
}

MatMult()
m1%*%m2
