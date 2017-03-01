#Data Structures 2
#February 16, 2017
#MWS

#Matrices
a <- 1:12 #sequence function
print(a)

#creating a matrix from an atomic vector

m <- matrix(data=1:12,nrow=4,ncol=3)
print(m)

m <- matrix(data=1:12,nrow=4,byrow=TRUE)
print(m)

# use dim()
dim(m)

dim(m) <- c(6,2)
print(m)
dim(m) <- c(4,3)
print(m)

# basic dimensions of the matrix
nrow(m)
ncol(m)
length(m)
print(m)

# adding names to rows and columns
names(m)

# add names with an assignment
rownames(m) <- c("a","b","c","d")
print(m)
colnames(m) <- LETTERS[1:ncol(m)]
print(m)
rownames(m) <- letters[nrow(m):1]
print(m)







#simple subsetting
#print a single element
print(m[2,3])
print(m["c","C"])

#print an entire row or column with empty subscript

#print row 3
print(m[3,])

#print first column
print(m[,1])
print(m[,])
print(m[])



# using paste function for automating names
rownames(m) <- paste("Site",1:nrow(m),sep="")
colnames(m) <- paste("Species",LETTERS[1:ncol(m)],sep="")
print(m)

# transpose matrix very easily with t
m2 <- t(m)
print(m2)

# add a row to m with rbind()
m2 <- rbind(m2,c(10,20,30,40))
print(m2)
rownames(m2)
rownames(m2)[4] <- "myFix"
print(m2)

# add a column with cbind()
m2 <- cbind(c(0.2,0.4,0.6,0.8),m2)
print(m2)

# access individual or compound elements
m2["SpeciesA","Site2"] #equals m2[1,3]
m2[c("SpeciesB","SpeciesC"),c("Site1","Site4")]

# transform matrix back into vector
myVec <- as.vector(m)
print(myVec)

#Lists are atomic vectors but each element can be of any type we want and can hold other structures, including other lists

myList <- list(1:10,matrix(1:8,nrow=4,byrow=TRUE),letters[1:3],pi)

print(myList)
str(myList)
myList[4]
# myList[4] - 3
myList[[4]] - 3
#suppose a list with 10 elements in it
#[[5]] equals contents of car #5
# [c(4,5,6)] equals a littel train of cars 4,5, and 6

myList[[2]]
myList[[2]][3,2]


# pulling out results from stats in R with a list
varY <- runif(10)
varX <- runif(10)
myModel <- lm(varY~varX)
print(myModel)
summary(myModel)
plot(x=varX, y=varY)
plot(myModel)

# figure out structure of myModel
str(summary(myModel))
names(summary(myModel))
summary(myModel)$coefficients
summary(myModel)$coefficients[2,4]

## Data Frames
# a data frame is a list of equal-length atomic vectors
varA <- 1:12
varB <- rep(c("Con","LowN","HighN"), each=4)
varC <- runif(12)
dFrame <- data.frame(varA,varB,varC,stringsAsFactors = FALSE)
print(dFrame)
str(dFrame)

# add a row to data frame with the function rbind
# but first must create it as a data frame

newData <- data.frame(list(varA=13,varB="HighN",varC=0.668), stringsAsFactors=FALSE)
str(newData)
dFrame <- rbind(dFrame,newData)
str(dFrame)
tail(dFrame)

# adding a column is more simple
newVar <- data.frame(varD=runif(13))
dFrame <- cbind(dFrame,newVar)
head(dFrame)
