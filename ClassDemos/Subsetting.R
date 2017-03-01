# Subsetting in R
# 21 February 2017
# MWS

# Techniques for subsetting atomic vectors

z <- c(3.1, 9.2, 1.3, 0.4, 7.5)

# positive index values
z[c(2,3)]

# negative index values
z[-c(2,3)]

# create a vector of logical values that are the same length as the target vector

z[z<3]

tester <- z<3
print(tester)
z[tester]

# which function for index values
which(z<3)

# this is overkill
z[which(z<3)]

z[-(length(z):(length(z)-2))]

# vector elements can have names
names(z) <- letters[1:5]
z[c("b","c")]


#Subsetting matrices & data frames

m <- matrix(1:12,nrow=3)
dimnames(m) <- list(paste("Species",LETTERS[1:nrow(m)],sep=""),paste("Site",1:ncol(m),sep=""))
print(m)

#Subsetting based on elements
m[1:2,3:4]

# or subset on character strings
m[c("Species A","SpeciesB"), c("Site3","Site4")]

m[1:2, ]

m[ ,3:4]

# use logicals for more complex subsetting
# e.g. select columsn for which the totals are > 15

# try this logical

colSums(m) > 15

m[ ,colSums(m) > 15]

# select all rows for which the row total is 22
m[rowSums(m)== 22, ]

# select all rows for which the row total is NOT 22

m[rowSums(m)!=22, ]


# Choose all rows for which the numbers for site 1 are less than 3
# AND all the columns for which the numbers for Species A are less than 5

# logical for the rows
m[ ,"Site1"] < 3

# show this for all the columns
m[m[ ,"Site1"]< 3, ]


#logical for columns
m["SpeciesA", ] < 5

# shows this for all the rows
m[ ,m["SpeciesA", ] < 5]

# now combine the row and column selections
m[m[ ,"Site1"]<3, m["SpeciesA", ]<5]
m

# caution, dropping down to 1 row or column creates a vector

z <- m[1, ]
print(z)

# use drop=false option to prevent this
z2 <- m[1, ,drop=FALSE]
print(z2)
str(z2)


# only a few more things for data frames
data <- read.csv(file="antcountydata.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
str(data)


#subsetting as usual on elements
data[3,2]

# specify just the column names, which are list elements
dataNames <- data[ ,c("state","county")]
data2Names <- data[c("state","county")]
str(dataNames)
str(data2Names)

# be careful to include both dinesions in the data matrix

m[2, ]

m[ ,2]

m[2]

# specify both dimensions
m[2,1]
