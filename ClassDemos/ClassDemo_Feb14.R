# Basic data structures in R
# 14 February 2017
# MWS

# Use the assignment operator
x <- 5 #preferred
y = 4 #legal but confusing
y = y + 1.1 #take current value of y, add 1.1, and store as y
y <- y+ 1.1
print(y)

#Variable names

z <- 3 #
plant_height <- 3.3 #looks gangly
plant.height <- 4 # sometimes used in functions
plantHeight <- 3 # camelCaseFormatting


# the combine function
z <- c(3.2, 5, 5, 6)
print(z)
typeof(z)

z <- c(c(3,4),c(5,6))
print(z)      


#character strings surrounded by single or double quotes

z <- c("perch", "bass", 'trout')
print(z)
z <- c("This is only 'one' character string", 
       "this is a second")
print(z)
typeof(z)
is.numeric(z)

# building logicals
# no quotes, all caps
z <- c(TRUE, TRUE, FALSE)


# Three properties of vectors
# type
is.logical(z)
typeof(z)
str(z)

# Length
length(z)

# Names
z <- runif(5)
print(z)

# names are optional
names(z)
names(z) <- c("chow","pug","beagle","greyhound","akita")
print(z)

#add names with variable creation (with or without quotes)

z2 <- c(gold = 3.3, silver = 10, lead = 2)
print(z2)

# reset names
names(z2) <- NULL
print(z2)


### Three features of atomic variables
# Coercion

z <- c(3,"eggs")
typeof(z)
print(z)

#hierarchy of coercion

# logical -> integer -> double -> character 

# Comparison operators yield a logic variable outcome

a <- runif(10)
a > 0.5
print(a)


# how many elements are greater than 0.5?
sum(a > 0.5)

#what proportion of the elements are greater than 0.5?
 mean (a > 0.5)
 
 # Qualifying exam question: approximately what proportion of observations drawn from a normal distribution with mean 0, variance 1, are larger than 2.0?
 mean(rnorm(1000) >2)
 
 ###Vectorization
 z <- c(10,20,30)
 z +1 
 y <- c(1,2,3)
 z + y
 y <- c(1,2)
 
 z^2 + z
 
 ### Recycling
 y <- c(1,2)
 z <- c(10,20,30)
 y+z
 z <- c(10,20,30,40)
 y+z
 
 # creating vectors
 z <- vector(mode="numeric",length=0)
 print(z)
 
 #add element to z
 z <- c(z,5)
 print(z)

 # create vector predetermined length
 z <- rep(0,100)
 str(z)
 
 z <- rep(NA,100)
 typeof(z)
 
 z[1] <- "Washington"
 typeof(z)
 