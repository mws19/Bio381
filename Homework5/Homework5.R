# Morgan Southgate
# Homework 5
# February 15, 2017

#1
x <- 1.1
a <- 2.2
b <- 3.3

##a
z <- (x^(a^b))
print(z)

##b
z <- ((x^a)^b)
print(z)

##c
z <-((3*x^3)+(2*x^2)+1)
print(z)

##d
(z %% 1)
floor((z %% 1)*10)

#2
##a
x <- seq(from = 1, to=8)
y <- seq(from = 7, to = 1)
c(x,y)

##b
rep(1:5, c(1,2,3,4,5))

##c
rep(5:1, c(1,2,3,4,5))

#3
v <- runif(2)
r <- sqrt(v[1]^2 + v[2]^2)
theta <- atan(v[2]/v[1])
print(r)
print(theta)

#4
queue <- c("sheep", "fox", "owl", "ant")

##a
queue <- c(queue, "serpent")

##b
queue <- queue[-1]

##c
queue <- c("donkey",queue)

##d
queue <- queue[-5]

##e
queue <- queue[-3]

##f
queue <- append(queue, "aphid", after = 2)

##g
which(queue == "aphid")

#5
v <- rep(1:100, by=1)
v[v%%2 != 0 & v%%3 !=0 & v%%7 !=0]

#6
z <- runif(1000)

##a
y<-c(mean(z<0.1),mean(z>0.9),mean(z>.45 & z<.55))
print(y)

##b
x1 <- log10(z)
x2 <- z^2
x3 <- exp(z)
x4 <- sqrt(z)

##c
w1 <- c(mean(x1<0.1),mean(x1>0.9),mean(x1>.45 & x<.55))
print(w1)

w2 <- c(mean(x2<0.1),mean(x2>0.9),mean(x2>.45 & x<.55))
print(w2)

w3 <- c(mean(x3<0.1),mean(x3>0.9),mean(x3>.45 & x3< .55))
print(w3)

w4 <- c(mean(x3<0.1),mean(x3>0.9),mean(x3>.45 & x3< .55))
print(w4)
