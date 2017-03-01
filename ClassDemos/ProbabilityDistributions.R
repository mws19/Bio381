# Random Subsetting and Probability Distributions
# February 23, 2017
# MWS


# grab the ant data
data <- read.table(file="antcountydata.csv", header=TRUE,sep=",",stringsAsFactors=FALSE)
littleData <- data[1:8, 3:6]

# running sample with an integer
sample(10)

#applied to a vector, it randomizes the order of the elements
print(littleData$n.species)
sample(littleData$n.species)

sample(x=littleData$n.species, size=3)

# sample with replacement for bootstrapping

sample(x=littleData$n.species,size=20,replace=TRUE)

#sample sizes must match
# sample(x=littleData$n.species,size=20,replace=FALSE)

# suppose elements are sampled equiprobably?
# community assembly and null models
mainlandSpecies <- paste("Species",1:10,sep="")
popSizes <- c(1000,500,100,20,10,5,5,5,1,1)

#equiprobable colonization
islandA <- sample(x=mainlandSpecies,size=5)
print(islandA)



# assume colonization potential is proportional to population size
islandB <- sample(x=mainlandSpecies,size=5,prob=popSizes)
print(islandB)

# colonization now of individuals, not species
islandC <- sample(x=mainlandSpecies,size=100,prob=popSizes,replace=TRUE)
head(islandC)
table(islandC)
unique(islandC)
length(unique(islandC))


# colonization of equiprobable individuals
islandD <- sample(x=mainlandSpecies,size=100,prob=NULL,replace=TRUE)
head(islandD)
table(islandD)
unique(islandD)
length(unique(islandD))
length(table(islandD)[table(islandD)>9])




# ----------------------------
# Poisson distribution 
# "d" function gives the probability density
MyVec <- dpois(x=seq(0,10), lambda=1)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

MyVec <- dpois(x=seq(0,10), lambda=2)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)
print(MyVec)
sum(MyVec)

MyVec <- dpois(x=seq(0,10), lambda=5)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

MyVec <- dpois(x=seq(0,10), lambda=0.1)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

# p poisson
MyVec <- ppois(q=seq(0,10), lambda=2)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

qpois(p=c(0.025,0.975),lambda=50)
#output here means that 95% of the occurrences drawn will be between 37 and 64

x <- rpois(n=1000,lambda=1.1)
hist(x)

# Binomial
# p = probability of dichotomous outcome
# size = number of trials
# x = possible outcomes

MyVec <- dbinom(x=seq(0,10),size=10,p=0.5)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

MyVec <- dbinom(x=seq(0,10),size=10,p=0.75)
names(MyVec) <- seq(0,10)
barplot(height=MyVec)

qbinom(p=c(0.025,0.975),size=100,prob=0.75)

# negative binomial: number of failures (values of MyVec) in a series of Bernouli trials with probability of success p, before we reach a target number of successes
# more heterogeneous ("overdispersed") than a Poisson

MyVec <- dnbinom(x=seq(0,40),size=5, prob=0.5)
names(MyVec) <- seq(0,40)
barplot(height=MyVec)

MyVec <- dnbinom(x=seq(0,40),size=1, prob=0.1)
names(MyVec) <- seq(0,40)
barplot(height=MyVec)

# alternatively specify mean = mu
# size is a dispersion parameter
# small values of size give overdispersion

MyVec <- dnbinom(x=seq(0,40),size=1, mu=5)
names(MyVec) <- seq(0,40)
barplot(height=MyVec)

MyVec <- dnbinom(x=seq(0,40),size=10, mu=5)
names(MyVec) <- seq(0,40)
barplot(height=MyVec)

# pnbinom for tail distribution
pnbinom(q=3, size=1, mu=5)

# qnorm for actual values for a p 
qnbinom(p=0.05,size=10,mu=5)

# confidence interval
qnbinom(p=c(0.025,0.975), prob=0.5,size=10)

# random sample from negative binomial
MyVec <- rnbinom(n=100,size=1,mu=20)
quantile(MyVec,prob=c(0.025,0.975))
qnbinom(p=c(0.025,0.975),size=1,mu=20)



# uniform distribution
## parameters for minimum and maximum

limits <- seq(0,10,by=0.01)
z <- dunif(x=limits, min=0, max=5)
names(z) <- limits
plot(x=limits, y=z, type="l", xlim=c(0,10))

# punif for tail probability

limits <- seq(0,10, by=.1)
z <- punif(q=limits,min=0,max=5)
names(z) <- limits
plot(x=limits, y=z, type="l", xlim=c(0,10))


# qunif for quantiles

qunif(p=c(0.025,0.975), min=0,max=5)


# runif to generate data
hist(runif(n=1000,min=0,max=5))


#-----------------------
# normal distribution

hist(rnorm(n=100,mean=100,sd=2))
hist(rnorm(n=100,mean=2,sd=2))
MyVec <- rnorm(n=100,mean=2,sd=2)
summary(MyVec)
TossZeroes <- MyVec[MyVec>0]
hist(TossZeroes)
summary(TossZeroes)

# gamma distribution - has 2 parameters: shape1, shape2
# continuous, positive values

hist(rgamma(n=100,shape=1,scale=10))


# gamma with shape=1 gives an exponential with scale = mean

hist(rgamma(n=100,shape=0.1,scale=10))

# large shape parameter looks like normal distribution
hist(rgamma(n=100,shape=20,scale=1))

# mean=shape*scale
#variance = shape*scale^2

# Beta distribution
# continuous
#bounded between 0 and 1
# parameter shape1= number of successes +1
# parameter shape2 = number of failures +1

# shape1 = 1 and shape2 = 1    "no data"
hist(rbeta(n=1000,shape1=1,shape2=1),breaks=seq(0,1,length=100))

# shape1 = 2, shape2 = 1
hist(rbeta(n=1000,shape1=2,shape2=1),breaks=seq(0,1,length=100))

# two tosses, 1 head, 1 tail
hist(rbeta(n=1000,shape1=2,shape2=2),breaks=seq(0,1,length=100))

# two tosses, both heads
hist(rbeta(n=1000,shape1=3,shape2=1),breaks=seq(0,1,length=100))

# more data, equal counts
hist(rbeta(n=1000,shape1=20,shape2=20),breaks=seq(0,1,length=100))

# more data, unequal counts
hist(rbeta(n=1000,shape1=20,shape2=10),breaks=seq(0,1,length=100))

# shape parameters less than 1
hist(rbeta(n=1000,shape1=0.4,shape2=0.4),breaks=seq(0,1,length=100))

# Estimating parameters from data
# Maximum likelihood estimation
# maximize p(data|parameters)

library(MASS)
x <- rnorm(1000,mean=92.5,sd=2.5)
hist(x)
fitdistr(x,"normal")
mean(x)
sd(x)

# but what should we fit to?
z <- fitdistr(x,"gamma")
str(z)
# rate = 1/scale
# so here is the estimate of the mean
z$estimate[1]/z$estimate[2]

# estimate of variance
z$estimate[1]/z$estimate[2]^2
