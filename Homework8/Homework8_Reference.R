# CompBio Homework 8
# March 8, 2017
# MWS

############LINEAR REGRESSION##################################

# continuous - continuous data
xVar <- 1:10
yVar <- runif(10)
dataFrame <- data.frame(xVar,yVar)

# model
regModel <- lm(yVar~xVar,data=dataFrame)  # linear regression function

# model output
print(regModel)
print(summary(regModel))

# plot
plot(y=dataFrame$yVar,x=dataFrame$xVar,pch=21,bg="lightblue",cex=2, main="Linear Regression", xlab="X variable", ylab="Y variable")
abline(regModel)

############## ANOVA #####################
# Dependent - continuous; independent - discrete

# data
xVar <- as.factor(rep(c("Control","Heated","Cooled"),each=5))
yVar <- c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))
dataFrame <- data.frame(xVar,yVar)

# model
anovaModel <- aov(yVar~xVar,data=dataFrame)  ## function that runs ANOVA

# model output
print(anovaModel)
summary(anovaModel)

# plot
boxplot(yVar~xVar,data=dataFrame,col=c("grey","thistle","orchid"))


############################ CONTINGENCY TABLE ANALYSIS ###################
# Both discrete

# data
vec1 <- c(50,66,22)
vec2 <- c(120,22,30)
dataMatrix <- rbind(vec1,vec2)
rownames(dataMatrix) <- c("Cold","Warm")
colnames(dataMatrix) <-c("Aphaenogaster",
                         "Camponotus",
                         "Crematogaster")
print(dataMatrix)

# model + model output
print(chisq.test(dataMatrix))

# plot
mosaicplot(x=dataMatrix,
           col=c("goldenrod","grey","black"),
           shade=FALSE)
barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))

# expected data counts
chisq.test(dataMatrix)$expected

# verify expected counts
(sum(dataMatrix[ ,1])*sum(dataMatrix[1, ]))/sum(dataMatrix)

# Comparing the expected and observed visually
par(mfrow=c(2,1))
expected <- as.matrix(chisq.test(dataMatrix)$expected) # put our expected counts into a matrix

barplot(height=expected,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))
barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))


############ LOGISTIC REGRESSION ################

# data
xVar <- rgamma(n=20,shape=5,scale=5)
yVar <- rbinom(n=20,size=1,p=0.5)
dataFrame <- data.frame(xVar,yVar)

# model
# glm function performs logistic regression
logRegMod <- glm(yVar ~ xVar,
                 data=dataFrame,
                 family=binomial(link="logit"))
# model output
print(logRegMod)
summary(logRegMod)

par(mfrow=c(1,1))

# plot
plot(x=dataFrame$xVar, y=dataFrame$yVar,pch=21,bg="tan",cex=2.5)
curve(predict(logRegMod,data.frame(xVar=x),type="response"),add=TRUE,lwd=2)

