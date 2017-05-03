### Functions for Statistical Analysis - Homework 8
## March 8, 2017
## MWS

#################################################################################
# Function: Regression
# Description: Fits a regression line to a scatterplot of x and y values
# input: continuous x & y variables
# output: abline & scatterplot
# ------------------------------------------------------------------------------

linReg <- function(xVar= 1:100,yVar=runif(100,min=0,max=100),
                   dataframe=data.frame(xVar,yVar)){
  linRegMod <- lm(yVar~xVar,data=dataframe)
  linRegOut <- c(slope=summary(linRegMod)$coefficients[2,1],
                 pValue=summary(linRegMod)$coefficients[2,4])
    return(linRegOut)}

linReg()

linRegPlot <-  function(xVar=1:100,yVar=runif(100,min=0,max=100),dataframe=data.frame(xVar,yVar)){
  lrplot <- plot(y=dataframe$yVar,
                       x=dataframe$xVar,
                 cex=2, 
                 pch=20,
                 bg="lightblue",
                       main="Linear Regression",
                       xlab="X Variable", 
                       ylab="Y variable",
                       xlim=range(xVar),
                       ylim=range(yVar)
                      )
  return(linRegPlot)}

linRegPlot()

# tiny data set
linReg(xVar=runif(10,min=0,max=20),yVar=runif(10,min=20,max=40))



#################################################################################
# Function: Logistic Regression
# input: continuous x variable & categorical y variable
# output: estimate of xvar and p value of xvar
# ------------------------------------------------------------------------------
logReg <- function(xVar=rgamma(n=20,shape=5,scale=5), 
                   yVar=rbinom(n=20,size=1,p=0.5)){
  logRegMod <- glm(yVar~ xVar,
                   family=binomial(link="logit"))
  logRegOut <- c(xVarEst=summary(logRegMod)$coefficients[2,1],
                 pValue=summary(logRegMod)$coefficients[2,4])
  dataframe<-data.frame(xVar, yVar)
  logRegPlot <- plot(x=dataframe$xVar, 
                     y=dataframe$yVar,
                     pch=21,
                     bg="tan",
                     cex=1,
                     main="Logistic Regression",
                     xlab="X variable",
                     ylab="Y Variable")
  curve(predict(logRegMod,
                data.frame(x=xVar),
                type="response"),
                add=TRUE,
                lwd=2)
  return(logRegOut)}

logReg()

# tiny data set
xVar <- rgamma(n=30,shape=4,scale=4)
yVar <- rbinom(n=30,size=1,p=0.5)
datfram <- data.frame(xVar,yVar)
 
logReg(xVar=xVar,yVar=yVar,dataframe=datfram)

#################################################################################
# Function: ANOVA
# input: categorical x variable and continuous y variable
# output: p value
# ------------------------------------------------------------------------------
analVar <- function(xVar1=as.factor(rep(c("A","B","C","D","E"),each=5)),
                    yVar1=c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))){
                      df=data.frame(xVar1,yVar1)
          aovMod <- aov(yVar1~xVar1,data=df)
          aovOut <- summary(aovMod)[[1]][["Pr(>F)"]][1]
          analVarplot <- boxplot(yVar1~xVar1,
                                 data=df,
                                 col=c("red","yellow","blue","green","purple"),
                                 xlab=names(xVar1),ylab=names(yVar1))
          
          return(aovOut)}
        
analVar()

# tiny data set
xVar1 <- as.factor(rep(c("a","b","c"),each=5))
yVar1 <- c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))
analVar(xVar1=xVar1,yVar1=yVar1)

#################################################################################
# Function: Contingency Table
# input: discrete independent variable and discrete dependent variable
# output: 
# ------------------------------------------------------------------------------
contTable <- function(x=c(22,40,60),y=c(40,80,45),datamatrix=rbind(x,y)){
          contTableMod <- chisq.test(datamatrix)
          contTableOut <- print(chisq.test(datamatrix)[3])
          return(contTableOut)}

contTable()
