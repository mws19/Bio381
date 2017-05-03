### Functions for Statistical Analysis - Homework 8
## March 8, 2017
## MWS
par(mfrow=c(1,1))
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
                      pch=21,
                      bg="lightblue",
                      main="Linear Regression",
                      xlab="X Variable", 
                      ylab="Y variable",
                      xlim=range(xVar),
                      ylim=range(yVar))
  linRegMod <- lm(yVar~xVar,data=dataframe)
  abline(linRegMod)
  return(lrplot)
  }

linRegPlot()

# tiny data set
rdat <- data.frame(xVar=runif(10,min=0,max=20),yVar=runif(10,min=20,max=40))
linReg(rdat)
linRegPlot(rdat)


#################################################################################
# Function: Logistic Regression
# Description: 
# input: continuous x variable & categorical y variable
# output: estimate of xvar and p value of xvar
# ------------------------------------------------------------------------------
logReg <- function(xVar=rgamma(n=20,shape=5,scale=5), 
                   yVar=rbinom(n=20,size=1,p=0.5)){
  logRegMod <- glm(yVar~ xVar,
                   family=binomial(link="logit"))
  logRegOut <- c(xVarEst=summary(logRegMod)$coefficients[2,1],
                 pValue=summary(logRegMod)$coefficients[2,4])
   return(logRegOut)}

logReg()

# Log Reg plot
logRegPlot <- function(xVar=rgamma(n=20,shape=5,scale=5), 
                       yVar=rbinom(n=20,size=1,p=0.5),dataframe = data.frame(xVar,yVar)){
lrResults <- glm(yVar ~ xVar, family="binomial"(link="logit"))
LRplot <- plot(dataFrame$xVar, y=dataFrame$yVar,
                   pch=21,
                   bg="tan",
                   cex=1,
                   main="Logistic Regression")
LRplot1 <- curve(predict(lrResults,
              data.frame(xVar=x),
              type="response"),
      add=TRUE,
      lwd=2)
return(LRplot1)}

logRegPlot()

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
analVar <- function(xVar=as.factor(rep(c("A","B","C","D","E"),each=3)),
                    yVar=c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))){
                      df=data.frame(xVar,yVar)
          aovMod <- aov(yVar~xVar,data=df)
          aovOut <- summary(aovMod)[[1]][["Pr(>F)"]][1]
         return(aovOut)}
        
analVar()

analVarPlot <- function(xVar=as.factor(rep(c("A","B","C","D","E"),each=3)),yVar=c(rgamma(15,shape=5,scale=5))){
  df <- data.frame(xVar,yVar)
  aovMod <- aov(yVar~xVar,data=df)
  aovPlot <-  boxplot(yVar~xVar,
                      data=df,
                      col=c("red","yellow","blue","green","purple"),
                    xlab=names(xVar),ylab=names(yVar))
  return(aovPlot)}
  
analVarPlot() 

# tiny data set
xVar1 <- as.factor(rep(c("a","b","c"),each=5))
yVar1 <- c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))
analVar(xVar=xVar1,yVar=yVar1)

#################################################################################
# Function: Contingency Table
# input: discrete independent variable and discrete dependent variable
# output: 
# ------------------------------------------------------------------------------
contTable <- function(x=c(22,40,60),y=c(40,80,45),datamatrix=rbind(x,y)){
  rownames(datamatrix)=c("Cold","Warm")
  colnames(datamatrix)=c("Species1","Species2","Species3")
  contTableMod <- chisq.test(datamatrix)
  contTableOut <- print(chisq.test(datamatrix)[3])
          return(contTableOut)}

contTable()

contPlot <- function(x=c(22,40,60),y=c(40,80,45),datamatrix=rbind(x,y)){
  rownames(datamatrix)=c("Cold","Warm")
  colnames(datamatrix)=c("Species1","Species2","Species3")
  mplot <- mosaicplot(x=datamatrix,col=c("red","blue","yellow"),shade=F)
  return(mplot)}

contPlot()
  