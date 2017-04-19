# Illustration of structured programming
# 9 March 2017
# MWS

###################################################################################################
# FUNCTIOn: GetData
# read in a .csv file
# input: .csv file
# output: data frame
#-----------------------------------------------------------------------------------------------------
GetData <- function(fileName=NULL){
  if(is.null(fileName)){dataFrame <- data.frame(ID=1:10,
                                               varA <- runif(10),
                                               varB <- runif(10))
} else {
  dataFrame <- read.table(file=fileName,
                          header=TRUE, 
                          sep=",",
                          stringsAsFactors=FALSE)
}

return(dataFrame)
}

GetData()

###################################################################################################
# FUNCTIOn: FitRegressionModel
# fits an ordinary least squares regression
# input: x and y numeric vectors of same length
# output: entire model summary from lm
#-----------------------------------------------------------------------------------------------------
FitRegressionModel <- function(xVar=runif(10),
                            yVar=runif(10)){
  dataFrame <- data.frame(xVar,yVar)
  regModel <- lm(yVar~xVar,data=dataFrame)
  return(summary(regModel))
}

###################################################################################################
# FUNCTIOn: SummarizeOutput
# pull elements from lm model summary list
# input: list from model summary call of lm
# output: vector of regression residuals
#-----------------------------------------------------------------------------------------------------
SummarizeOutput <- function(z=NULL){
if(is.null(z)) {
  z <- summary(lm(runif(10)~runif(10)))
}
return(z$residuals)
}

###################################################################################################
# FUNCTION: GraphResults
# graph data and fited OLS line
# input: x and y vectors of numeric. same length.
# output: creates graph
#-----------------------------------------------------------------------------------------------------
GraphResults <- function(xVar=runif(10),
                         yVar=runif(10)){
dataFrame <- data.frame(xVar,yVar)
plot(y=dataFrame$yVar,
     x=dataFrame$xVar,
     pch=21,
     bg="lightblue",
     cex=2)
regModel <- lm(yVar~xVar,data=dataFrame)
abline(regModel)

message("Message: Regression graph created")
}

GraphResults()

#--------------------------------------------------------------------------------------------------
# Global Variables
antFile <- "antcountydata.csv" # New ENgland Ant Data
xCol <- 7 # column 7= latitude centroid of county
yCol <- 5 # column 5= number of ant species
#--------------------------------------------------------------------------------------------------

# Program Body
temp1 <- GetData(fileName=antFile)

x <- temp1[,xCol]
y <- temp1[,yCol]

temp2 <- FitRegressionModel(xVar=x,yVar=y)
temp3 <- SummarizeOutput(temp2)
GraphResults(xVar=x,yVar=y)

print(temp3)
print(temp2)
