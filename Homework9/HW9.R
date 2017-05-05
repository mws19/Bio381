
adDat <- read.table("QuantitativeDataAdiantum1.csv",sep=",",header=T,stringsAsFactors = F)
head(adDat)
tail(adDat)

# Clean up data set by changing NA values to 0
# Transform NA values to 0
adDat[is.na(adDat)]<- 0
head(adDat)
dim(adDat)


# dgamma distribution
library(MASS)
fitdistr(adDat$Mg,"gamma")

ranMg <- rgamma(30,1.08,.00102)

fitdistr(adDat$Ca,"gamma")
ranCa <- rgamma(30,2.71,.004)

linReg(ranMg,ranCa)
linRegPlot(ranMg,ranCa)


# depth A soil layer - test with random data
om <- adDat$Ompct
fitom <- fitdistr(om,"gamma")

randOM <- rgamma(30,fitom$estimate[1],fitom$estimate[2])

spID <- c("A","V")
ranSpID <- rep(spID,length=30)
ranDat <- data.frame(ranSpID,randOM)

analVar(xVar=ranDat$ranSpID,yVar=ranDat$randOM)


# depth A soil layer -test with ranBling data
randOM.A <- rgamma(15,10,.2)
randOM.V <- rgamma(15,5,.2)

spIDV <- c("V")
spIDA <- c("A")

ranSPV <- rep(spIDV,length=15)
ranSPA <- rep(spIDA,length=15)

ranBlingSpID <- c(ranSPV,ranSPA)
ranBlingOM <- c(randOM.V,randOM.A)

ranDatBling<-data.frame(ranBlingSpID,ranBlingOM)

ANOV(xVar=ranDatBling$ranBlingSpID,yVar=ranDatBling$ranBlingOM)
ANOVplot(xVar=ranDatBling$ranBlingSpID,yVar=ranDatBling$ranBlingOM)
