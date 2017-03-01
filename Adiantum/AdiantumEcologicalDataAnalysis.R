# Adiantum Macroscale Ecological Data Analysis
# February 26, 2017
# MWS

AdiantumEcoData <- read.table("Macroscale_AdiantumEcologicalData.csv", header=T, sep=",", stringsAsFactors = FALSE)

# check structure of data frame
str(AdiantumEcoData)
head(AdiantumEcoData)
tail(AdiantumEcoData)

#subset data by species
Aa <- subset(AdiantumEcoData, SpeciesID == "A")
Ap <- subset(AdiantumEcoData, SpeciesID == "P")
Av <- subset(AdiantumEcoData, SpeciesID == "V")

# study ecological variables by species

## Percent Canopy Cover
breakAp <- seq(from=80, to=96, by=2)
print(breakAp)
breakAa <- seq(from=50, to=96, by=2)
breakAv <- seq(from=50, to=96, by=2)

histApPCC <- hist(Ap$Percent.Canopy.Cover, breaks= breakAp, xlab= "Percent Canopy Cover", ylab= "Counts", main="A. pedatum Percent Canopy Cover Distribution", col="blue")

histAaPCC <- hist(Aa$Percent.Canopy.Cover, breaks= breakAa, xlab= "Percent Canopy Cover", ylab= "Counts", main="A. aleuticum Percent Canopy Cover Distribution", col="red")

histAvPCC <- hist(Av$Percent.Canopy.Cover, breaks= breakAv, xlab= "Percent Canopy Cover", ylab="Counts", main="A. viridimontanum Percent Canopy Cover Distribution", col="green")

### Histogram of all three species without subsetting species
histAdiantumPCC <- hist(AdiantumEcoData$Percent.Canopy.Cover, xlab = "Percent Canopy Cover", ylab = "Counts", main = "Adiantum Percent Canopy Cover Distribution", col="pink")

#### PCC combined histograms

#Calculate the range of the graph
xlim <- range(50,100)
ylim <- range(0,histAaPCC$counts,
              histApPCC$counts, histAvPCC$counts)

#Plot the first graph
plot(histAaPCC, xlim = xlim, ylim=ylim, 
     col = rgb(1,0,0,0.4), xlab = 'Percent Canopy Cover', freq=TRUE, main = 'Distribution of Percent Canopy Cover by Species')

#plot the second graph on top of this
opar <- par(new = FALSE)
plot(histApPCC,xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n',
     col = rgb(0,0,1,0.4), add = TRUE,
     freq = TRUE)

#plot of third graph on top of both
opar <- par(new = FALSE)
plot(histAvPCC, xlim = xlim, ylim=ylim, xaxt = 'n', yaxt = 'n', col = rgb (0,1,0,0.4), add = TRUE, freq = TRUE)

## add a legend in the corner
legend('topright',c('A. aleuticum','A. pedatum', 'A. viridimontanum'),
       fill = rgb(1:0,0:1,0.4), bty = 'n',
       border = NA)
par(opar)

##Another method of overlapping histograms

plot( histAaPCC, col=rgb(0,0,1,1/4), xlim= c(50,100), main="Percent Canopy Cover Distribution by Species", xlab="Percent Canopy Cover", ylab="Counts")
plot( histApPCC, col=rgb(1,0,0,1/4), xlim= c(50,100), add=T)
plot( histAvPCC, col=rgb(0,1,0,1/4), xlim= c(50,100), add=T)

legend('topright', c('A.aleuticum', 'A.pedatum', 'A.viridimontanum'), fill=rgb(0:0,1:0,0:1), border=NA)

### PCC ANOVA
SerpPCCAnova <- aov(Percent.Canopy.Cover ~ SpeciesID, data=AdiantumEcoData)
summary(SerpPCCAnova)

### Microslope
AaMicroSlopehist <- hist(Aa$MicroSlope, xlab= "Microslope", ylab= "Frequency", main="A. aleuticum Microslope Distribution", col="red")

ApMicroSlopehist <- hist(Ap$MicroSlope, xlab= "Microslope", ylab= "Frequency", main="A. pedatum Microslope Distribution", col="blue")

AvMicroSlopehist <- hist(Av$Microslope, xlab= "Microslope", ylab= "Frequency", main= "A. viridimontanum Microslope Distribution", col="green")

### Macroslope
AaMacroSlopehist <- hist(Aa$Macroslope, xlab="Macroslope", ylab="Frequency", main = "A. aleuticum Macroslope Distribution", col="red")

ApMacroSlopehist <- hist(Ap$Macroslope, xlab="Macroslope", ylab="Frequency", main = "A. pedatum Macroslope Distribution", col="blue")

AvMacroSlopehist <- hist(Av$Macroslope, xlab="Macroslope", ylab="Frequency", main = "A. viridimontanum Macroslope Distribution", col="red")

### MacroHillSlopeForm
AphillSlopeForm <- Ap$MacroHillslopeForm
print(AphillSlopeForm)

AaHillslopeFormbar <- barplot(AphillSlopeForm, xlab = "Hillslope Form", ylab="Frequency", main= "A. aleuticum Hillslope Form Distribution", col = "red")


## Litter Depth
histAaLitterDepth <- hist(Aa$AvgLitterDepth, xlab = "Average Litter Depth", ylab="Frequency", main= "A. aleuticum Litter Depth Distribution", col = "red")

histApLitterDepth <- hist(Ap$AvgLitterDepth, xlab = "Average Litter Depth", ylab="Frequency", main= "A. pedatum Litter Depth Distribution", col = "blue")

histAvLitterDepth <- hist(Av$AvgLitterDepth, xlab = "Average Litter Depth", ylab="Frequency", main= "A. viridimontanum Litter Depth Distribution", col = "green")

## Overlapping histograms
plot( histAaLitterDepth, col=rgb(0,0,1,1/4), xlim= c(0,4), main="Average Litter Depth Between Species", xlab="Litter Depth", ylab="Counts")
plot( histApLitterDepth, col=rgb(1,0,0,1/4), xlim= c(0,4), add=T)
plot( histAvLitterDepth, col=rgb(0,1,0,1/4), xlim= c(0,4), add=T)

### Litter Depth Anova
SerpLitterDepthAnova <- aov(AvgLitterDepth ~ SpeciesID, data=AdiantumEcoData)
summary(SerpLitterDepthAnova)

## Depth O Layer
AaLitterDepthhist <- hist(Aa$AvgDepth0, xlab = "Average Depth O Layer (cm)", ylab="Frequency", main= "A. aleuticum O Layer Depth Distribution", col = "red")

ApLitterDepthhist <- hist(Ap$AvgLitterDepth, xlab = "Average Depth O Layer (cm)", ylab="Frequency", main= "A. pedatum O Layer Depth Distribution", col = "blue")

AvLitterDepthhist <- hist(Av$AvgLitterDepth, xlab = "Average Depth O Layer (cm)", ylab="Frequency", main= "A. viridimontanum O Layer Depth Distribution", col = "green")
