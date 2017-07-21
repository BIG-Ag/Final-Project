## THis is the final project for Coursera DataScience Class5
## Charles 07/20/2017

## Set working directory and load data
setwd("C:/Study/Coursera/1 Data-Science/2 RStudio/5 Class 5/4 Final-Project/Final-Project")
dataStorm <- read.csv("repdata%2Fdata%2FStormData.csv.bz2", na.strings = "NA")
str(dataStorm)
head(dataStorm)
names(dataStorm)

## Set library
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

## Process data
# Subset
columnsKeep <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
dataSub <- select(dataStorm, columnsKeep)
head(dataSub)
# Transfer exp
indexProperty <- NA
index <- grep("H|h",dataSub$PROPDMGEXP)
indexProperty[index] <- 1e2
index <- grep("K|k",dataSub$PROPDMGEXP)
indexProperty[index] <- 1e3
index <- grep("M|m",dataSub$PROPDMGEXP)
indexProperty[index] <- 1e6
index <- grep("B|b",dataSub$PROPDMGEXP)
indexProperty[index] <- 1e9
indexProperty[is.na(indexProperty)] <- 0

indexCorp <- NA
index <- grep("H|h",dataSub$CROPDMGEXP)
indexCorp[index] <- 1e2
index <- grep("K|k",dataSub$CROPDMGEXP)
indexCorp[index] <- 1e3
index <- grep("M|m",dataSub$CROPDMGEXP)
indexCorp[index] <- 1e6
index <- grep("B|b",dataSub$CROPDMGEXP)
indexCorp[index] <- 1e9
indexCorp[is.na(indexCorp)] <- 0
# make new dataframe
dataRecal <- dataSub %>%
        mutate(totalInjury = FATALITIES+INJURIES) %>%
        mutate(indexOfProperty = indexProperty) %>%
        mutate(indexOfCrop = indexCorp) %>%
        mutate(totalProperty = PROPDMG * indexOfProperty) %>%
        mutate(totalCrop = CROPDMG * indexOfCrop) %>%
        mutate(totalDamage = totalProperty + totalCrop)

head(dataRecal)

## Question1
dataForHealth <- dataRecal %>%
        group_by(EVTYPE) %>%
        summarise(numFatalities = sum(FATALITIES),
                  numInjuries = sum(INJURIES),
                  numTotal = sum(totalInjury)) %>%
        arrange(desc(numTotal))
dataForHealth <- as.data.frame(dataForHealth)

g1 <- ggplot(head(dataForHealth,10), aes(x=reorder(EVTYPE,numFatalities),y=numFatalities))
q1 <- g1 + geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("Population health impact - Top 10") +
        ylab("Number of fatalities") +
        xlab("Type of events")
g2 <- ggplot(head(dataForHealth,10), aes(x=reorder(EVTYPE,numInjuries),y=numInjuries))
q2 <- g2 + geom_bar(stat = "identity") +
        coord_flip() +
        ylab("Number of injuries") +
        xlab("Type of events")
grid.arrange(q1,q2,nrow=2)

dataForHealth2 <- select(dataForHealth, c(1,2,3))
dataForHealth2 <- melt(head(dataForHealth2,10), id.vars = "EVTYPE")
g3 <- ggplot(dataForHealth2, aes(x=reorder(EVTYPE,value), y=value, fill=variable))
q3 <- g3 + geom_bar(stat = "identity") +
        coord_flip()
plot(q3)


## Question2
dataForEco <- dataRecal %>%
        group_by(EVTYPE) %>%
        summarise(numProperty = sum(totalProperty),
                  numCrop = sum(totalCrop),
                  numTotal = sum(totalDamage)) %>%
        arrange(desc(numTotal))
dataForEco <- as.data.frame(dataForEco)
g1 <- ggplot(head(dataForEco,10), aes(x=reorder(EVTYPE,numProperty),y=numProperty))
q1 <- g1 + geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("Economic consequences - Top 10") +
        ylab("Property damage") +
        xlab("Type of events")
g2 <- ggplot(head(dataForEco,10), aes(x=reorder(EVTYPE,numCrop),y=numCrop))
q2 <- g2 + geom_bar(stat = "identity") +
        coord_flip() +
        ylab("Crop damage") +
        xlab("Type of events")
grid.arrange(q1,q2,nrow=2)

























