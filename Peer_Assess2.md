# Analyze population health and economic impact of weather events in the US
# Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities.

Based on the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, we will respond to two questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

The answers to these questions will appear in the **Results** section of this article.

In the data processing section of the economic impact, I have made some data cleaning and data transformation as described below.

By answering these questions, the authorities can prioritize their actions following weather forecasts warning of such events.

# Data Processing

### R setup section


```r
rm(list=ls())
options(stringsAsFactors = FALSE)

if (!is.element("DBI",installed.packages()) ) {install.packages("DBI")}
library(DBI)

if (!is.element("RSQLite",installed.packages()) ) {install.packages("RSQLite")}
library(RSQLite)

if (!is.element("proto",installed.packages()) ) {install.packages("proto")}
library(proto)

if (!is.element("gsubfn",installed.packages()) ) {install.packages("gsubfn")}
library(gsubfn)

if (!is.element("sqldf",installed.packages()) ) {install.packages("sqldf")}
library(sqldf)

if (!is.element("reshape2",installed.packages()) ) {install.packages("reshape2")}
library("reshape2")

if (!is.element("ggplot2",installed.packages()) ) {install.packages("ggplot2")}
library(ggplot2)

if (!is.element("tcltk",installed.packages()) ) {install.packages("tcltk")}
library(tcltk)
```

### Load data


```r
if (!file.exists("repdata-data-StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                destfile="repdata-data-StormData.csv.bz2", method="curl")
}
storm_data <- read.csv('./repdata-data-StormData.csv.bz2', header=T) ## no need to unzip the file
```

### Process to find the most harmful types of events with respect to population health


```r
## Sum the total number of FATALITIES and INJURIES per Event Type
Impact <- aggregate( cbind( FATALITIES , INJURIES ) ~ EVTYPE , data = storm_data , sum )

## Select the top 10 FATALITIES
High_Impact_F <- sqldf(
  "SELECT * FROM Impact ORDER BY FATALITIES DESC LIMIT 10"
  )

## Select the top 10 INJURIES
High_Impact_I <- sqldf(
  "SELECT * FROM Impact ORDER BY INJURIES DESC LIMIT 10"
)

## Merge the top FATALITIES with the TOP INJURIES
High_Impact <- rbind(High_Impact_F, High_Impact_I)
High_Impact <- High_Impact[!duplicated(High_Impact), ]
```

### Prepare the impact on population data for plotting and prepare the plot


```r
ForPlot <- melt(High_Impact[,c('EVTYPE','FATALITIES','INJURIES')],id.vars = 1)


g <- ggplot(data=ForPlot, aes(x = EVTYPE, y=value, fill=variable)) + 
    geom_bar(stat="identity") + 
    coord_cartesian(ylim=c(0, 10000))  ## Make a note on inhuries from Tornado

g <- g + labs(title = "Most harmful events", x = "Event type", y = "Number of people")

g<- g + theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1))

g <- g + facet_wrap( ~ variable, ncol=1) 
```


### Process to find the Highest impact types of events with respect to economic impact

The economic impact is calculated as the sum of the property damage and the corp damage. 

By exploring the data, I realized that the column with code for damage has additional values to the B, M, K mentioned in the documentation. So, first I explore the data to see if these additional codes are meaningful or not. 



```r
sqldf("SELECT PROPDMGEXP , COUNT(PROPDMGEXP) FROM storm_data GROUP BY PROPDMGEXP")
```

```
##    PROPDMGEXP COUNT(PROPDMGEXP)
## 1                        465934
## 2           +                 5
## 3           -                 1
## 4           0               216
## 5           1                25
## 6           2                13
## 7           3                 4
## 8           4                 4
## 9           5                28
## 10          6                 4
## 11          7                 5
## 12          8                 1
## 13          ?                 8
## 14          B                40
## 15          H                 6
## 16          K            424665
## 17          M             11330
## 18          h                 1
## 19          m                 7
```

```r
## As the NO CODE ("") has significant number of instances, check how many instance have no code but do have a value for damage
sqldf("SELECT PROPDMG FROM storm_data WHERE (PROPDMGEXP = '' AND PROPDMG>0)")
```

```
##    PROPDMG
## 1     0.41
## 2     3.00
## 3     2.00
## 4     4.00
## 5     4.00
## 6    10.00
## 7    10.00
## 8    10.00
## 9     4.00
## 10    5.00
## 11   10.00
## 12   35.00
## 13   75.00
## 14    3.00
## 15   10.00
## 16    1.00
## 17    3.00
## 18   20.00
## 19    2.00
## 20   20.00
## 21   10.00
## 22    1.00
## 23   20.00
## 24    5.00
## 25    4.00
## 26    5.00
## 27    4.00
## 28    6.00
## 29    7.00
## 30    7.00
## 31   10.00
## 32    9.00
## 33    3.00
## 34    2.00
## 35    8.00
## 36    8.00
## 37    6.00
## 38    3.00
## 39    2.00
## 40    7.00
## 41    4.00
## 42    1.00
## 43    3.00
## 44    5.00
## 45    6.00
## 46    5.00
## 47    3.00
## 48    1.00
## 49   10.00
## 50    3.00
## 51    5.00
## 52    3.00
## 53    5.00
## 54    2.00
## 55    3.00
## 56    9.00
## 57    4.00
## 58    3.00
## 59    5.00
## 60    6.00
## 61    3.00
## 62    3.00
## 63    6.00
## 64    3.00
## 65    9.00
## 66    2.00
## 67    4.00
## 68    5.00
## 69    5.00
## 70    2.00
## 71    4.00
## 72    5.00
## 73    3.00
## 74    6.00
## 75   20.00
## 76    3.00
```

There are 76 instances with property damage, but no code for $ value. In this analysis I will ignore them, alas its worth to try to get information and modify the raw data.




```r
sqldf("SELECT CROPDMGEXP , COUNT(CROPDMGEXP) FROM storm_data GROUP BY CROPDMGEXP")
```

```
##   CROPDMGEXP COUNT(CROPDMGEXP)
## 1                       618413
## 2          0                19
## 3          2                 1
## 4          ?                 7
## 5          B                 9
## 6          K            281832
## 7          M              1994
## 8          k                21
## 9          m                 1
```

```r
## As the NO CODE ("") has significant number of instances, check how many instance have no code but do have a value for damage
sqldf("SELECT CROPDMG FROM storm_data WHERE (CROPDMGEXP = '' AND CROPDMG>0)")
```

```
##   CROPDMG
## 1       3
## 2       4
## 3       4
```

In the above tables we can see that we can ignore all codes except K,M,B as the others have relatively moinor impact. 



As the damage is presented with codes for thousand (K), millions (M) and billions (B), I have transformed all of them to be in Billions of $, for both the property damage and the corp damage.



```r
#############################################
## Convert all $ values to Billion's of $
#############################################


## A function to ocnvert to Billions
ConvertToBillions <- function(code) {
  switch(code, K = 1e-6, M = 1e-3, B = 1, 0)
}

###################################################
## Do the transformation for the property damage
###################################################
PropCost <- aggregate (storm_data$PROPDMG, list(type=storm_data$EVTYPE, exp=storm_data$PROPDMGEXP), sum)

PropCost <- sqldf("SELECT * FROM PropCost WHERE exp IN ('B', 'M', 'K,')")

PropCost$Cost <- PropCost$x * sapply(PropCost$exp, ConvertToBillions)


###################################################
#Do the transformation for the corp damage
###################################################
CropCost <- aggregate (storm_data$CROPDMG, list(type=storm_data$EVTYPE, exp=storm_data$CROPDMGEXP), sum)

CropCost <- sqldf("SELECT * FROM CropCost WHERE exp IN ('B', 'M', 'K,')")

CropCost$Cost <- CropCost$x * sapply(CropCost$exp, ConvertToBillions)

DmgCost <-rbind(PropCost, CropCost)
DmgCostAgg<-aggregate(DmgCost$Cost, list(EVTYPE=DmgCost$type), sum)

## Find the top 10 event types
High_Impact_E <- sqldf("SELECT * FROM DmgCostAgg ORDER BY x DESC LIMIT 10")
names(High_Impact_E)[2] <- "Cost"
```

### Prepare the economic impact for plotting and prepare the plot


```r
## Transform th eevent names to factors, so the order will be in program control and not alphabetical
## Set order of evtype according to cost

High_Impact_E$EVTYPE <- factor(High_Impact_E$EVTYPE, levels=High_Impact_E[,1], ordered=TRUE)

gE <- ggplot(data=High_Impact_E, aes(x = EVTYPE, y=Cost, color)) + 
  geom_bar(stat="identity", fill="darkgreen")

gE <- gE + labs(title = "Highest Economic Impact", x = "Event type", y = "Cost Billions USD")

gE <- gE + theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1))
```

# Results

## The most harmful types of events with respect to population health

The following plot describes the most harmful typed of events.

**Note** - As Tornado injuries are very high (above 90,000), we have trimmed the chart so the other events can be visible. 

```r
print (g)
```

![](Peer_Assess2_files/figure-html/Plot health-1.png) 

As can be seen above - TORNADO has the highest inhuruis and also highest fatalities.


## The type of events with the greatest economic consequences


```r
print (gE)
```

![](Peer_Assess2_files/figure-html/Plot Economy-1.png) 

As can be seen above - FLOOD has the highest economic impact



