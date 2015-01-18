# Reproducible Research: Peer Assessment 1


```r
library("Hmisc")
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
## Loading and preprocessing the data
```

```r
unzip(zipfile="activity.zip")
setwd("/home/chase/repro/RepData_PeerAssessment1/")
activitydata <- read.csv("activity.csv",header=TRUE)
activityday <- transform(activitydata,date=as.Date(date))
time <- formatC(activitydata$interval/100, 2, format = "f")
activitydata$date.time <- as.POSIXct(paste(activitydata$date, time), format = "%Y-%m-%d %H.%M", 
    tz = "GMT")
activitydata$time <- format(activitydata$date.time, format = "%H:%M:%S")
activitydata$time <- as.POSIXct(activitydata$time, format = "%H:%M:%S")
```

## What is mean total number of steps taken per day?
First, Lets transform the data a bit to give us totals by day.


```r
steps <- tapply(activityday$steps, activityday$date, FUN = sum,na.rm=TRUE)
```
Now, Let's Plot This to a Histogram.


```r
hist(steps,breaks=61,main="Steps Per Day By Frequency",xlab="Daily Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Now, Let's calculate Mean and Median using these day totals.


```r
mean(steps,na.rm=TRUE)
```

```
## [1] 9354
```

```r
median(steps,na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

First, we are going to calculate the averages for each interval.

```r
interval <- tapply(activitydata$steps, activitydata$time,mean,na.rm=TRUE)
```

Now we load it into a data frame.


```r
patterndata <- data.frame(time = as.POSIXct(names(interval)), interval = interval)
```


```r
plot(y=patterndata$interval,x=patterndata$time,type='l',xlab="Time of Day",ylab="Mean Number of Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Now Lets Find the five minute interval with the highest number of steps.


```r
which.max(patterndata$interval)
```

```
## 2014-08-17 08:35:00 
##                 104
```

## Imputing missing values

Let's find all the missing values.


```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

Now let's fill in the missing data with the average for that interval.


```r
filledactivity <- activitydata
filledactivity$steps <- with(filledactivity,impute(steps,mean))
filledactivityday <- transform(filledactivity,date=as.Date(date))
filledsteps <- tapply(filledactivityday$steps, filledactivityday$date, FUN = sum,na.rm=TRUE)
```
Now, Let's Plot This to a Histogram.


```r
hist(filledsteps,breaks=61,main="Steps Per Day By Frequency",xlab="Daily Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

Now, Let's calculate Mean and Median using these day totals.


```r
mean(filledsteps)
```

```
## [1] 10766
```

```r
median(filledsteps)
```

```
## [1] 10766
```

As we can see, imputing the NA values has raised the mean and median.


## Are there differences in activity patterns between weekdays and weekends?

Let's isolate the weekends from the weekdays.


```r
weekday <- function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday")) {
        return("weekend")
    } else {
        return("weekday")
    }
}

filledactivity$weekday <- as.factor(sapply(filledactivity$date.time, weekday))
```

Let's subset the data into weekend and weekday datasets


```r
weekends <- filledactivity[filledactivity$weekday=="weekend",]
weekday <- filledactivity[filledactivity$weekday=="weekday",]

weekday <- as.vector(tapply(weekday$steps, weekday$interval, mean))
weekends <- as.vector(tapply(weekends$steps, weekends$interval, mean))
```

Now Let's Plot it


```r
int <- as.numeric(filledactivity$interval[1:288])

par(mfrow=c(2,1))
plot(int,weekends,type='l',main="Weekend",ylab="Steps",xlab="Interval")
plot(int,weekday,type='l',main="Weekday",ylab="Steps",xlab="Interval")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
