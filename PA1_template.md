---
title: "Reproducible Research: Peer Assessment 1"
author: "Author: H. Moore"
date: "Date: 2019-07-13"
output: 
  html_document:
    keep_md: true
---

## Load packages and libraries

```r
library(data.table)
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

## Loading and preprocessing the data
 1. Show any code that is needed to load the data (i.e. read.csv())

```r
projectdata <- "activity.zip"

if (!file.exists(projectdata)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, projectdata, method="curl")
}  

if (!file.exists("activity")) { 
  unzip(projectdata) 
}
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityDT <- data.table::fread("activity.csv", na.strings = "?")
activityFDT <- activityDT[(steps != 'NA')]
activityFDT[, steps := as.integer(activityFDT$steps)]
activityFDT[, date := as.Date(activityFDT$date, format = "%Y-%m-%d")]
```


## What is mean total number of steps taken per day?

```r
stepsPerDaySum <- activityFDT[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("steps"), by = date]

hist(stepsPerDaySum[, stepsPerDaySum$steps], 
     ylim = c(0,30),
     main="Total Steps Per Day",
     xlab = "Steps",
     ylab = "Days",
     col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Calculate and report the mean and median of the total number of steps taken per day

```r
stepsPerDayMeanMedian <- activityFDT[, list( mean(steps), median(steps)), by = date]
colnames(stepsPerDayMeanMedian) <- c("date","meansteps","mediansteps")
stepsPerDayMeanMedian
```

```
##           date  meansteps mediansteps
##  1: 2012-10-02  0.4375000           0
##  2: 2012-10-03 39.4166667           0
##  3: 2012-10-04 42.0694444           0
##  4: 2012-10-05 46.1597222           0
##  5: 2012-10-06 53.5416667           0
##  6: 2012-10-07 38.2465278           0
##  7: 2012-10-09 44.4826389           0
##  8: 2012-10-10 34.3750000           0
##  9: 2012-10-11 35.7777778           0
## 10: 2012-10-12 60.3541667           0
## 11: 2012-10-13 43.1458333           0
## 12: 2012-10-14 52.4236111           0
## 13: 2012-10-15 35.2048611           0
## 14: 2012-10-16 52.3750000           0
## 15: 2012-10-17 46.7083333           0
## 16: 2012-10-18 34.9166667           0
## 17: 2012-10-19 41.0729167           0
## 18: 2012-10-20 36.0937500           0
## 19: 2012-10-21 30.6284722           0
## 20: 2012-10-22 46.7361111           0
## 21: 2012-10-23 30.9652778           0
## 22: 2012-10-24 29.0104167           0
## 23: 2012-10-25  8.6527778           0
## 24: 2012-10-26 23.5347222           0
## 25: 2012-10-27 35.1354167           0
## 26: 2012-10-28 39.7847222           0
## 27: 2012-10-29 17.4236111           0
## 28: 2012-10-30 34.0937500           0
## 29: 2012-10-31 53.5208333           0
## 30: 2012-11-02 36.8055556           0
## 31: 2012-11-03 36.7048611           0
## 32: 2012-11-05 36.2465278           0
## 33: 2012-11-06 28.9375000           0
## 34: 2012-11-07 44.7326389           0
## 35: 2012-11-08 11.1770833           0
## 36: 2012-11-11 43.7777778           0
## 37: 2012-11-12 37.3784722           0
## 38: 2012-11-13 25.4722222           0
## 39: 2012-11-15  0.1423611           0
## 40: 2012-11-16 18.8923611           0
## 41: 2012-11-17 49.7881944           0
## 42: 2012-11-18 52.4652778           0
## 43: 2012-11-19 30.6979167           0
## 44: 2012-11-20 15.5277778           0
## 45: 2012-11-21 44.3993056           0
## 46: 2012-11-22 70.9270833           0
## 47: 2012-11-23 73.5902778           0
## 48: 2012-11-24 50.2708333           0
## 49: 2012-11-25 41.0902778           0
## 50: 2012-11-26 38.7569444           0
## 51: 2012-11-27 47.3819444           0
## 52: 2012-11-28 35.3576389           0
## 53: 2012-11-29 24.4687500           0
##           date  meansteps mediansteps
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsPerDayInterval <- activityFDT[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("steps"), by = interval]

with (stepsPerDayInterval, 
      plot(x = interval, y = steps,
              type = "l",
              xlab = "Interval",
              ylab = "Steps",
              main = "Average Number of Steps Per Interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
as.numeric(stepsPerDayInterval$interval[which.max(stepsPerDayInterval$steps)])
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
as.numeric(count(activityDT[(steps == 'NA')]))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFillNAs <- activityDT
activityFillNAStepVal <- mutate(activityFillNAs, steps = replace(steps, steps=='NA', 0))
activityFillNAStepVal <- data.table(activityFillNAStepVal)
activityFillNAStepVal[, steps := as.integer(activityFillNAStepVal$steps)]
activityFillNAStepVal[, date := as.Date(activityFillNAStepVal$date, format = "%Y-%m-%d")]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  Calculate and report the mean and median of the total number of steps taken per day with no steps NA values.

```r
stepsPerDaySumMeanMedianNoNas <- activityFillNAStepVal[, list( sum(steps), mean(steps), median(steps)), by = date]
colnames(stepsPerDaySumMeanMedianNoNas) <- c("date","sumsteps","meansteps","mediansteps")

hist(stepsPerDaySumMeanMedianNoNas[, stepsPerDaySumMeanMedianNoNas$sumsteps], 
     ylim = c(0,30),
     main="Total Steps Per Day",
     xlab = "Steps",
     ylab = "Days",
     col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
stepsPerDayMeanMedianNoNas <- activityFillNAStepVal[, list(mean(steps), median(steps)), by = date]
colnames(stepsPerDayMeanMedianNoNas) <- c("date","meansteps","mediansteps")
stepsPerDayMeanMedianNoNas
```

```
##           date  meansteps mediansteps
##  1: 2012-10-01  0.0000000           0
##  2: 2012-10-02  0.4375000           0
##  3: 2012-10-03 39.4166667           0
##  4: 2012-10-04 42.0694444           0
##  5: 2012-10-05 46.1597222           0
##  6: 2012-10-06 53.5416667           0
##  7: 2012-10-07 38.2465278           0
##  8: 2012-10-08  0.0000000           0
##  9: 2012-10-09 44.4826389           0
## 10: 2012-10-10 34.3750000           0
## 11: 2012-10-11 35.7777778           0
## 12: 2012-10-12 60.3541667           0
## 13: 2012-10-13 43.1458333           0
## 14: 2012-10-14 52.4236111           0
## 15: 2012-10-15 35.2048611           0
## 16: 2012-10-16 52.3750000           0
## 17: 2012-10-17 46.7083333           0
## 18: 2012-10-18 34.9166667           0
## 19: 2012-10-19 41.0729167           0
## 20: 2012-10-20 36.0937500           0
## 21: 2012-10-21 30.6284722           0
## 22: 2012-10-22 46.7361111           0
## 23: 2012-10-23 30.9652778           0
## 24: 2012-10-24 29.0104167           0
## 25: 2012-10-25  8.6527778           0
## 26: 2012-10-26 23.5347222           0
## 27: 2012-10-27 35.1354167           0
## 28: 2012-10-28 39.7847222           0
## 29: 2012-10-29 17.4236111           0
## 30: 2012-10-30 34.0937500           0
## 31: 2012-10-31 53.5208333           0
## 32: 2012-11-01  0.0000000           0
## 33: 2012-11-02 36.8055556           0
## 34: 2012-11-03 36.7048611           0
## 35: 2012-11-04  0.0000000           0
## 36: 2012-11-05 36.2465278           0
## 37: 2012-11-06 28.9375000           0
## 38: 2012-11-07 44.7326389           0
## 39: 2012-11-08 11.1770833           0
## 40: 2012-11-09  0.0000000           0
## 41: 2012-11-10  0.0000000           0
## 42: 2012-11-11 43.7777778           0
## 43: 2012-11-12 37.3784722           0
## 44: 2012-11-13 25.4722222           0
## 45: 2012-11-14  0.0000000           0
## 46: 2012-11-15  0.1423611           0
## 47: 2012-11-16 18.8923611           0
## 48: 2012-11-17 49.7881944           0
## 49: 2012-11-18 52.4652778           0
## 50: 2012-11-19 30.6979167           0
## 51: 2012-11-20 15.5277778           0
## 52: 2012-11-21 44.3993056           0
## 53: 2012-11-22 70.9270833           0
## 54: 2012-11-23 73.5902778           0
## 55: 2012-11-24 50.2708333           0
## 56: 2012-11-25 41.0902778           0
## 57: 2012-11-26 38.7569444           0
## 58: 2012-11-27 47.3819444           0
## 59: 2012-11-28 35.3576389           0
## 60: 2012-11-29 24.4687500           0
## 61: 2012-11-30  0.0000000           0
##           date  meansteps mediansteps
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

 Yes, the values between 0 and 5000 are not skewed.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityFillNAStepValWkDT <- activityFillNAStepVal
activityFillNAStepValWkDT[, date := as.Date(activityFillNAStepValWkDT$date)]
activityFillNAStepValWkDT <- mutate(activityFillNAStepValWkDT,weekday=weekdays(date))
activityFillNAStepValWkDT$weekday[!activityFillNAStepValWkDT$weekday=="Saturday" & !activityFillNAStepValWkDT$weekday=="Sunday"]<-"Weekday"
activityFillNAStepValWkDT$weekday[activityFillNAStepValWkDT$weekday=="Saturday" | activityFillNAStepValWkDT$weekday=="Sunday"]<-"Weekend"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activityFillNAStepValWkDT <- data.table(activityFillNAStepValWkDT)
activityFillNAStepValWkDTg <- group_by(activityFillNAStepValWkDT,interval,weekday)
activityFillNAStepValWkDTSz <- summarize(activityFillNAStepValWkDTg,mean=mean(steps))
activityFillNAStepValWkDTSz$weekday < -as.factor(activityFillNAStepValWkDTSz$weekday)
```

```
## Warning in Ops.factor(as.factor(activityFillNAStepValWkDTSz$weekday)): '-'
## not meaningful for factors
```

```
##   [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
##  [24] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
##  [47] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
##  [70] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
##  [93] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [116] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [139] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [162] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [185] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [208] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [231] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [254] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [277] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [300] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [323] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [346] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [369] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [392] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [415] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [438] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [461] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [484] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [507] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [530] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [553] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [576] NA
```

```r
xyplot(mean ~ interval | weekday, data=activityFillNAStepValWkDTSz, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
