---
title: "RepData_PeerAssessment1"
author: "Elvijs"
date: "2024-08-23"
output: html_document
---

Load the data (i.e. read.csv()).

``` r
activity <- read.csv("activity.csv")
```

Quick look at data to understand the format

``` r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Process/transform the data (if necessary) into a format suitable for your analysis
Change date to Date

``` r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## First part: What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

### Calculate the total number of steps taken per day
Grouping data by date, then summing each date and creating summary data frame

``` r
total_steps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

Quick look at newly created data frame

``` r
head(total_steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### Make a histogram of the total number of steps taken each day
Creating standart histogram with title and name of X axis

``` r
hist(total_steps$steps, breaks = 20, main = "Steps taken per day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

``` r
median(total_steps$steps)
```

```
## [1] 10765
```

## Second part: What is mean total number of steps taken per day?
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Grouping data by interval and calculating the mean and creating a summary data frame

``` r
avg_steps <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```

Creating plot with type = "l"

``` r
plot(avg_steps$interval, avg_steps$steps, type = "l", main = "Average number of steps", xlab = "Intervals of 5 minutes", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Finding the max value and selecting the corresponding interval among all dates with which.max()

``` r
avg_steps$interval[which.max(avg_steps$steps)]
```

```
## [1] 835
```

## Third part: Imputing missing values
### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA NAs)

``` r
sum(is.na(activity))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Here I use the mean of 5 minute intervals to fill in for missing values.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
activity_updated <- activity 
for (i in avg_steps$interval) {
  activity_updated[activity_updated$interval == i & is.na(activity_updated$steps), ]$steps <- avg_steps$steps[avg_steps$interval == i]}
```

Quick look at newly created data frame

``` r
head(activity_updated)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Double check

``` r
sum(is.na(activity_updated))
```

```
## [1] 0
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
total_steps_updated <- aggregate(steps ~ date, data = activity_updated, sum, na.rm = TRUE)
hist(total_steps_updated$steps, breaks = 20, main = "Steps taken per day (updated)", xlab = "Steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

``` r
mean(total_steps_updated$steps)
```

```
## [1] 10766.19
```

``` r
median(total_steps_updated$steps)
```

```
## [1] 10766.19
```

Both means are equal due to approch used (10 766.19), but median have slightly changed (from 10 765 to 10766.19). Inputting missing values with averages leads to more data points that are equal to mean and smaller differencies in distribution.

## Fourth part: Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Here we update activity_updated by adding two new columns and classifying the data based on the day of the weeek.Basically it classifies each data as either "weekend" or "weekday"


``` r
activity_updated$day <- weekdays(activity_updated$date)
activity_updated$week <- ""
activity_updated[activity_updated$day == "Saturday" | activity_updated$day == "Sunday", ]$week <- "weekend"
activity_updated[!(activity_updated$day == "Saturday" | activity_updated$day == "Sunday"), ]$week <- "weekday"
activity_updated$week <- factor(activity_updated$week)
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


``` r
avg_steps_updated <- aggregate(steps ~ interval + week, data = activity_updated, mean)
library(lattice)
xyplot(steps ~ interval | week, data = avg_steps_updated, type = "l",
       layout = c(1, 2), 
       xlab = "Intervals of 5 minutes", 
       ylab = "Average number of steps",
       main = "Average number of steps (weekend & weekday)")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)
