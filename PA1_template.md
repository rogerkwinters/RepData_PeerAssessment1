# Reproducible Research: Peer Assessment 1
Roger K. Winters  


## Loading and preprocessing the data

- Load the data
- Process/transform the data


```r
activity_data <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity_data$date <- as.Date(activity_data$date, format = '%Y-%m-%d')
```

## What is mean total number of steps taken per day?

- Make a histogram of the total number of steps taken each day


```r
steps_per_day <- aggregate(steps ~ date, activity_data, sum)

hist(steps_per_day$steps, breaks = 8, col = "darkgreen",
     main = paste("Total Number of Steps Taken Each Day"), 
     xlab = "Number of Steps", 
     ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

- Calculate and report the mean and median total number of steps taken per day


```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_daily_pattern <- aggregate(steps ~ interval, activity_data, mean)

plot(avg_daily_pattern$interval, avg_daily_pattern$steps, type = "l", xlab = "5-min Intervals",
        col = "darkgreen", lwd = "2",
        ylab = "Average Number of Steps Taken Across All Days", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_daily_pattern[which.max(avg_daily_pattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
library(Hmisc)
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
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I decided the use the mean of all steps in the activity data to fill in the missing values.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_data_imputed <- activity_data
activity_data_imputed$steps <- as.integer(with(activity_data_imputed, round(impute(steps, mean))))
steps_per_day_imputed <- aggregate(steps ~ date, activity_data_imputed, sum)
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean(steps_per_day_imputed$steps)
```

```
## [1] 10751.74
```

```r
median(steps_per_day_imputed$steps)
```

```
## [1] 10656
```

The average number of steps has declined slightly.


```r
hist(steps_per_day_imputed$steps, breaks = 8, col = "darkgreen",
     main = paste("Total Number of Steps Taken Each Day (Imputed)"), 
     xlab = "Number of Steps", 
     ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)

activity_data_imputed$day_of_week <- ifelse(weekdays(activity_data_imputed$date) %in% c("Saturday", "Sunday"),
        'weekend','weekday')
steps_by_day_of_week <- aggregate(steps ~ interval + day_of_week, activity_data_imputed, mean)

ggplot(steps_by_day_of_week, aes(x = interval,  y = steps)) + 
        geom_line(colour = "darkgreen", size = 1.25) + 
        facet_wrap(~ day_of_week, nrow = 2, ncol = 1) +
        labs(x = "Interval", y = "Average Number of Steps") +
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
