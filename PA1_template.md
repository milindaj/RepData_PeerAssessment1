Reproducible Research: Peer Assessment 1
==========================================
Milind Joshi 15th Feb

### Global settings

```r
echo = TRUE  # Always print code
library(ggplot2)
library(lattice)
```

### Loading and preprocessing the data

```r
unzip("activity.zip")
actData <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
actData$month <- as.numeric(format(actData$date, "%m"))
# create final view of data for abalysis by ommiting NAs
actDataF <- na.omit(actData)
rownames(actDataF) <- 1:nrow(actDataF)
head(actDataF)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(actDataF)
```

```
## [1] 15264     4
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
ggplot(actDataF, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.8) + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalStepsPerDay <- aggregate(actDataF$steps, list(Date = actDataF$date), FUN = "sum")$x
mean(totalStepsPerDay)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(totalStepsPerDay)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgDailySteps <- aggregate(actDataF$steps, list(interval = as.numeric(as.character(actDataF$interval))), FUN = "mean")
names(avgDailySteps)[2] <- "avgOfSteps"

ggplot(avgDailySteps, aes(interval, avgOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgDailySteps[avgDailySteps$avgOfSteps == max(avgDailySteps$avgOfSteps), ]
```

```
##     interval avgOfSteps
## 104      835   206.1698
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(actData))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will be using 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actData2 <- actData 
for (i in 1:nrow(actData2)) {
    if (is.na(actData2$steps[i])) {
        actData2$steps[i] <- avgDailySteps[which(actData2$interval[i] == avgDailySteps$interval), ]$avgOfSteps
    }
}

head(actData2)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(actData2, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (without missing data)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalStepsPerDay <- aggregate(actData2$steps, 
                           list(Date = actData2$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalStepsPerDay)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalStepsPerDay)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalStepsPerDay)
oldMedian <- median(totalStepsPerDay)
## comparision of mean
newMean - oldMean
```

```
## [1] 0
```

```r
## comparision of median
newMedian - oldMedian
```

```
## [1] 1.188679
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(actData2)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
actData2$weekdays <- factor(format(actData2$date, "%A"))
levels(actData2$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(actData2$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(actData2$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(actData2$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(actData2$steps, 
                      list(interval = as.numeric(as.character(actData2$interval)), 
                           weekdays = actData2$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "avgOfSteps"

xyplot(avgSteps$avgOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
