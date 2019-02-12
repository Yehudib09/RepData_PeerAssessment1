### Loading and preprocessing the data

1.  Load the data (i.e. read.csv())

``` r
activity <- read.csv("/Users/yehudibaptiste/desktop/r programming/activity.csv", as.is = TRUE)
```

1.  Process/transform the data (if necessary) into a format suitable for your analysis

``` r
clean_data <- good_act <- activity[complete.cases(activity), ]
```

1.  Inspect the clean data

``` r
summary(clean_data)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:15264       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0

``` r
head(clean_data)
```

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

### What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

``` r
daily_steps <- aggregate(steps ~ date, clean_data, sum)
```

1.  Make a histogram of the total number of steps taken each day

``` r
hist(daily_steps$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
(mean(daily_steps$steps))
```

    ## [1] 10766.19

``` r
(median(daily_steps$steps))
```

    ## [1] 10765

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
average_daily_steps_interval <- aggregate(steps ~ interval, clean_data, mean)
plot(average_daily_steps_interval$interval, average_daily_steps_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
highest_average_steps_interval <- which.max(average_daily_steps_interval$steps)
average_daily_steps_interval[highest_average_steps_interval, ]$interval
```

    ## [1] 835

``` r
average_daily_steps_interval[highest_average_steps_interval, ]$steps
```

    ## [1] 206.1698

### Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
missingdata <- activity[!complete.cases(activity), ]
nrow(missingdata)
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- average_daily_steps_interval$steps[which(average_daily_steps_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}
```

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
new_daily_steps <- aggregate(steps ~ date, activity, sum)
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
hist(new_daily_steps$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
mean(new_daily_steps$steps)
```

    ## [1] 10766.19

``` r
median(new_daily_steps$steps)
```

    ## [1] 10766.19

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

``` r
activity$day_type <- as.factor(sapply(activity$date, week_day))
library(ggplot2)
new_daily_steps<- aggregate(steps ~ interval+day_type, activity, mean)
plt <- ggplot(new_daily_steps, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("No of steps Per Interval by day type")
print(plt)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-15-1.png)
