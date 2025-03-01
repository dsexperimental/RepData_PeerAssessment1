---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





## Loading and preprocessing the data


> 1. Load the data (i.e. read.csv())
> 1. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv(unz("activity.zip","activity.csv"))
```


## What is mean total number of steps taken per day?

> For this part of the assignment, you can ignore the missing values in the dataset.
> 
> 1. Calculate the total number of steps taken per day


```r
daySteps <- data %>% group_by(date) %>% summarize(steps = sum(steps,na.rm = TRUE))
```

> 2. Make a histogram of the total number of steps taken each day


```r
hist(daySteps$steps,main="Total Steps per Day Histogram",xlab="Total Steps")
```

![](PA1_template_files/figure-html/totalSteps-1.png)<!-- -->

> 3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDay <- mean(daySteps$steps)
medianStepsPerDay <- median(daySteps$steps)
```

__Mean Steps per Day:__ 9354.2295082

__Median Steps per Day:__ 10395

## What is the average daily activity pattern?

> 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

_Note on interval value: The interval is in the format %h%m. It was loaded into the data frame as an integer but it is not ordered consecutively. For example it jumps from 155 to 200 to represent the 1:55 and then 2:00 intervals, which is a change of 5 minutes, not 45. This affects the spacing of the interval plot. I am however leaving it in this format rather than converting to a properly ordered value._ 



```r
intervalSteps <- data %>% group_by(interval) %>% summarize(ave = mean(steps,na.rm = TRUE))

plot(intervalSteps$interval,intervalSteps$ave,type="l")
```

![](PA1_template_files/figure-html/perinterval-1.png)<!-- -->

> 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxAveInterval = intervalSteps$interval[which.max(intervalSteps$ave)]
```

__Interval with largest average:__ 835

## Imputing missing values

> Note that there are a number of days/intervals where there are missing values NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
> 
> 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalMissing <- sum(is.na(data$steps))
totalNotMissing <- sum(!is.na(data$steps))

#this is a data frame with the nubmer of missing and not missing values
missingByDay <- data %>% group_by(date) %>% summarize(missing = sum(is.na(steps)), notmissing = sum(!is.na(steps)))

uniqueDayNonMissing <- unique(missingByDay$notmissing)
```

__Total Missing Values:__ 2304

I was inteterested in these initial values too.

__Total Values _not_ Missing:__ 15264

__Number of Non-missing values per Day, unique values:__ 0, 288

From the last value above, we see that days are either fully populated or not populated at all.

> 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
> 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

To impute values, we will replace an interval with the mean number of steps in that interval taken over all days.


```r
# we will create a vector of average steps per interval to align with the rows
# of the data vector.
intervalAveSteps <- rep(intervalSteps$ave,61)
intervalValue <- rep(intervalSteps$interval,61)

# Test to make sure our vectors align!
valuesAlign = sum(intervalValue != data$interval) == 0
if(!valuesAlign) {
  error("The values did not align!")
}

## now create the modified data
missingData <- is.na(data$steps)
imputedData <- data

imputedData$steps[missingData] <- intervalAveSteps[missingData]
```


> 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputedDaySteps <- imputedData %>% group_by(date) %>% summarize(steps = sum(steps))


hist(imputedDaySteps$steps,main="Total Steps per Day Histogram",xlab="Total Steps")
```

![](PA1_template_files/figure-html/imputedperday-1.png)<!-- -->

Here are the mean and median for the total steps each day.


```r
imputedMeanStepsPerDay <- mean(imputedDaySteps$steps)
imputedMedianStepsPerDay <- median(imputedDaySteps$steps)
```

__Mean Steps per Day:__ 1.0766189\times 10^{4}

__Median Steps per Day:__ 1.0766189\times 10^{4}

This might look suspicious at first glance that the mean and median are the same,
but we added a number of days containing exactly the mean number of steps. The 
median value must have fallen in this range of "fixed" data.

### Conclusion Regarding Imputed Data

> Do these values differ from the estimates from the first part of the assignment?

Yes, the mean and median step counts are higher and the number of values in the bottom bin of the 
histogram has gotten much smaller.

> What is the impact of imputing missing data on the estimates of the total daily number of steps?

This can be answered many ways. One effect is that several days with anomalously low counts have been
replaced with more typical numbers.


## Are there differences in activity patterns between weekdays and weekends?

> For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
> 
> 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
modData <- imputedData

#convert the data column to the date class
modData$date <- as.Date(modData$date,"%Y-%m-%d")

#construct the new values and add to the data set
dayOfWeek <- weekdays(modData$date)
isWeekend <- (dayOfWeek == "Saturday") | (dayOfWeek == "Sunday")
dayTypeChar <- rep("weekday",length(modData$date))
dayTypeChar[isWeekend] = "weekend"

modData$daytype <- factor(dayTypeChar)

#create a new data frame with steps and interval,daytype
modIntervalSteps <- modData %>% group_by(interval,daytype) %>% summarize(ave = mean(steps,na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```


> 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ggplot(modIntervalSteps,aes(interval,ave)) + geom_line() + facet_grid(daytype ~ .) + labs(title="Steps per Interval by Day Type")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

I am adding an additional plot to show the two series overlayed. I hope someone doesn't see this and think I did the assignment wrong.


```r
ggplot(modIntervalSteps,aes(interval,ave,col=daytype)) + geom_line(size=1) + labs(title="Steps per Interval by Day Type")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
