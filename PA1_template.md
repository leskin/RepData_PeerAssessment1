# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We first use read.csv to read the data into a data frame, and convert the date from a factor variable to a Date variable.


```r
stepData <- read.csv("activity.csv", header=TRUE)
str(stepData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
stepData$date <- as.Date(stepData$date, format = "%Y-%m-%d")
str(stepData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

Use the dplyr package to calculate the total number of steps for each day
and then make a histogram of the results.  We ignore any missing data values for this plot.


```r
library(dplyr)
stepsPerDay <- stepData %>% group_by(date) %>% summarise_each(funs(sum), steps)
hist(stepsPerDay$steps, breaks = 25, xlab = "Total Steps Per Day", 
     main = "Histogram of Total Steps per Day (ignoring missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
# Note that we were asked to create a histogram, but if we wanted to make a bargraph instead, 
# the following command would do so:
# barplot(stepsPerDay$steps, names.arg = stepsPerDay$date)
```

Next we calculate the mean number of steps each day and the median number of steps each day and report the results.


```r
meanSteps <- mean(stepsPerDay$steps, na.rm = TRUE)
medianSteps <- median(stepsPerDay$steps, na.rm = TRUE)
```

The mean number of steps per day = 10766.19 and the median number of steps per day = 10765

## What is the average daily activity pattern?

We are interested in the daily activity pattern, so we plot the average number of steps taken during each 5-minute interval reported, averaged across all days.  We also calculate and report the 5-minute interval that contains the maximum number of steps taken, on average across all the days in the dataset.


```r
library(lattice)
intervalMeanData <- stepData %>% group_by(interval) 
intervalMeanData <- intervalMeanData %>% summarise_each(funs(mean(.,na.rm = TRUE)), steps)
xyplot(steps ~ interval, data = intervalMeanData,
         type = "l",
         xlab = "Interval",
         ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxInterval <- intervalMeanData %>% top_n(steps,n=1)
maxInterval
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

The 5-minute interval at 835 contains the maximum number of steps (206.1698113) on average, across all the days in the dataset.

## Imputing missing values

The dataset contains missing values (coded as NA).  These can introduce bias into some calculations or summaries of the data. First we use the summary command to list which variables have missing values.


```r
summary(stepData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

As shown above, there are 2304 missing (NA) values in the dataset for the number of steps in a 5-minute interval.  Each of the missing NA values will be replaced by the mean for the given time interval.  The mean values for each time interval are contined in the intervalMeanData data frame.  A new data frame (modifiedStepData) is created which is equal to the original dataset but with the missing data filled in.  We use the summary command on the new data frame to confirm that all missing data has been replaced (and we see that no NA values are reported for the new dataset).


```r
modifiedStepData <- stepData
for (i in 1:nrow(modifiedStepData)){
  if (is.na(modifiedStepData$steps[i])) {
    desiredInterval <- modifiedStepData$interval[i]
    newStepsValue <- intervalMeanData$steps[intervalMeanData$interval == desiredInterval]
    modifiedStepData$steps[i] <- newStepsValue
  }
}
summary(modifiedStepData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

We now plot the histogram for the total number of steps per day for the dataset where missing values have been replaced and compare it to the histogram for the oringal dataset.

```r
library(dplyr)
modifiedStepsPerDay <- modifiedStepData %>% group_by(date) %>% summarise_each(funs(sum), steps)
hist(modifiedStepsPerDay$steps, breaks = 25, xlab = "Total Steps Per Day",
     main = "Histogram of Total Steps per Day (imputing missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
# Note that we were asked to create a histogram, but if we wanted to make a bargraph instead, 
# the following command would do so:
# barplot(modifiedStepsPerDay$steps, names.arg = modifiedStepsPerDay$date)
```
Note that by imputing the missing values, the shape of the distribution does not significantly change, but the frequency of the maximum number of steps per day does significantly increase.

```r
modifiedMeanSteps <- mean(modifiedStepsPerDay$steps)
modifiedMedianSteps <- median(modifiedStepsPerDay$steps)
```


After replacing all NA values with the mean value for the appropriate time interval, the mean number of steps per day = 10766.19 and the median number of steps per day = 10766.19. So we see that the mean and the median values are now equal, which makes sense because we replaced missing values with mean values for each time interval.

## Are there differences in activity patterns between weekdays and weekends?

The next task is to look for a difference in activity pattern between weekdays and weekend days.  We create a new factor variable (dayType) that identifies each day as a weekday or weekend day. We then plot the average number of steps taken for each time interval, averaged across all weekday days and weekend days.


```r
library(lattice)
summaryData <- modifiedStepData
summaryData$dayType <- as.factor(c("weekend","weekday","weekday","weekday","weekday","weekday","weekend")
                                 [as.POSIXlt(summaryData$date)$wday + 1])
activityPattern <- summaryData %>% group_by(dayType, interval) %>% summarise_each(funs(mean), steps)
xyplot(steps ~ interval | dayType, data = activityPattern,
         layout = c(1,2), stack = TRUE, type = "l",
         xlab = "Interval",
         ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

These plots clearly show that there is a difference in activity pattern between weekday days and weekend days.  The average number of steps during each 5-minute time interval is more uniform during the day for weekend days, and the larger number of steps per time period is focused more during the morning (from about 5 AM to 10 AM) for weekday days.  The people also appear to start walking earlier on weekday days than they do on weekend days.
