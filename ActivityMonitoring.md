# Reproducible Research: Peer Assessment 1

### Course - Reproducible Research
#### Week 2 Assignment - R Markdown & Knitr
Suleman Wadur 


Loading data file 

```r
## Turns off exponential notation of numeric values such as when using the mean function
options(scipen = 999)

## Load needed libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
## Set working directory
workdir <- "C:/Move 4/Coursera/DataScience/Course5-ReproducibleResearch/Week2/Assignment/RepData_PeerAssessment1"
setwd(workdir)



if (file.exists("activity.zip") && !file.exists("activity.csv")) {
  unzip(zipfile="./activity.zip", exdir=".")
}

if(!file.exists("activity.csv")) {
  stop("One of the input files is missing!")
}

activitydata <- read.csv("activity.csv")
```
  
  
  
#### Mean total number of steps taken per day

```r
#plot(activitydata$date, mean(is.na(activitydata$steps)))  DailyMean=activitydata$steps
data0 <- subset(activitydata, !is.na(steps))
TotalStepsByDay <- setNames(aggregate(data0$steps, list(data0$date), FUN=sum), c("Day", "Total.Steps"))
#plot(data1$x, data1$Day, type = "h", xlab = "Day", ylab = "Mean daily steps", main = "Average total steps per day")

TotalStepsByDay
```

```
##           Day Total.Steps
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## 11 2012-10-13       12426
## 12 2012-10-14       15098
## 13 2012-10-15       10139
## 14 2012-10-16       15084
## 15 2012-10-17       13452
## 16 2012-10-18       10056
## 17 2012-10-19       11829
## 18 2012-10-20       10395
## 19 2012-10-21        8821
## 20 2012-10-22       13460
## 21 2012-10-23        8918
## 22 2012-10-24        8355
## 23 2012-10-25        2492
## 24 2012-10-26        6778
## 25 2012-10-27       10119
## 26 2012-10-28       11458
## 27 2012-10-29        5018
## 28 2012-10-30        9819
## 29 2012-10-31       15414
## 30 2012-11-02       10600
## 31 2012-11-03       10571
## 32 2012-11-05       10439
## 33 2012-11-06        8334
## 34 2012-11-07       12883
## 35 2012-11-08        3219
## 36 2012-11-11       12608
## 37 2012-11-12       10765
## 38 2012-11-13        7336
## 39 2012-11-15          41
## 40 2012-11-16        5441
## 41 2012-11-17       14339
## 42 2012-11-18       15110
## 43 2012-11-19        8841
## 44 2012-11-20        4472
## 45 2012-11-21       12787
## 46 2012-11-22       20427
## 47 2012-11-23       21194
## 48 2012-11-24       14478
## 49 2012-11-25       11834
## 50 2012-11-26       11162
## 51 2012-11-27       13646
## 52 2012-11-28       10183
## 53 2012-11-29        7047
```
  
Histogram: total number of steps taken per day

```r
hist(TotalStepsByDay$Total.Steps, main = "Histogram: Total Number of Steps per Day", col = "lightblue", xlab = "Total Steps per day" )
```

![](ActivityMonitoring_files/figure-html/HistogramTotalSteps-1.png)<!-- -->

Mean and Median of Steps Taken per day

```r
meanOfSteps <- mean(TotalStepsByDay$Total.Steps)
medianOfSteps <- median(TotalStepsByDay$Total.Steps)
```
Mean is 10766.1886792 and median is 10765  
  
  
#### Time Series: Average Daily Activity Pattern

```r
ActivtyPattern <- aggregate(steps ~ interval, data0, mean)
plot(ActivtyPattern$interval, ActivtyPattern$steps, type = "l", main="Average Daily Activity Pattern", ylab = "Average Daily Steps", xlab = "5-minute Interval")
```

![](ActivityMonitoring_files/figure-html/AverageDailyActivityPattern-1.png)<!-- -->

```r
maxSteps <- ActivtyPattern[which.max(ActivtyPattern$steps),]
```
5-minute interval with the maximum number of steps is 835 with 206.1698113 steps



#### Imputing missing values

```r
# get a vector of complete cases. This will return true or false, with rows having missing values as TRUE
y <- !complete.cases(activitydata)

# counts the number of records with TRUE, indicating they have missing records.
mrecords <- sum(y)
```
There are a total of 2304 records with missing values
  
  
Using the mean of 5-minute interval, a new variable is added for records with missing steps...

```r
#create a new variable with the copy of the steps data. This will be updated in the function below
activitydata$new.steps <- activitydata$steps

#determine average steps by interval
meansByInterval <- aggregate(steps ~ interval, data = activitydata, mean)

# gets index of all rows in the original dataset with missing data
p <- which(is.na(activitydata), arr.ind=TRUE)

for(i in 1:nrow(p)) {
  j <- p[i,1]
  
  #gets index of the mean interval stored in meansByInterval data frame
  indexOfMeanInterval = which(meansByInterval$interval == activitydata[i,3])
  
  #update the new steps with the mean of its interval
  activitydata[j, 4] <- meansByInterval[indexOfMeanInterval,2]
}
```


Calculating summary on updated data...

```r
TotalStepsByDay <- setNames(aggregate(activitydata$new.steps, list(activitydata$date), FUN=sum), c("Day", "Total.Steps"))
hist(TotalStepsByDay$Total.Steps, main = "Histogram: Total Number of Steps per Day: Missing Data corrected", col = "lightblue", xlab = "Total Steps per day" )
```

![](ActivityMonitoring_files/figure-html/HistogramStepsUpdated-1.png)<!-- -->


```r
NewMeanOfSteps <- mean(TotalStepsByDay$Total.steps)
```

```
## Warning in mean.default(TotalStepsByDay$Total.steps): argument is not
## numeric or logical: returning NA
```

```r
NewMedianOfSteps <- median(TotalStepsByDay$Total.steps)
```

```
## Warning in is.na(x): is.na() applied to non-(list or vector) of type 'NULL'
```
Old Mean: 10766.1886792 and Old median: 10765  
New Mean: NA and New median:   

From the histogram above, the frequency has certainly increased. Only the median total daily steps seems to have changed when the missing data got updated.  


#### Activity Pattern: Weekdays vs Weekends

```r
#loop through data and derive the weekend/weekday
for (i in 1:nrow(activitydata)) {
  if (weekdays(as.Date(activitydata$date[i])) == "Saturday" | weekdays(as.Date(activitydata$date[i])) == "Sunday")
      activitydata$weekday[i] <- "weekend"
  else
    activitydata$weekday[i] <- "weekday"
}

TotalStepsByWeekDay <- setNames(aggregate(activitydata$new.steps~activitydata$weekday+activitydata$interval, list(activitydata$weekday), FUN=mean), c("Weekday", "Interval", "Average.Steps"))

#TotalStepsByWeekDay


#defines a 2 row, 1 column panel plot area
par(mfrow = c(2,1) )

#subset data according to weekend or weekday and plot the time series graph
dday <- subset(TotalStepsByWeekDay, Weekday == "weekday")
dend <- subset(TotalStepsByWeekDay, Weekday == "weekend")
plot(dday$Interval, dday$Average.Steps, type = "l", main="Activity Pattern - Weekday", ylab = "Average number of Steps", xlab = "5-minute Interval", col = "lightblue" )

plot(dend$Interval, dend$Average.Steps, type = "l", main="Activity Pattern - weekend", ylab = "Average number of Steps", xlab = "5-minute Interval", col = "lightblue")
```

![](ActivityMonitoring_files/figure-html/WeekDayPattern-1.png)<!-- -->

