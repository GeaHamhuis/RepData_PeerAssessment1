Steps taken....
=====

This document leads you through the analysis of the data of a activity tracker. Over two month the tracker collected data of the steps taken by one person. 


```r
library(dplyr)
library(ggplot2)
# read data
activity<-read.csv("activity.csv")
```


**1. What is mean total number of steps taken per day? **

In this section we will look at the total numbers of steps taken per day. The distribution of the total steps is as follows:


```r
#calculate total steps per day
stepsday<-activity%>% group_by(date) %>% summarise(totalsteps=sum(steps))
# create histogram
ggplot(stepsday, aes(totalsteps))+geom_histogram(binwidth=500)+
   labs(title="Histogram of steps per day")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
#calculate mean and median
mean<-as.integer(mean(stepsday$totalsteps, na.rm=TRUE))
median<-median(stepsday$totalsteps, na.rm=TRUE)
```

The histogram shows that the most common number of steps per day is around 10000 steps.
The mean of total number of steps taken per day is 10766 and the median of total number of steps taken per day is 10765.


**2. What is the average daily activity pattern?**

Here we will take a look at activity throughout an average day. The average number of steps taken in each interval is: 


```r
# calculate the mean number of steps per interval, make a time series plot, and identify the interval with the highest average number of steps.
stepsinterval<-activity%>% group_by(interval)%>%summarise(avginterval=mean(steps, na.rm=TRUE))
max<-top_n(stepsinterval,1, avginterval)
plot(stepsinterval$avginterval, type="l", main="timeserie average steps", xlab="interval", ylab="steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

As you can see in the figure there is hardly any activity at night. This person is most active in the morning. The 835th interval contains the maxime average steps across the day.


**3. Imputing missing values**

The origanal data set is missing measurements at 2304 intervals. To complete the data set these missing values will be replaced by the mean steps for that interval across all days.


```r
# impute the missing data with the mean number of steps of that interval 
activityimp<-activity
for (i in 1:nrow(activity)) {
  if (is.na(activity$steps[i])) {
    x<-activity$interval[i]
    row<-stepsinterval[stepsinterval$interval==x,]
    activityimp$steps[i]<-row$avginterval
  }
}
# calculate the average number of step per day on the imputed data, make a histogram and calculate the mean and median
stepsdayimp<-activityimp%>% group_by(date) %>% summarise(totalsteps=sum(steps))
ggplot(stepsdayimp, aes(totalsteps))+geom_histogram(binwidth = 500)+
   labs(title="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
meanimp<-as.integer(mean(stepsdayimp$totalsteps, na.rm=TRUE))
medianimp<-as.integer(median(stepsdayimp$totalsteps, na.rm=TRUE))
```

The mean of total number of steps taken per day is 10766 and the median of total number of steps taken per day is 10766. The mean and median of the imputed dataset are almost identical to the mean and median of the original data. 


**4. Are there differences between weekdays and weekends?**

Here you can see the time series of number of steps taken throughout the day, for the average day in the weekend an the average weekday.


```r
##create a factor 'weekday'
activityimp$weekday<-weekdays(as.Date(activityimp$date))
for (i in 1:nrow(activityimp)) {
    ifelse ((activityimp$weekday[i]=="zaterdag"|activityimp$weekday[i]=="zondag"), activityimp$weekday[i]<-"weekend", activityimp$weekday[i]<-"weekday")
}
activityimp$weekday<-as.factor(activityimp$weekday)

## create a plot

stepsintervalweekday<-activityimp %>% filter(weekday=="weekday") %>% group_by(interval) %>% summarise(avginterval=mean(steps, na.rm=TRUE))
stepsintervalweekend<-activityimp %>% filter(weekday=="weekend") %>% group_by(interval) %>% summarise(avginterval=mean(steps, na.rm=TRUE))
par(mfcol=c(2,1))
par(mar=c(2,2,1,1))
plot(stepsintervalweekend$avginterval, type="l", main="weekend", ylab="steps")
plot(stepsintervalweekday$avginterval, type="l", main="weekday", ylab="steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

On a weekday this person is active earlier in the morning. On the other hand, he or she is more active in the evening at the weekend.

