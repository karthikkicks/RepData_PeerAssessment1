
Reproducible Research: Peer Assessment 1
========================================

##Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
data$date<-as.Date(data$date,'%Y-%m-%d')
```

##What is mean total number of steps taken per day?


```r
aggDate <- aggregate(steps~date, data, sum)
hist(aggDate$steps, main='Histogram of Total Steps on each day', xlab='Steps per day')
```

![plot of chunk Mean steps](figure/Mean steps-1.png) 

```r
Mean<-round(mean(aggDate$steps), digits=3)
Median<-median(aggDate$steps)
```

Mean total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>  
Median of total number of steps taken per day is 10765

##What is the average daily activity pattern?


```r
aggInterval <- aggregate(steps~interval, data, mean)
plot(aggInterval, type='l', xlab='5 Minute Interval')
```

![plot of chunk daily activity pattern](figure/daily activity pattern-1.png) 

```r
MaxInterval <- aggInterval$interval[aggInterval$steps==max(aggInterval$steps)]
```
5 minute interval that has the maximum number of steps is 835

##Imputing missing values


```r
NumberofNAs <- sum(is.na(data$steps))
ModifiedData <- data
for(i in 1:nrow(ModifiedData)){
    if(is.na(ModifiedData[i,1])){
        ModifiedData[i,1]<-aggInterval$steps[aggInterval$interval==ModifiedData[i,3]]
    }
}

aggDate_M <- aggregate(steps~date, ModifiedData, sum)
hist(aggDate_M$steps, main='Histogram of Total Steps on each day without NA')
```

![plot of chunk Imputing missing values](figure/Imputing missing values-1.png) 

```r
Mean_M<-mean(aggDate_M$steps)
Median_M<-median(aggDate_M$steps)
```
Number of missing values is 2304  
Mean of number of steps taken per day after inserting missing values is 1.0766189 &times; 10<sup>4</sup>  
Median of number of steps taken per day after inserting missing values is 1.0766189 &times; 10<sup>4</sup>  

##Are there differences in activity patterns between weekdays and weekends?


```r
library(chron)
ModifiedData[,'Day']<-vector('character')
for(i in 1:nrow(ModifiedData)){
    if(is.weekend(ModifiedData[i,2])){
        ModifiedData[i,4]<-'weekend'
    }else ModifiedData[i,4] <- 'weekday'
}
ModifiedData <- aggregate(steps~interval+Day, ModifiedData, mean)
library(lattice)
xyplot(steps~interval | factor(Day), data=ModifiedData, layout=c(1,2), xlab='interval',ylab='Number of Steps', type='l')
```

![plot of chunk weekday weekend patterns](figure/weekday weekend patterns-1.png) 

































