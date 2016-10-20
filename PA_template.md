# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
#Code for reading in the dataset and/or processing the data
unzip("activity.zip")
data <- read.csv( "activity.csv",stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

```r
#Histogram of the total number of steps taken each day
library(dplyr)
data <- group_by(data, date)
summary <- summarise(data, total_steps = sum(steps))
summary1 <- filter(summary, !is.na(total_steps))
library(ggplot2)
ggplot(summary1, aes(x = total_steps)) +
    geom_histogram() +
  labs(title = "Histogram of the total number of steps taken each day")
```

![](PA_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
#Mean and median number of steps taken each day 
summary2 <- summarise(summary1, mean = mean(total_steps), median =median(total_steps))
```

The mean number of steps taken each day is 10766, and the median number of steps taken each day is 10765.


    

## What is the average daily activity pattern?

```r
#Time series plot of the average number of steps taken
data <- group_by(data, interval)
summary3 <- summarise(data, average_steps = mean(steps, na.rm = TRUE))
ggplot(summary3, aes(x = interval, y = average_steps))+
    geom_line()
```

![](PA_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
#The 5-minute interval that, on average, contains the maximum number of steps
max_inerval <- summary3[which(summary3$average_steps == max(summary3$average_steps)), 1]
```

The 835 interval,on average, contains the maximum number of steps.

## Imputing missing value


```r
#Code to describe and show a strategy for imputing missing data
total_nas <- sum(is.na(data$steps))
merge <- merge(data, summary3, by = "interval")
for ( i in 1:length(merge$steps)) {
    if (is.na(merge$steps[i])) {
        merge$steps[i] <- merge$average_steps[i]
    }
     
}

data1 <- select(merge, -average_steps)
```

The total number of missing values in the dataset is 2304

Use the mean for that 5_minute interval to fill in the missing values.



```r
#Histogram of the total number of steps taken each day after missing values are imputed
data1 <- group_by(data1, date)
summary4 <- summarise(data1, total_steps = sum(steps))

ggplot(summary4, aes(x = total_steps)) +
    geom_histogram() +
    labs(title = "Histogram of the total number of steps taken each day(Imputed)")
```

![](PA_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
summary5 <- summarise(summary4, mean = mean(total_steps), median =median(total_steps))
```

     
After the missing value is imputed, the mean number of steps taken each day is 10766, and the median number of steps taken each day is 10766.


## Are there differences in activity patterns between weekdays and weekends?


```r
#Panel plot comparing the average number of steps taken per 5-minute interval #across weekdays and weekends
data1$date <- as.Date(data1$date)
data1$weekdays <- weekdays(data1$date)
weekid <- c()
for (i in 1:length(data1$weekdays)){
    if(data1$weekdays[i] == "星期日" | data1$weekdays[i] == "星期六"){
        weekid<- c(weekid,"Weekend")
    } else{
        weekid <- c(weekid, "Weekday")
    }    
}
data1$id <- weekid
data1$id <- as.factor(data1$id)

data1 <- group_by(data1, id, interval)
summary6 <- summarise(data1, average_steps = mean(steps))

ggplot(summary6, aes(x = interval, y = average_steps)) +
    geom_line() +
    facet_grid(id~.)
```

![](PA_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
