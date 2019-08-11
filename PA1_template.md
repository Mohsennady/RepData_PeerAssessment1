
#Activity Dataset


```r
library(tidyverse)

library(lubridate)


download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "./activity.zip")

unzip("./activity.zip",exdir = "./activity")

list.files("./activity")
```

```
## [1] "activity.csv"
```

```r
activity_data<-read.csv("./activity/activity.csv")
```

##Exploring the dataset


```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#Converting the date variable from factor variable to date variable

activity_data$date<-ymd(as.character(activity_data$date))

head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##What is mean total number of steps taken per day?


```r
#Make a histogram of the total number of steps taken each day

activity_data %>% group_by(date) %>% summarise(total = sum(as.numeric(steps),na.rm = TRUE)) %>% ggplot(aes(x=total))+geom_histogram(bins = 20) + ggtitle("Total number of steps taken each day") + xlab("Total steps") + ylab("Count of total steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
#Calculate and report the mean and median total number of steps taken per day

activity_data %>% group_by(date) %>% summarise(total = sum(as.numeric(steps),na.rm = TRUE)) %>% summarise(mean = mean(total), median = median(total))
```

```
## # A tibble: 1 x 2
##    mean median
##   <dbl>  <dbl>
## 1 9354.  10395
```

##What is the average daily activity pattern?


```r
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activity_data %>% group_by(interval) %>% summarise(mean=mean(steps, na.rm = TRUE)) %>% ggplot(aes(x=interval, y= mean))+geom_line()+ggtitle("Average daily activity pattern")+xlab("interval")+ylab("average steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

activity_data %>% group_by(interval) %>% summarise(mean=mean(steps, na.rm = TRUE)) %>% arrange(desc(mean)) %>% head(1)
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```

##Imputing missing values


```r
#report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# From the summary() output, only the steps column has NA values

activity_data %>% filter(is.na(steps))%>% nrow()
```

```
## [1] 2304
```

```r
#Fill the missing values based in the average of that interval

activity_data %>% group_by(interval) %>% mutate(mean=mean(steps, na.rm =
                                                                  TRUE),new_steps=ifelse(is.na(steps), mean, steps)) %>% ungroup() %>% slice(300:10)
```

```
## # A tibble: 291 x 5
##    steps date       interval   mean new_steps
##    <int> <date>        <int>  <dbl>     <dbl>
##  1     0 2012-10-02       55 0.132          0
##  2     0 2012-10-02       50 0.302          0
##  3     0 2012-10-02       45 1.47           0
##  4     0 2012-10-02       40 0              0
##  5     0 2012-10-02       35 0.868          0
##  6     0 2012-10-02       30 0.528          0
##  7     0 2012-10-02       25 2.09           0
##  8     0 2012-10-02       20 0.0755         0
##  9     0 2012-10-02       15 0.151          0
## 10     0 2012-10-02       10 0.132          0
## # ... with 281 more rows
```

```r
# Create a new dataset with the NA values filled

New_data <- activity_data %>% group_by(interval) %>% mutate(mean=mean(steps, na.rm = TRUE),new_steps=ifelse(is.na(steps), mean, steps)) %>% ungroup %>% select(new_steps, date, interval)

str(New_data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ new_steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date     : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#Create a histogram of the total steps taken each day

New_data %>% group_by(date) %>% summarise(total = sum(as.numeric(new_steps),na.rm = TRUE)) %>% ggplot(aes(x=total))+geom_histogram(bins = 20) + ggtitle("Total number of steps taken each day after filling NA values") + xlab("Total steps") + ylab("Count of total steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
#Calculate the mean and median of total steps taken daily for the new dataset

New_data %>% group_by(date) %>% summarise(total = sum(as.numeric(new_steps),na.rm = TRUE)) %>% summarise(mean=mean(total), median=median(total))
```

```
## # A tibble: 1 x 2
##     mean median
##    <dbl>  <dbl>
## 1 10766. 10766.
```

```r
#The data becomes more normally distributed as shown in the histogram and the equality of the mean and median
```


##Differences in activity patterns between weekdays and weekends


```r
#Creating a new coulmn with the weekday

New_data$weekday<-wday(New_data$date,label = TRUE)

head(New_data)
```

```
## # A tibble: 6 x 4
##   new_steps date       interval weekday
##       <dbl> <date>        <int> <ord>  
## 1    1.72   2012-10-01        0 Mon    
## 2    0.340  2012-10-01        5 Mon    
## 3    0.132  2012-10-01       10 Mon    
## 4    0.151  2012-10-01       15 Mon    
## 5    0.0755 2012-10-01       20 Mon    
## 6    2.09   2012-10-01       25 Mon
```

```r
levels(New_data$weekday)
```

```
## [1] "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"
```

```r
#Creating another column to indicate if the weekday is weekend or not

New_data<-mutate(New_data,wday_end=ifelse(weekday=="Sun","Weekend","Weekday"))

head(New_data)
```

```
## # A tibble: 6 x 5
##   new_steps date       interval weekday wday_end
##       <dbl> <date>        <int> <ord>   <chr>   
## 1    1.72   2012-10-01        0 Mon     Weekday 
## 2    0.340  2012-10-01        5 Mon     Weekday 
## 3    0.132  2012-10-01       10 Mon     Weekday 
## 4    0.151  2012-10-01       15 Mon     Weekday 
## 5    0.0755 2012-10-01       20 Mon     Weekday 
## 6    2.09   2012-10-01       25 Mon     Weekday
```

```r
#Plotting the panel plot required

New_data %>% group_by(interval,wday_end) %>% summarise(mean=mean(new_steps)) %>% ggplot(aes(x=interval, y= mean))+geom_line()+ggtitle("Average daily activity pattern")+xlab("interval")+ylab("average steps")+facet_grid(wday_end~.)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
#Viewing the first rows of the dataset

New_data %>% group_by(interval,wday_end) %>% summarise(mean=mean(new_steps))
```

```
## # A tibble: 576 x 3
## # Groups:   interval [288]
##    interval wday_end    mean
##       <int> <chr>      <dbl>
##  1        0 Weekday  1.94   
##  2        0 Weekend  0.215  
##  3        5 Weekday  0.384  
##  4        5 Weekend  0.0425 
##  5       10 Weekday  0.150  
##  6       10 Weekend  0.0165 
##  7       15 Weekday  0.171  
##  8       15 Weekend  0.0189 
##  9       20 Weekday  0.0854 
## 10       20 Weekend  0.00943
## # ... with 566 more rows
```

```r
#So some intervals have greater average steps in weedays than weekends and vice versa.
```

