Project1\_course5
================
aomelanuk
2018-08-30

-   [1. Synopsis](#synopsis)
-   [2. Checking environment](#checking-environment)
-   [3.1 Loading data](#loading-data)
-   [3.2 Histogram of the total number of steps taken each day](#histogram-of-the-total-number-of-steps-taken-each-day)
-   [![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)](#section)
-   [![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)](#section-1)
-   [![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)](#section-2)

Github repo for the Course: [Reproducible Research](https://github.com/aomelanuk/course_5_project1)

------------------------------------------------------------------------

### 1. Synopsis

The goal of this work collect a large amount of data about personal movement using activity monitoring and handle it for analisys

------------------------------------------------------------------------

### 2. Checking environment

loading needable librariesfor data processing in this work I will use next libraries: dplyr, ggplot2

``` r
library("ggplot2")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

------------------------------------------------------------------------

### 3.1 Loading data

Checking exist data on the PC, if not exist dovnload it from source (<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>)

``` r
if(!(file.exists("activity.csv")))
{
           if(!(file.exists("repdata_data_activity.zip")))  
           {
                      url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
                      download.file(url,"repdata_data_activity.zip")
           }
           unzip("repdata_data_activity.zip",overwrite = TRUE)
}
my_data<-tbl_df(read.csv("activity.csv",na.strings = "NA",colClasses = "character"))
```

for loading I use function read.csv with parameters for data handling I use variable my\_data As result I have data for next analisys, but all variables are chracter and next step I transform variable to useful types

``` r
my_data$date<-ymd(my_data$date)
my_data$interval<-as.integer(my_data$interval)
my_data$steps<-as.integer(my_data$steps)
```

------------------------------------------------------------------------

### 3.2 Histogram of the total number of steps taken each day

For results mean, average I create new data frame with this results by day I plot histogram using new table my\_data\_h with mean, median, and sum

``` r
my_data_h<-my_data%>%select(date,steps)%>%group_by(date)%>%summarise_all(c(funs(mean),funs(sum)))
head(my_data_h)
```

    ## # A tibble: 6 x 3
    ##   date         mean   sum
    ##   <date>      <dbl> <int>
    ## 1 2012-10-01 NA        NA
    ## 2 2012-10-02  0.438   126
    ## 3 2012-10-03 39.4   11352
    ## 4 2012-10-04 42.1   12116
    ## 5 2012-10-05 46.2   13294
    ## 6 2012-10-06 53.5   15420

``` r
hist(my_data_h$sum,breaks = 26,col = "red", main = "Histogram number of steps", xlab = "steps quantity")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)
--------------------------------------------------------------------

### 3.3 Mean and median number of steps taken each day

for calculate this data I use data frame my\_data\_h

``` r
median_steps=median(my_data_h$sum,na.rm = TRUE)
print(paste("median steps quantity", median_steps))
```

    ## [1] "median steps quantity 10765"

``` r
mean_steps=mean(my_data_h$sum,na.rm = TRUE)
print(paste("Mean steps quantity", mean_steps))
```

    ## [1] "Mean steps quantity 10766.1886792453"

------------------------------------------------------------------------

### 3.4 Time series plot of the average number of steps taken

``` r
mean_interval<-my_data%>%
      filter(!is.na(steps))%>%
      select(interval,steps)%>%
      group_by(interval)%>%
      summarise_all(funs(mean)) 
plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l",main = "Time series plot of the average number of steps taken",
     xlab = "interval", ylab = "average steps", col="red")                          
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)
--------------------------------------------------------------------

### 3.5 The 5-minute interval that, on average, contains the maximum number of steps

``` r
top_int<-my_data%>%filter(!is.na(steps))%>%select(interval,steps)%>%
      group_by(interval)%>%summarise_all(funs(mean))%>%
      arrange(desc(steps))%>%head(1)
print(top_int)
```

    ## # A tibble: 1 x 2
    ##   interval steps
    ##      <int> <dbl>
    ## 1      835  206.

------------------------------------------------------------------------

### 3.6 Code to describe and show a strategy for imputing missing data

in first step I lookin rows with NA data

``` r
print(paste("Quantity NA rows",my_data%>%filter(is.na(steps))%>%nrow))
```

    ## [1] "Quantity NA rows 2304"

days when steps data NA

``` r
na_date<-my_data%>%filter(is.na(steps))%>%select(date)%>%unique()
print("date with NA data")
```

    ## [1] "date with NA data"

``` r
print(as.character(na_date$date))
```

    ## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
    ## [6] "2012-11-10" "2012-11-14" "2012-11-30"

for inserting data in rows with NA I use data frame mean\_interval (was created before)

``` r
my_data[is.na(my_data$steps),"steps"]<-mean_interval$steps
```

------------------------------------------------------------------------

### 3.7 Histogram of the total number of steps taken each day after missing values are imputed

``` r
my_data_h<-my_data%>%select(date,steps)%>%
      group_by(date)%>%summarise_all(c(funs(mean),funs(sum)))
head(my_data_h)
```

    ## # A tibble: 6 x 3
    ##   date         mean    sum
    ##   <date>      <dbl>  <dbl>
    ## 1 2012-10-01 37.4   10766.
    ## 2 2012-10-02  0.438   126 
    ## 3 2012-10-03 39.4   11352 
    ## 4 2012-10-04 42.1   12116 
    ## 5 2012-10-05 46.2   13294 
    ## 6 2012-10-06 53.5   15420

``` r
hist(my_data_h$sum,breaks = 26,col = "red", 
     main = "Histogram number of steps,without NA", xlab = "steps quantity")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)
---------------------------------------------------------------------

### 3.8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

my\_data&lt;-my\_data%&gt;%mutate(weekday = wday(date))

``` r
my_data<-my_data%>%mutate(weekday = wday(date))
mean_interval_w<-my_data%>%
      filter(weekday==c(1,7))%>%
      select(interval,steps)%>%
      group_by(interval)%>%
      summarise_all(funs(mean))
mean_interval<-my_data%>%
      filter(weekday==c(2:6))%>%
      select(interval,steps)%>%
      group_by(interval)%>%
      summarise_all(funs(mean))
```

    ## Warning in weekday == c(2:6): longer object length is not a multiple of
    ## shorter object length

``` r
par(mfrow=c(2,1))   

plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l",main = "Time series plot of the average number of steps taken on weekdays", xlab = "interval", ylab = "average steps", col="green")    
plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l",
     main = "Time series plot of the average number of steps taken on other days",
     xlab = "interval", ylab = "average steps", col="blue") 
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)
