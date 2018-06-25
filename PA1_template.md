**Overview**
============

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

**Report**
==========

Loading and Preprocessing Data
------------------------------

In this section shows how to preparing data for further analysis.

### 1. Load the data

    df <- read.csv("activity.csv", header =  TRUE)
    head(df)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    str(df) # checking data format

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### 2. Process/transform the data into suitable format

Raw data shows that `date` variable is not in datetime format, so it
should be converted using `as.Date` function. This data also have
missing values that coded in `NA`. For further computation, this missing
value should be exluded and using only complete observation data.

    df$date <- as.Date(df$date, format = "%Y-%m-%d") # convert to datetime variable
    colSums(is.na(df)) # checking for number of missing value

    ##    steps     date interval 
    ##     2304        0        0

    df.complete <- df[complete.cases(df),]
    colSums(is.na(df.complete)) # make sure there is no missing value left

    ##    steps     date interval 
    ##        0        0        0

Mean Total Number of Steps Taken per Day
----------------------------------------

### 1. Calculate the total number of steps per day

    total.steps <- tapply(df.complete$steps, df.complete$date, sum)
    head(total.steps)

    ## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
    ##        126      11352      12116      13294      15420      11015

### 2. Make a histogram ot the total number of steps taken each day

    hist(total.steps, 
         main = "Histogram of Total Number of Steps", 
         xlab = "Total number of steps each day")

<img src="Peer_Assignment_1_Reproducible_files/figure-markdown_strict/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

At glance, the histogram shows that distribution of number of steps is
approaching normal distribution. Large portion of data concentrated in
middle and the rest distributed relatively even around center.

### 3. Calculate and report the mean and median of the total number of steps taken per day

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10765

Mean and median of the total number of steps taken per day is almost
identical, this backed previous suggestion that the data is normally
distributed.

Average Daily Activity Pattern
------------------------------

### 1. Make time series plot of 5-minutes interval of average number of steps taken

    total.step.interval <- tapply(df.complete$steps, df.complete$interval, mean)
    library(lattice)
    xyplot(total.step.interval ~ unique(df.complete$interval), type = "l", 
         main = "Time series plot of 5-minutes interval steps",
         xlab = "5-minutes interval", ylab = "Average steps")

<img src="Peer_Assignment_1_Reproducible_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

### 2. Maximum number of steps in 5-minutes interval

    subset(total.step.interval, total.step.interval == max(total.step.interval))

    ##      835 
    ## 206.1698

835 is when the maximum average number of steps in 5-minutes interval

Imputing Missing Value
----------------------

### 1. Calculate and report the total number of missing values in the dataset

    colSums(is.na(df))

    ##    steps     date interval 
    ##     2304        0        0

There is 2304 missing value in the dataset.

### 2. Devise a strategy for filling in all of the missing values in the dataset and create new dataset with the missing data filled in

Since the missing value contain several dates, so the imputation will
using mean of steps taken per 5-minutes interval.

    mean.steps.interval <- tapply(df.complete$steps, df.complete$interval, mean)
    # strategy to impute data based on its mean per 5-minutes interval
    dfcopy <- df
    for (i in unique(dfcopy$interval)) {
      dfcopy[is.na(dfcopy$steps) & dfcopy$interval == i, 1] <- 
        mean(dfcopy[dfcopy$interval == i, 1], na.rm = T) 
    }
    colSums(is.na(dfcopy))

    ##    steps     date interval 
    ##        0        0        0

    identical(dim(dfcopy), dim(df)) # check whether there is excluded value or not

    ## [1] TRUE

### 3. Make a histogram of the total number of steps taken each day

    total.step.complete <- tapply(dfcopy$steps, dfcopy$date, sum)
    hist(total.step.complete, 
         main = "Histogram of Total Number of Steps Taken After Imputation",
         xlab = "Total number of steps each day")

<img src="Peer_Assignment_1_Reproducible_files/figure-markdown_strict/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### 4. Calculate and report the mean and median total number of steps taken per day

    mean(total.step.complete)

    ## [1] 10766.19

    median(total.step.complete)

    ## [1] 10766.19

The mean of number of steps taken per day after imputation is similar to
before imputation. On the other hand, median has slightly different
value compared to medain before imputation. After imputation, the median
and mean of steps taken per day is equal.

Pattern in weekdays and weekend
-------------------------------

### 1. Create a new factor variable in the dataset with two levels

    weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri")
    dfcopy$weekday <- factor((weekdays(dfcopy$date, abbreviate = T) %in% weekdays), 
                         levels = c(FALSE, TRUE), 
                         labels = c("weekend", "weekdays"))

### 2. Make a panel plot containing a time series plot

    library(dplyr)
    average.interval.weekday <- dfcopy %>%
      select(steps, interval, weekday) %>%
      group_by(interval, weekday) %>%
      summarise(avg = mean(steps))
    attach(average.interval.weekday)
    xyplot(avg ~ interval | weekday, type = "l", 
           layout = c(1,2), 
           xlab = "5-minute interval", ylab = "Average steps taken",
           main = "Average Number of Steps Between Weekday and Weekend")

<img src="Peer_Assignment_1_Reproducible_files/figure-markdown_strict/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

    detach(average.interval.weekday)
