Reproducible Research_Peer Assignment 1
================
ITguy666
2026-03-16

For this assignment, we will first download the data from a personal
activity monitoring device that collects data at 5 minute intervals
throughout the day. The data consists of two months of data from an
anonymous individual collected during months of October and November
2012 and include the number of steps taken in 5 minute intervals each
day.

The variables included in this dataset are:

1)  steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)

2)  date: The date on which the measurement was taken in YYYY-MM-DD
    format

3)  interval: Identifier for the 5-minute interval in which measurement
    was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

``` r
knitr::opts_chunk$set(echo = TRUE,warning = FALSE, message = FALSE)
##Loading libraries and preprocessing the activity data
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
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
library(readxl)

#Define file name and url
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp_zip <- tempfile(fileext=".zip")

#Download the file and unzip file
download.file(url,temp_zip,mode="wb")
file_list <- unzip(temp_zip,list=TRUE)
print(file_list) # To see file name
```

    ##           Name Length                Date
    ## 1 activity.csv 350829 2014-02-11 10:08:00

``` r
activitydata <- read.csv(unz(temp_zip,"activity.csv"))
unlink(temp_zip)
nrow(activitydata)
```

    ## [1] 17568

``` r
names(activitydata)
```

    ## [1] "steps"    "date"     "interval"

After downloading the data, we will next look at the first question in
what is the mean total number of steps taken per day with the following
analyses: 1. Calculate total number of steps taken per day 2. Make a
histogram of the total number of steps taken each day 3. Calculate the
mean and median of total number of steps taken per day

Solution: To tackle the above, we will first need to convert the date
into a format that can be analysed using as.Date function. Following
which, we will need to group the number of steps per day and sum up the
data for each day. This can be done in creating another
“totalsteps_perday” dataset using group_by date function and then pipe
it to summarise using summarize function to sum up the steps taken in
another variable within the dataset called “total_steps”. Latter will
denote the total steps in a day and each row in the dataset is one day.
We will also use the na.rm = TRUE to ignore missing values. Next, use
the function hist() on totalsteps_perday\$total_steps to create the
histogram.

``` r
#Convert Date format
activitydata$date <- as.Date(activitydata$date,format="%Y-%m-%d")

#Calculate the total steps per day by using group_by date and then use summarise to sum up the total steps for each day
totalsteps_perday <- activitydata %>% 
                        group_by(date) %>% summarize(total_steps = sum(steps,na.rm=TRUE))

#Plot Histogram
hist(totalsteps_perday$total_steps,main="Total Steps Taken Each Day",
        xlab="Steps",
        col="steelblue",
        breaks=20, na.rm=TRUE)
```

![](PA1_template_files/figure-gfm/question%201-1.png)<!-- -->

``` r
# Compute Mean and Median
mean_stepsperday <- mean(totalsteps_perday$total_steps,na.rm=TRUE)
median_stepsperday <- median(totalsteps_perday$total_steps,na.rm=TRUE)
paste("The mean number of steps per day is", round(mean_stepsperday,0))
```

    ## [1] "The mean number of steps per day is 9354"

``` r
paste("The median number of steps per day is", median_stepsperday)
```

    ## [1] "The median number of steps per day is 10395"

Next, we will study the data to derive the answers to the below: 1) What
is the average daily activity pattern? 2) Make a time series plot of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all days (y-axis) 3) Which 5-minute interval, on average
across all the days in the dataset, contains the maximum number of
steps?

Solution: To solve the above, we will also use the group_by and
summarize functions to create a dataset “Average_daily_pattern” that
groups by interval and then take the mean of the steps within the
interval to create a new variable “avg_steps” for this. Similarly,
remove NA values by setting na.rm = TRUE.Then plot interval and
avg_steps in the “Average_daily_pattern” dataset.To calculate the
maximum steps, we use the filter function to detect the datapoint
whereby avg_steps is the max(avg_steps) and then come back with the
corresponding interval which is 835.

``` r
# Average steps per 5-minute interval
Average_daily_pattern <- activitydata %>%
                                group_by(interval) %>%
                                summarize(avg_steps = mean(steps,na.rm=TRUE))
#Time series plot
plot(Average_daily_pattern$interval,
     Average_daily_pattern$avg_steps,
        type = "l", col="red",iwd = 2, main = "Average Daily Activity Pattern By 5-minute Intervals",
     xlab = "5-minute Interval",
     ylab = "Average Steps")
```

![](PA1_template_files/figure-gfm/question%202-1.png)<!-- -->

``` r
# Find 5-minute interval with maximum number of steps
max_interval <- Average_daily_pattern %>% filter(avg_steps==max(avg_steps))
paste("The maximum number of steps is",round(max_interval$avg_steps,0),"and the maximum interval is",max_interval$interval)
```

    ## [1] "The maximum number of steps is 206 and the maximum interval is 835"

Imputing missing values Note that there are a number of days/intervals
where there are missing values (coded as NA). The presence of missing
days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

2.  Devise a strategy for filling in all of the missing values in the
    dataset. For example, you could use the mean/median for that day, or
    the mean for that 5-minute interval, etc.

3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

Solution: To resolve the above, we use sum(is.na) to count the total
NAs. Following which, we will create a new variable “activity_imputed”
that imputes the missing values by first joining the raw activity data
with average daily pattern and then use mutate to create a new variable
using ifelse that takes the value of “avg_steps” if the value is NA or
missing. Following which, we remove the avg_step field from the dataset
to keep it clean. With this dataset, we recompute the daily steps using
group by and summarise functions. And then plot the histogram, mean and
median.

``` r
# Count total NAs
total_na <- sum(is.na(activitydata$steps))
paste("The number of missing values are ",total_na)
```

    ## [1] "The number of missing values are  2304"

``` r
#Create a new dataset and fill NAs as average of steps 
activity_imputed <- activitydata %>%
                        left_join(Average_daily_pattern, by="interval") %>%
                        mutate(steps = ifelse(is.na(steps),avg_steps,steps)) %>%
                        select (-avg_steps)

#Recompute total steps per day with imputed data
imputed_steps_per_day <- activity_imputed %>% 
                        group_by(date) %>%
                        summarize(total_steps = sum(steps,na.rm=TRUE))

#Histogram for imputed data
hist(imputed_steps_per_day$total_steps,
     main="Total Steps Per Day Using Imputed Data for NAs",
     xlab = "Steps",
     col = "lightgreen",
     breaks = 20)
```

![](PA1_template_files/figure-gfm/question%203-1.png)<!-- -->

``` r
# Compute Mean and Median of imputed data
mean_imputed_stepsperday <- 
        mean(imputed_steps_per_day$total_steps,na.rm=TRUE)
median_imputed_stepsperday <- 
        median(imputed_steps_per_day$total_steps,na.rm=TRUE)
paste("The mean imputed steps per day is", round(mean_imputed_stepsperday,0))
```

    ## [1] "The mean imputed steps per day is 10766"

``` r
paste("The mean imputed steps per day is", round(median_imputed_stepsperday,0))
```

    ## [1] "The mean imputed steps per day is 10766"

Are there differences in activity patterns between weekdays and
weekends? For this part the weekdays() function may be of some help
here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

Solution: To differentiate weekdays vs weekends, we use weekdays() and
as.factor() functions in a new variable “day_type”. This “day_type”
variable will be combined with “interval” variable to determine the
breakdowns in the plotting of trend lines using ggplot.

``` r
#Create factor for day type
Activity_imputed <- activity_imputed %>%
                        mutate(day_type = ifelse(weekdays(date) %in% c("Saturday","Sunday"),
                                                     "weekend","weekday"),
                                                day_type = as.factor(day_type))
#Calculate average steps by interval and day_type
panel_data <- Activity_imputed %>% 
        group_by(interval,day_type) %>%
        summarize(avg_steps = mean(steps))

#Panel plot using ggplot2
ggplot(panel_data,aes(x=interval,y=avg_steps,color=day_type))+
        geom_line() +
        facet_wrap(~day_type,ncol=1)+
        labs(title="Comparison of Activity Patterns Between Weekday and Weekend",        
                x="Interval",y="Average Number of Steps")+
                theme_bw()
```

![](PA1_template_files/figure-gfm/question%204-1.png)<!-- -->
