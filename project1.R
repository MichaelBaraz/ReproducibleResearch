#The variables included in this dataset are:
  # steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
  # date: The date on which the measurement was taken in YYYY-MM-DD format
  # interval: Identifier for the 5-minute interval in which measurement was taken


setwd("C:/Users/Michael.Baraz/Documents/DataScience/ReproducibleResearch")

### 1. Code for reading in the dataset and/or processing the data

## if the file does not exist then download archive file 
archiveFile <- "repdata-data-activity.zip"
if(!file.exists(archiveFile)) {
  archiveURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  if(Sys.info()["sysname"] == "Windows") {
    download.file(url=archiveURL,destfile=archiveFile,method="curl")
  } else {
    download.file(url=url,destfile=archiveFile)
  }
}
if(!(file.exists("activity.txt"))) { unzip(archiveFile) }

## Read data files into data frames
#  Note: stringsAsFactors is set to FALSE so that dates are not factors but strings 
#  that will be converted to dates later
activity <- read.csv("activity.csv", stringsAsFactors = F)

# After we read in the data file, confirm contents of the data frame.
names(activity)

str(activity)

head(activity, 10)



### 2. Histogram of the total number of steps taken each day

## create a dataset with NA values removed. To consider only those days for which
#  step counts were recorded, use the complete.cases method
activity_complete_days <- activity[complete.cases(activity), ]

## Perform the analysis required for this part of the assignment using the group_by 
#  and summarize functions from dplyr. The sum can be calculated using summarise once
#  the data has been organized by date using group_by.

## Use group_by and summarize functions from dplyr
library("dplyr")

step_summary  <-  activity_complete_days %>% 
  group_by(date) %>% 
  summarize(daily_step_count = sum(steps))

## verify new data frame
head(step_summary, 5)

## Plot the graph
hist(step_summary$daily_step_count, 
     main = "Histogram of total steps per day",
     xlab = "Total steps Daily",
     col = "blue",
     breaks = 20
     )




### 3. Mean and median number of steps taken each day
## mean
mean(step_summary$daily_step_count)

## median
median(step_summary$daily_step_count)


### 4. Time series plot of the average number of steps taken


## Within each recorded interval across all days in each of the two months, what was the average 
#  number of steps taken in each interval? To answer this question, group_by each interval and 
#  then take the mean of the steps in each interval.
interval  <- activity_complete_days %>% 
  group_by(interval) %>% 
  summarize(mean_interval = mean(steps))


## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average
#  number of steps taken, averaged across all days (y-axis)

## Plot the graph
#png(filename = "average_steps_per_day.png", width = 600, height = 600, units = "px")
plot(interval$interval, 
     interval$mean_interval, 
     type = "l", 
     las = 1, 
     col = "blue", 
     main = "Time Series Plot of the 5-Minute Interval\n and the Average Number of Steps Taken, Averaged Across All Days",
     col.main = "blue",
     font.main = 4,
     xlab = "Five Minute Intervals",
     ylab = "Average Number of Steps take\n Averaged Across All Days"
)

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval[which.max(interval$mean_interval), ]




### Imputing missing values

## 1) Calculate and report the total number of missing values in the dataset (i.e. the 
#  total number of rows with NAs)

nrow(activity[is.na(activity$steps),])

## 2) Devise a strategy for filling in all of the missing values in the dataset. 
#  The strategy does not need to be sophisticated. For example, you could use the mean/median 
#  for that day, or the mean for that 5-minute interval, etc.

## solution: Use the mean for the 5-minute interval to populate NA values for a given interval.


## 3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

## merge original activity data frame with interval data frame
newactivity <- merge(activity, interval, by = 'interval', all.y = F)

## merge NA values with averages rounding up for integers
newactivity$steps[is.na(newactivity$steps)] <- as.integer(
  round(newactivity$mean_interval[is.na(newactivity$steps)]))

## drop and reorder columns to match original activity data frame
## http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame
keeps <- names(activity)
newactivity <- newactivity[keeps]


## 4) Make a histogram of the total number of steps taken each day and Calculate
#  and report the mean and median total number of steps taken per day.

## (total number of (steps taken per day))
newtotal <- aggregate(steps ~ date, newactivity, sum)

## add descriptive variable names
names(newtotal)[2] <- "sum_steps"

## check out new data frame
head(newtotal, 5)


## Plot histogram of the total number of steps taken each day and Calculate and 
#  report the mean and median total number of steps taken per day.
hist(
  newtotal$sum_steps,
  col = "blue",
  main = "Histogram of the Total Number of Steps Taken Each Day",
  xlab = "Total Number of Steps Taken Each Day",
  breaks = 20
)

## mean
mean(newtotal$sum_steps)

## median
median(newtotal$sum_steps)

## Do these values differ from the estimates from the first part of the assignment?

# Yes, but only slightly:

  #mean(total) = 10766.19, while mean(newtotal) = 10765.64. Rounding produces the same value.
  #median(total) = 10765, while median(newtotal) = 10762. 3 step difference.

## What is the impact of imputing missing data on the estimates of the total daily number of steps?

# This depends on how the missing data is imputed. Used the average for a given interval, there 
# was practically no difference because it pulled the averages closer to the inserted average value.




### Are there differences in activity patterns between weekdays and weekends?

## 1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#  indicating whether a given date is a weekday or weekend day.

## create new data frame
newnewactivity <- newactivity

## set up logical/test vector
weekend <- weekdays(as.Date(newnewactivity$date)) %in% c("Saturday", "Sunday")

## Fill in weekday column
newnewactivity$daytype <- "weekday"

## Replace "weekday" with "weekend" where day == Sat/Sun
newnewactivity$daytype[weekend == TRUE] <- "weekend"

## Convert new character column to factor
newnewactivity$daytype <- as.factor(newnewactivity$daytype)

## Confirm new data frame
str(newnewactivity)

head(newnewactivity, 5)

## double check
weekdays(as.Date(newnewactivity$date[3]))


## 2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#  interval (x-axis) and the average number of steps taken, averaged across all weekday 
#  days or weekend days (y-axis).

## the average number of steps taken, averaged across all days for each 5-minute
## interval
newinterval <- aggregate(steps ~ interval + daytype, newnewactivity, mean)

## add descriptive variable names
names(newinterval)[3] <- "mean_steps"

## check out new data frame
head(newinterval, 5)

library(lattice)
xyplot(
  mean_steps ~ interval | daytype,
  newinterval,
  type = "l",
  layout = c(1,2),
  main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days",
  xlab = "5-Minute Interval",
  ylab = "Average Number of Steps Taken"
)

## End of file