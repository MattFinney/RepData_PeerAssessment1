rm(list=ls())
require(ggplot2)
require(plyr)

# Loading and preprocessing the data
activity <- read.csv('activity.csv')

# Calculate the total number of steps taken per day
totalStepsByDay <- with(activity, tapply(steps,date,sum, na.rm=TRUE))

# Make a histogram of the total number of steps taken each day
hist(totalStepsByDay, xlab = 'Total number of steps', main = 'Histogram of total number of steps taken each day')

# Calculate and report the mean and median of the total number of steps taken
# per day

# Mean
mean(totalStepsByDay)

# Median
median(totalStepsByDay)


# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
meanStepsByInterval <- with(activity, tapply(steps,interval,mean, na.rm=TRUE))

plot(rownames(meanStepsByInterval),meanStepsByInterval, type='l', xlab = '5-minute interval', ylab = 'Average Steps Taken')

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
names(meanStepsByInterval[which.max(meanStepsByInterval)])

# Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e.
# the total number of rows with NAs)
sum(is.na(activity))

# Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
activity2 <- activity
activity2$steps <- ifelse(is.na(activity2$steps), meanStepsByInterval[as.character(activity2$interval)],activity2$steps)

# Calculate the total number of steps taken per day
totalStepsByDay2 <- with(activity2, tapply(steps,date,sum, na.rm=TRUE))

# Make a histogram of the total number of steps taken each day
hist(totalStepsByDay2, xlab = 'Total number of steps', main = 'Histogram of total number of steps taken each day')

# Calculate and report the mean and median of the total number of steps taken
# per day

# Mean
mean(totalStepsByDay2)

# Median
median(totalStepsByDay2)

# Do these values differ from the estimates from the first part of the
# assignment? What is the impact of imputing missing data on the estimates of
# the total daily number of steps?

# These values are different from the values in part 1 of the assignment.
# Imputing missing data on the estimates of the total daily number of steps
# appears to have brought the dataset into closer alignment with a standard
# normal curve.

# Create a new factor variable in the dataset with two levels – “weekday” and
# “weekend” indicating whether a given date is a weekday or weekend day.
activity2$weekday <- ifelse(weekdays(as.Date(activity2$date)) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')
activity2$weekday <- as.factor(activity2$weekday)

# Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis).
meanStepsByInterval2 <- ddply(activity2,.(interval,weekday),summarize, meanSteps = mean(steps))

g <- ggplot(meanStepsByInterval2, aes(y=meanSteps,x=interval)) + geom_line() + facet_grid(weekday ~ .) + xlab('Interval') +ylab('Average number of steps') + ggtitle('Average number of steps for each interval, split by weekday/weekend')
print(g)
