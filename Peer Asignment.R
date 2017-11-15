# load the data using read.csv()

setwd("C:/Users/pujas/Desktop/Data Science/REproducible Research/week2/Assignment")
data<- read.csv("activity.csv")
data$date<- as.Date(data$date)

#Mean total number of steps taken per day
stepsbyday <- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)
qplot(stepsbyday, xlab = "Number of Steps taken Each Day", ylab = "Total Frequency", binwidth =500)
medianbyday <- median(stepsbyday)
meanbyday<- mean(stepsbyday)

#The average daily activity pattern
avg <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(names(avg), avg, xlab="5-min interval", type="l", ylab="Average no. of steps")
maxavg<- max(avg)
maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])


#Imputing missing values
totalna<- sum(is.na(data$steps))

# creating a copy of data set so that the missing value can be imputed in it
imputedata <- data

# Devise a strategy for filling in all of the missing values in the datase.
# In place of NA, using the mean for that 5-minute interval
imputedata$steps[which(is.na(data$steps))] <- as.vector(avg[as.character(data[which(is.na(data$steps)),3])])

#Creating a new dataset that is equal to the original dataset but with the missing data filled in.
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)

#Calculating the differences in activity patterns between weekdays and weekends
imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")

#A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
ggplot(aggregateData, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dayType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")