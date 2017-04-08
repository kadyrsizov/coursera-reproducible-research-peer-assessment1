#Course: Reproducible research
#Set up the working directory


##Loading and preprocessing the data

setwd("C:/Users/Dana/Desktop/Projects/coursera/Reproducible Research/Week2")

# make sure the sources data folder exists
if (!file.exists('source data')) {
  dir.create('source data')
}

# check to see if the existing tidy data set exists; if not, make it...
if (!file.exists('source data/repdata_2Fdata2Factivity.zip')) {
  
  # download the zip file and unzip
	unzip('source data/repdata_2Fdata_2Factivity.zip',exdir='source data',overwrite=TRUE)
 
  # Read Activity monitoring data ('data') data set from the working directory
	data <- read.csv("./source data/activity.csv", stringsAsFactors = FALSE)
} else {
  
  # Read Activity monitoring data ('data') data set from the working directory
	data <- read.csv("./source data/activity.csv", stringsAsFactors = FALSE)
	
}


#Looking at a summary for the dataset using “summary” and “str” methods:

summary(data)
str(data)


#Looking at the first 6 rows of the dataset:

head(data)

#Converting the “date” variable to a Date classe and the “interval” variable to a factor:

data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

#1. What is mean total number of steps taken per day?

#Subsitting the dataset to ignore missing values

NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

#Aggregating the number of steps taken each day:
#Creating a data frame with the steps taken for each day
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)
#Adding column names to the created data frame
colnames(steps_each_day) <- c("date", "steps")


#Making a histogram of the total number of steps taken each day:
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

#number of steps taken per day:
#Mean
mean(steps_each_day$steps)

#Median
median(steps_each_day$steps)


##What is the average daily activity pattern?	
#Calculating the average
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)

#Adding columns names
colnames(steps_per_interval) <- c("interval", "average_steps")

#ploting the average daily activity pattern 
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

#The maximum number of average steps
max_steps <- max(steps_per_interval$average_steps)
max_steps

##Imputing missing values
sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))

#finding the indices of missing values (NAs)
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
#Imputing missing values using the mean for that 5-minute interval
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
                steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
                }))
summary(complete_data)
str(complete_data)

#4 - Making a histogram of the total number of steps taken each day for the complete dataset:

#Creating a data frame with the steps taken for each day
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
#Adding column names to the created data frame
colnames(steps_each_day_complete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

mean(steps_each_day_complete$steps)
median(steps_each_day_complete$steps)



##Are there differences in activity patterns between weekdays and weekends?

#Creating a factor variable "day "to store the day of the week:
complete_data$day <- as.factor(weekdays(complete_data$date))

#Creating a logical variable "is_weekday" (weekday=TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 


#Calculating the average number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)


#Calculating the average number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
#steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

#Adding columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
#colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_per_interval_weekdays$day <- "Weekday"
# steps_per_interval_weekends$day <- "Weekend"

#Merging the two togather
week_data <- steps_per_interval_weekdays
#Converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)

#Making the plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
 
	