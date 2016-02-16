#loading library
library(ggplot2)
library(knitr)

#Setting The echo
opts_chunk$set(echo = TRUE, results = 'hold')

#Assuming the raw dataset(.zip) downloaded into the working directory
unzip("repdata_data_activity.zip", overwrite = T)

#Reading data
datasource <- read.csv ('activity.csv', header = TRUE, sep = ",",
                colClasses=c("numeric", "character", "numeric"))

#Preprocessing - Formating date(as date) and interval (as factor)
datasource$date <- as.Date(datasource$date, format = "%Y-%m-%d")
datasource$interval <- as.factor(datasource$interval)

#Accumulating total step/day and creating day vs step_perday table
steps_perday <- aggregate(steps ~ date, datasource, sum)
colnames(steps_perday) <- c("date","steps")
head(steps_perday)

#Drawing Histogram for total steps/day
hist(x=steps_perday$steps ,col="green",breaks=20,xlab="Total Steps/Day",
     ylab="Frequency",
     main="The Distribution of Total Steps/Day")

#CALCULATING mean
mean (steps_perday$steps, na.rm=TRUE)

#CALCULATING medium
median(steps_perday$steps, na.rm=TRUE)

#PATTERN AVERAGE PER INTERVAL , use integer to ensure smooth time series plot
steps_per_interval <- aggregate(list(steps=datasource$steps),   by=list(interval=as.integer(datasource$interval)), FUN=mean,na.rm=TRUE)
head(steps_per_interval)


#Plotting Timeseries for PAttern
plot(steps_per_interval$interval, 
     steps_per_interval$steps,
     xlab="5 minutes intervals", ylab="Average steps per Interval",type="l")

#Maximum Steps
maximum_steps <- max(steps_per_interval$steps)
max_interval <- steps_per_interval[steps_per_interval$steps==maximum_steps,]
max_interval