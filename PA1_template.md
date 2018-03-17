---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

activity<-read.csv("./activity.csv",header = TRUE)
activity[,2]<-as.Date(activity$date)
str(activity)

## What is mean total number of steps taken per day?

steps_1<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
hist(steps_1,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")

print(mean_steps<-mean(steps_1))
print(median_steps<-median(steps_1))

summary(steps_1)

## What is the average daily activity pattern?

avg_steps<-with(activity,tapply(steps,interval,mean,na.rm=TRUE))
intervals<-unique(activity$interval)
new<-data.frame(cbind(avg_steps,intervals))
plot(new$intervals,new$avg_steps,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Average Steps per Interval")

index<-which.max(new$avg_steps)
max<-new[index,2]

## Imputing missing values
sum(is.na(activity$steps))

index<-which(is.na(activity$steps))
l<-length(index)
steps_avg<-with(activity,tapply(steps,date,mean,na.rm=TRUE))
na<-mean(steps_avg,na.rm = TRUE)
for (i in 1:l) {
        activity[index[i],1]<-na
}

###check if there are still NAs & review histogram
sum(is.na(activity$steps))
str(activity)
steps_2<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
hist(steps_2,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")


## Are there differences in activity patterns between weekdays and weekends?
library(dplyr)

activity_mod<- mutate(activity, day = ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday", "weekend", "weekday"))
activity_mod$day<-as.factor(activity_mod$day)
str(activity_mod)

act_wknd<-subset(activity_mod,as.character(activity_mod$day)=="weekend")
act_wkdy<-subset(activity_mod,as.character(activity_mod$day)=="weekday")
steps_wknd<-with(act_wknd,tapply(steps,interval,mean,na.rm=TRUE))
steps_wkdy<-with(act_wkdy,tapply(steps,interval,mean,na.rm=TRUE))
int_wknd<-unique(act_wknd$interval)
int_wkdy<-unique(act_wkdy$interval)
new_wknd<-data.frame(cbind(steps_wknd,int_wknd))
new_wkdy<-data.frame(cbind(steps_wkdy,int_wkdy))
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(new_wknd$int_wknd,new_wknd$steps_wknd,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Weekend")
     
plot(new_wkdy$int_wkdy,new_wkdy$steps_wkdy,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Weekday")
