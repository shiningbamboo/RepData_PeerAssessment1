unzip("repdata%2Fdata%2Factivity.zip")
rm(list=ls())
library(ggplot2)
library(knitr)
library(dplyr)
activity <- read.csv("activity.csv")
act <- activity[!is.na(activity$steps),]
summary(act)
dateid <- group_by(act, date)
daily_total_steps <- summarise(dateid, sumstep=sum(steps))
hist(daily_total_steps$sumstep, main="Histogram of Total Number of Steps per Day", xlab = "Total Number of Steps per Day")
mean(daily_total_steps$sumstep)
median(daily_total_steps$sumstep)
interval_id <- group_by(act, interval)
int <- summarise(interval_id, avgstep=mean(steps))
plot(int$interval, int$avgstep, type="l", main="Average Activity Across Intervals", xlab="Interval", ylab="Average Steps")
rnum <- which.max(int$avgstep)
print(paste("The interval with maximum number of steps", int[rnum,2], "is", int[rnum,1]))
na_act <- activity[is.na(activity$steps),]
print(paste("Number of NAs is:", nrow(na_act)))
interval_id_na <- group_by(activity, interval)
fill_step <- summarise(interval_id_na, steps=mean(steps, na.rm=T))
na_act <- na_act[,2:3]
na_act <- merge(fill_step, na_act)
na_act <- na_act[,c(2,3,1)]
act_new <- rbind(act, na_act)
act_new <- act_new[order(act_new$date),]
date_id_na <- group_by(act_new, date)
daily_steps <- summarise(date_id_na, step_tot=sum(steps))
hist(daily_steps$step_tot, main="Histogram of Total Number of Steps per Day after Replacing NAs", xlab = "Total Number of Steps per Day")
mean(daily_steps$step_tot)
median(daily_steps$step_tot)
act_new$day_type <- weekdays(as.Date(act_new[,2]))
act_new$day_type[act_new$day_type %in% c("Saturday","Sunday")] <- "weekend"
act_new$day_type[act_new$day_type != "weekend"] <- "weekday"
act_new$day_type <- as.factor(act_new$day_type)
act_new_by_day <- aggregate(steps ~ interval + day_type, act_new, mean)
qplot(x=interval, y=steps, data=act_new_by_day, geom="line", xlab="Intervals", ylab="Number of Steps") + facet_wrap(~day_type, ncol=1)
