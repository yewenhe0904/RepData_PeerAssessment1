#1
#Read data from the working directory
myactivity<-read.csv(paste0(getwd(),"/activity/activity.csv"))

#2
#Calculate the total numbers of steps per day by function tapply
ttl_spd<-tapply(myactivity$steps,INDEX = myactivity$date,sum)
ttl_spd<-as.data.frame(ttl_spd)
#Clean-up the data
names(ttl_spd)<-c("steps")
ttl_spd<-subset(ttl_spd,complete.cases(ttl_spd))
#Make a histogram of the total number of steps taken each day
#Call ggplot lib
library(ggplot2)
#Plot
range_steps<-range(ttl_spd,na.rm = T)[2]-range(ttl_spd,na.rm = T)[1]
g_hist<-ggplot(ttl_spd,aes(steps))
(g_hist  + geom_histogram(aes(steps),binwidth=range_steps/20,color="red",fill="tomato")
+ labs(x="Total steps taken each day")
+ labs(y="Count")
+ labs(title="Histogram of total steps per day"))

#report mean and median
#mean
mean(ttl_spd$steps,na.rm=T)
#median
median(ttl_spd$steps,na.rm = T)

#3
#Calculate the average steps taken for each 5-min interval across all days
avg_sp5_array<-tapply(myactivity$steps,INDEX = as.factor(myactivity$interval),mean,na.rm=T)
#Clean up the data
avg_sp5<-as.data.frame(as.numeric(1:288))
avg_sp5$interval<-as.numeric(names(avg_sp5_array))
avg_sp5$steps<-as.numeric(avg_sp5_array)
names(avg_sp5)<-c("interval_id","interval","steps")

#Plot
library(ggplot2)
g_timeseries<-ggplot(avg_sp5,aes(interval,steps))
(g_timeseries+geom_line()
+ labs(x="Time interval")
+ labs(y="Avg. steps")
+ labs(title="Times series plot of average steps taken for each 5-min interval across all days"))

#Find the interval contains the maximum steps
id_max<-match(max(avg_sp5$steps),avg_sp5$steps)
avg_sp5[id_max,"interval"]





