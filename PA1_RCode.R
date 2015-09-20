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

#4 Imputing missing data
#Calculate and report the total number of missing values in the dataset
completecases<-complete.cases(myactivity)
sum(!completecases)
#Use the mean 5-minute data to fill the missing values
#Define a function to return mean 5-minute data given time interval
match_sp5 <- function (time_int){
        id<-match(x = time_int,table = avg_sp5$interval)
        return(avg_sp5[id,"steps"])
}
#Use a for-loop imputing missing values
new_myactivity<-myactivity
for (i in 1:nrow(new_myactivity))
        {
        if (!completecases[i]){
                new_myactivity[i,"steps"]<-match_sp5(new_myactivity[i,"interval"])}
}

#Calculate the total numbers of steps per day by function tapply
new_ttl_spd<-tapply(new_myactivity$steps,INDEX = new_myactivity$date,sum)
new_ttl_spd<-as.data.frame(new_ttl_spd)
#Clean-up the data
names(new_ttl_spd)<-c("steps")
new_ttl_spd<-subset(new_ttl_spd,complete.cases(new_ttl_spd))
#Make a histogram of the total number of steps taken each day
#Call ggplot lib
library(ggplot2)
#Plot
new_range_steps<-range(new_ttl_spd,na.rm = T)[2]-range(new_ttl_spd,na.rm = T)[1]
new_g_hist<-ggplot(new_ttl_spd,aes(steps))
(new_g_hist  + geom_histogram(aes(steps),binwidth=new_range_steps/20,color="blue",fill="green")
+ labs(x="Total steps taken each day")
+ labs(y="Count")
+ labs(title="Histogram of total steps per day (missing values imputed)"))

#report mean and median
#mean
mean(new_ttl_spd$steps,na.rm=T)
#median
median(new_ttl_spd$steps,na.rm = T)

#5
#Weekdays and weekends
my_weekdays <- weekdays(as.Date(new_myactivity$date))
my_weekdays <- as.factor(my_weekdays)
levels(my_weekdays)<-c("weekday","weekday","weekend","weekend","weekday","weekday","weekday")
new_myactivity$weekday_flag<-my_weekdays
#Table
table(new_myactivity$weekday_flag)
#Calculate the mean steps taken in 5-min interval by two groups
new_avg_sp5<-tapply(new_myactivity$steps,INDEX = list(as.factor(new_myactivity$interval),as.factor(new_myactivity$weekday_flag)),mean)
#Clean up the data
new_avg_sp5<-as.data.frame(new_avg_sp5)
wd_avg<-data.frame("interval"=as.numeric(row.names(new_avg_sp5)),"steps"=new_avg_sp5$weekday,"weekday_flag"=rep("weekday",nrow(new_avg_sp5)))
we_avg<-data.frame("interval"=as.numeric(row.names(new_avg_sp5)),"steps"=new_avg_sp5$weekend,"weekday_flag"=rep("weekend",nrow(new_avg_sp5)))
new_wk_avg<-rbind(wd_avg,we_avg)
#Panel Plot
library(ggplot2)
new_g_timeseries=ggplot(new_wk_avg,aes(interval,steps))
(new_g_timeseries+geom_line()
                +facet_grid(weekday_flag ~ .)
                + labs(x="Time interval")
                + labs(y="Avg. steps")
                + labs(title="Times series plot of average steps taken for each 5-min interval across different groups"))

