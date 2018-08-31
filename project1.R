library("lubridate")
library("dplyr")
library("ggplot2")

## 3.1 Loading data

if(!(file.exists("activity.csv")))
{
           if(!(file.exists("repdata_data_activity.zip")))  
           {
                      url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
                      download.file(url,"repdata_data_activity.zip")
           }
           unzip("repdata_data_activity.zip",overwrite = TRUE)
}


my_data<-tbl_df(read.csv("activity.csv",na.strings = "NA",colClasses = "character"))
my_data$date<-ymd(my_data$date)
my_data$interval<-as.integer(my_data$interval)
my_data$steps<-as.integer(my_data$steps)

##3.1 Histogram of the total number of steps taken each day
my_data_h<-my_data%>%select(date,steps)%>%group_by(date)%>%summarise_all(c(funs(mean),funs(sum),funs(median)))


png(filename = "each_day_hist1.png")
hist(my_data_h$sum,breaks = 26,col = "red", main = "Histogram number of steps", xlab = "steps quantity")
dev.off()


## 3.3 Mean and median number of steps taken each day
median_steps=median(my_data_h$sum,na.rm = TRUE)
print(paste("median steps quantity", median_steps))

mean_steps=mean(my_data_h$sum,na.rm = TRUE)
print(paste("Mean steps quantity", mean_steps))


## 3.4 Time series plot of the average number of steps taken

mean_interval<-my_data%>%
  filter(!is.na(steps))%>%
  select(interval,steps)%>%
  group_by(interval)%>%
  summarise_all(funs(mean)) 

png(filename ="ave_steps_taken_per_day.png",width = 680, height = 680)
plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l",main = "Time series plot of the average number of steps taken",
     xlab = "interval", ylab = "average steps", col="red")          
dev.off()


## 3.5 The 5-minute interval that, on average, contains the maximum number of steps

top_int<-my_data%>%filter(!is.na(steps))%>%select(interval,steps)%>%
  group_by(interval)%>%summarise_all(funs(mean))%>%
  arrange(desc(steps))%>%head(1)
print(top_int)

##3.6 Code to describe and show a strategy for imputing missing data
print(paste("Quantity NA rows",my_data%>%filter(is.na(steps))%>%nrow))
na_date<-my_data%>%filter(is.na(steps))%>%select(date)%>%unique()
print(paste("date with NA data",as.character(na_date)))
##insering data in steps field
my_data[is.na(my_data$steps),"steps"]<-mean_interval$steps


##3.7 Histogram of the total number of steps taken each day after missing values are imputed
##my_data_h have average and summrised data
my_data_h<-my_data%>%select(date,steps)%>%
  group_by(date)%>%summarise_all(c(funs(mean),funs(sum)))

png(filename ="ave_steps_taken_per_day_without_NA.png",width = 680, height = 680)
plot(x=my_data_h$date,y=my_data_h$mean,type = "l",main = "Plot average steps by day without NA", xlab = "days", ylab = "average steps", col="red")
my_data%>%select(interval,steps)%>%group_by(interval)%>%summarise_all(c(funs(mean),funs(sum)))
dev.off()

##3.8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## weekdays 7-saturday 1-sunday

my_data<-my_data%>%mutate(weekday = wday(date))

mean_interval_w<-my_data%>%
  filter(weekday==c(1,7))%>%
  select(interval,steps)%>%
  group_by(interval)%>%
  summarise_all(funs(mean))

mean_interval<-my_data%>%
  filter(weekday %in% c(2:6))%>%
  select(interval,steps)%>%
  group_by(interval)%>% 
  summarise_all(funs(mean))

png(filename = "weekday_siple_days_plot.png",width = 900,height = 900)
par(mfrow=c(2,1))
plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l",main = "Time series plot of the average number of steps taken on weekdays", 
     xlab = "interval", ylab = "average steps", col="green")    
plot(x=mean_interval$interval,y=mean_interval$steps,
     type = "l", main = "Time series plot of the average number of steps taken on other days",
     xlab = "interval", ylab = "average steps", col="blue") 
dev.off()
