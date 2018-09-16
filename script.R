# common script
library(zip)
library(data.table)
Sys.setlocale("LC_TIME","C")

# 1. Code for reading in the dataset and/or processing the data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile <- "activity.zip"
datafile <- "activity.csv"
if (!file.exists(zipfile)) download.file(url, zipfile)
if (!file.exists(datafile)) unzip(zipfile)

if (!exists("da")) da <- read.csv(datafile)

da$weekday <- weekdays(as.Date(da$date))
da$date2 <- as.POSIXct(da$date, format="%Y-%m-%d")
da$time2 <- as.POSIXct(formatC(da$interval,width=4,flag="0"), format="%H%M")

# 2. Histogram of the total number of steps taken each day

totalStepDay<-tapply(da$step, da$date, sum, rm.na=TRUE)
hist(totalStepDay, xlab="steps", 
     main="total number of steps taken each day")

# 3. Mean and median number of steps taken each day

p.mean<-mean(totalStepDay, na.rm=T)
p.median<-median(totalStepDay, na.rm=T)

# 4. Time series plot of the average number of steps taken

da.tidy <- da[!is.na(da$steps),]

meanInt<-aggregate(da.tidy$steps, by=list(da.tidy$time2), mean)
names(meanInt)<-c("interval", "steps")
plot(meanInt$interval, meanInt$steps, type="l", xlab="interval",
     ylab="mean of steps", main="average number of steps")


# 5. The 5-minute interval that, on average, contains the maximum
#    number of steps

maxInt<-aggregate(da.tidy$steps, by=list(da.tidy$interval), max)
p.max<-max(maxInt[,2])

# 6. Code to describe and show a strategy for imputing missing data

p.missing<-nrow(da[is.na(da$steps),])

# 7. Histogram of the total number of steps taken each day after
#    missing values are imputed

totalStepDay<-tapply(da$step, da$date, sum)
hist(totalStepDay, xlab="steps", 
     main="total number of steps taken each day after missing values are imputed")

# 8. Panel plot comparing the average number of steps taken
#    per 5-minute interval across weekdays and weekends

da.tidy.we<-subset(da.tidy, da.tidy$weekday=="Saturday"|
                            da.tidy$weekday=="Sunday")
da.tidy.wd<-subset(da.tidy, da.tidy$weekday!="Saturday"&
                            da.tidy$weekday!="Sunday")

meanIntwe<-aggregate(da.tidy.we$steps, by=list(da.tidy.we$time2), mean)
meanIntwd<-aggregate(da.tidy.wd$steps, by=list(da.tidy.wd$time2), mean)
names(meanIntwe)<-c("interval", "steps")
names(meanIntwd)<-c("interval", "steps")
par.old<-par(no.readonly=T)
par(mfrow=c(1,2))
plot(meanIntwe$interval, meanIntwe$steps, type="l", ylim=c(0,250),
     xlab="interval", ylab="mean of steps",
     main="average number of steps, weekend")
plot(meanIntwd$interval, meanIntwd$steps, type="l", ylim=c(0,250),
     xlab="interval", ylab="mean of steps",
     main="average number of steps, weekday")
par(par.old)
