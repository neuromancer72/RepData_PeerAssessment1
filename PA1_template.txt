# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
dt<-read.csv(unz('activity.zip','activity.csv'))
```

## What is mean total number of steps taken per day?

```r
TotSteps<-tapply(dt$steps,as.factor(dt$date),sum,na.rm=TRUE)
hist(as.vector(TotSteps),breaks=10,main='Total number of steps taken per day',xlab='Number of steps',ylab='',col='blue')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Mean and Median of total number of steps taken per day

```r
s<-summary(TotSteps)
print(paste("Median =",s["Median"]))
```

```
## [1] "Median = 10400"
```

```r
print(paste("Mean =",s["Mean"]))
```

```
## [1] "Mean = 9350"
```


## What is the average daily activity pattern?

```r
IntvAvg<-tapply(dt$steps,as.factor(dt$interval),mean,na.rm=TRUE)
```

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
plot(as.numeric(unique(dt$interval)),IntvAvg,type='l',main='Average number of steps taken per 5- minute interval',xlab='5- minute interval',ylab='Number of steps',col='red')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

5- minute interval with the maximum number of  steps on average

```r
print(paste("5-minute interval id: ",names(which.max(IntvAvg))));
```

```
## [1] "5-minute interval id:  835"
```


```r
print(paste("maximum number of  steps on average: ",IntvAvg[which.max(IntvAvg)]))
```

```
## [1] "maximum number of  steps on average:  206.169811320755"
```

## Imputing missing values

Total number of missing values in the dataset


```r
print(paste("Number of missing value: ",nrow(dt[is.na(dt$steps)==T | is.na(dt$date)==T | is.na(dt$interval),])))
```

```
## [1] "Number of missing value:  2304"
```

As a strategy for imputing missing data we create a new dataset (called dt2) equal to the original dataset but with the missing data for a particular 5-minute interval replaced with the mean for that 5-minute interval


```r
dt2<-dt;

newsteps<-vector(nrow(dt),mode="numeric")
for(i in 1:nrow(dt)){
	newsteps[i]=ifelse(is.na(dt$steps[i])==T,IntvAvg[as.character(dt$interval[i])],dt$steps[i]);
}

dt2$steps<-newsteps;
```

Total number of steps taken each day according the new dataset


```r
TotSteps2<-tapply(dt2$steps,as.factor(dt2$date),sum,na.rm=TRUE)
hist(as.vector(TotSteps2),breaks=10,main='',xlab='Number of steps',ylab='',col='blue')
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

Mean and median of the new dataset


```r
s2<-summary(TotSteps2)
print(paste("Median =",s2["Median"]))
```

```
## [1] "Median = 10800"
```

```r
print(paste("Mean =",s2["Mean"]))
```

```
## [1] "Mean = 10800"
```

Differences between the new dataset and the original one

```r
print(paste("Differences between the two Median =",s2["Median"] - s["Median"]))
```

```
## [1] "Differences between the two Median = 400"
```

```r
print(paste("Differences between the two Mean =",s2["Mean"]- s["Mean"]))
```

```
## [1] "Differences between the two Mean = 1450"
```

## Are there differences in activity patterns between weekdays and weekends?

Add a new factor variable in the dataset: 

The new factor variable has two levels – “weekday” and “weekend” indicating whether a given date is weekend (Saturday or Sunday) or a weekday 
day


```r
temp<-vector(mode="character",length=dim(dt2)[1])
for(i in 1:dim(dt2)[1]){
	a<-strptime(dt2$date[i],"%Y-%m-%d")
	temp[i]=ifelse((a$wday==0) | (a$wday==6),"weekend","weekday");
}
dt2$wd=as.factor(temp)
```

Differences in activity patterns between weekdays and weekends

```r
dt2_we=dt2[dt2$wd=="weekend",]
dt2_wd=dt2[dt2$wd=="weekday",]

IntvAvg_we<-tapply(dt2_we$steps,as.factor(dt2_we$interval),mean)
IntvAvg_wd<-tapply(dt2_wd$steps,as.factor(dt2_wd$interval),mean)


par(mfrow=c(2,1))
plot(as.numeric(names(IntvAvg_we)),IntvAvg_we,type='l',main='weekend',xlab='5- minute interval',ylab='Number of steps',col='blue')
plot(as.numeric(names(IntvAvg_wd)),IntvAvg_wd,type='l',main='weekday',xlab='5- minute interval',ylab='Number of steps',col='blue')
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 
