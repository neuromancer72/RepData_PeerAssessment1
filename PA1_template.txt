# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


Add a new variable in the dataset: 
* newID: an identifier for the 5-minute interval within the day (a progressive integer  from 1 to 288).


```r
newID<-vector(mode="numeric",length=dim(dt)[1])

for(i in 1:dim(dt)[1]){
	newID[i]=ifelse(i%%288==0, 288, i%%288);
}
dt$newID<-newID
```

## What is mean total number of steps taken per day?

```r
TotSteps<-tapply(dt$steps,as.factor(dt$date),sum,na.rm=TRUE)
hist(as.vector(TotSteps),breaks=10,main='Total number of steps taken per day',xlab='Number of steps',ylab='',col='blue')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Mean and Median of total number of steps taken per day

```r
s<-summary(TotSteps)
s["Median"]
```

```
## Median 
##  10400
```

```r
s["Mean"]
```

```
## Mean 
## 9350
```


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
