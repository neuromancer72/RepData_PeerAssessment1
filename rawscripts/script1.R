setwd('C:\\Users\\roberto\\RepData_PeerAssessment1')
#setwd('C:\\Users\\Roberto\\RepData_PeerAssessment1')

dt<-read.csv(unzip(zipfile="activity.zip"))
newID<-vector(mode="numeric",length=dim(dt)[1])

for(i in 1:dim(dt)[1]){
	newID[i]=ifelse(i%%288==0, 288, i%%288);
}
dt$newID<-newID


names(dt)
#"steps"    "date"     "interval"

unique(dt$interval)

library(knitr)
knit("PA1_template.Rmd")
knit2html("PA1_template.md")


IntvAvg<-tapply(dt$steps,as.factor(dt$interval),mean,na.rm=TRUE)

plot(as.numeric(unique(dt$interval)),IntvAvg,type='l',main='Average number of steps taken per 5- minute interval',xlab='5- minute interval',ylab='Number of steps',col='red')
axis(3,at=c(0,600,1200,1800,2300),lab=c('0:00','6:00','12:00','18:00','23:00'),pos=210,padj=1)


newsteps<-vector(nrow(dt),mode="numeric")

for(i in 1:nrow(dt)){
	newsteps[i]=ifelse(is.na(dt$steps[i])==T,IntvAvg[as.character(dt$interval[i])],dt$steps[i]);
}

TotSteps2<-tapply(dt2$steps,as.factor(dt2$date),sum,na.rm=TRUE)
hist(as.vector(TotSteps2),breaks=10,main='',xlab='Number of steps',ylab='',col='blue')
