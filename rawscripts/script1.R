setwd('C:\\Users\\roberto\\RepData_PeerAssessment1')

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


