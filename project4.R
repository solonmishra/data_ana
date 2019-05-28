data<-read.csv("test.csv",header = T)
head(data,2)
data<-data.frame(data,big.cities=ifelse(data$City.Group=="Big Cities",1,0),others=ifelse(data$City.Group=="Other",1,0))
data<-data.frame(data,IL=ifelse(data$Type=='IL',1,0),FC=ifelse(data$Type=='FC',1,0),DT=ifelse(data$Type=='DT',1,0) , MB=ifelse(data$Type=='MB',1,0))
library(lubridate)
c1<-mdy(data$Open.Date)
c1<-data.frame(date=date(c1),month=month(c1),year=year(c1))
c1$date=as.numeric(format(c1$date,format="%d"))
data<-cbind(data,c1)

date<-mdy(data$Open.Date)
max(date)
months<-as.numeric(max(date)-date)/30
months<-round(months,2)
data<-cbind(data,months)

library(maps)
p<-data.frame(city=data$City)
p1<-data.frame(city=world.cities$name,pop=world.cities$pop)
p1<-p1[!duplicated(p1[,1]),]
m<-merge(p,p1,by ='city',all.x = T)
mean<-mean(m$pop,na.rm = T)
m$pop[is.na(m$pop)]<-mean
data<-data.frame(data,city.pop=m$pop)
head(data,2)
data<-data[c(38:47,1:37)]
