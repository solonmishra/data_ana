data<-read.csv("train.csv",header = T)
d<-read.csv("train.csv",header = T)
head(data,2)
data<-data.frame(data,big.cities=ifelse(data$City.Group=="Big Cities",1,0),others=ifelse(data$City.Group=="Other",1,0))
head(data,2)
data<-data.frame(data,IL=ifelse(data$Type=='IL',1,0),FC=ifelse(data$Type=='FC',1,0),DT=ifelse(data$Type=='DT',1,0))
library(lubridate)
c1<-mdy(data$Open.Date)
c1<-data.frame(date=date(c1),month=month(c1),year=year(c1))
c1$date=as.numeric(format(c1$date,format="%d"))
data<-cbind(data,c1)
data<-data[-c(2,4,5)]
data<-data[c(1,2,46,47,48,41,42,43,44,45,3:40)]
write.csv(data,"train2.csv")

#working with dates

data<-read.csv("train2.csv")
date<-mdy(d$Open.Date)
max(date)
months<-as.numeric(max(date)-date)/30
months<-round(months,2)
data<-cbind(data,months)
data<-data[c(1,46,2:45)]
head(data,2)
write.csv(data,"train2.csv")

library(ggplot2)
qplot(revenue,months,data=data)+geom_smooth()+ggtitle("Life of restaurant(months) V/s revenue")
ggsave("D://months.jpeg")
#install.packages("maps")
unique(data$City)
aggregate(data.frame(count=data$City),list(value=data$City),length)


library(maps)
head(world.cities)


p<-data.frame(city=data$City)
p1<-data.frame(city=world.cities$name,pop=world.cities$pop)
m<-merge(p,p1,by ='city',all.x = T)
m<-m[c(-3,-4)]
mean<-mean(m$pop,na.rm = T)
m$pop[is.na(m$pop)]<-mean

  max<-max(m$pop)
  min<-min(m$pop)
  for(j in 1:nrow(m)){
    m$pop[j]<-(m$pop[j]-min)/(max-min)
  }
data<-data.frame(data,city.pop=m$pop)
head(data,2)
data<-data[c(1,46,2:45)]

#linear regression 
for(i in 1:ncol())
{
  max<-max(data[,i])
  min<-min(data[,i])
  for(j in 1:nrow(data)){
    data[j,i]<-(data[j,i]-min)/(max-min)
  }
}

write.csv(data,"train3.csv")
train<-read.csv("train3.csv")
data<-read.csv("train.csv")
data1<-read.csv("testO.csv")
test<-read.csv("testdata.csv")

regressor<-lm(formula = revenue~.,data=train)
regressor

#prediction
y_pred=predict(regressor,newdata = test)
y_pred
y_pred<-(y_pred*(max(data$revenue)-min(data$revenue)))+min(data$revenue)
sub<-data.frame(Id=data1$Id,Prediction=y_pred)
write.csv(sub,"sub.csv")
