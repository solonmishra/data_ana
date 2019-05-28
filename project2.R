data<-read.csv("train.csv",header = T)
head(data)

c<-data.frame(big.cities=ifelse(data$City.Group=="Big Cities",1,0),others=ifelse(data$City.Group=="Other",1,0))
data<-cbind(data,c)
data<-data[-4]
data<-data[c(1:41,43,44,42)]

boxplot(data$big.cities,data$revenue)
abline(data$revenue~data$big.cities,lwd=1,lty=5,col="blue")

unique(data$Type)
data<-data.frame(data,IL=ifelse(data$Type=='IL',1,0),FC=ifelse(data$Type=='FC',1,0),DT=ifelse(data$Type=='DT',1,0))
data<-data[-4]
data<-data[c(1,2,3,41,42,44,45,46,4:40,43)]

#install.packages("tidyverse")
#install.packages("lubridate")
library(lubridate)
c1<-mdy(data$Open.Date)
cor(data[-c(1,2)])
head(c1,10)
c1<-data.frame(date=date(c1),month=month(c1),year=year(c1))
c1$date=as.numeric(format(c1$date,format="%d"))

data<-cbind(data,c1)
data<-data[-2]
head(data,2)
data<-data[c(1,2,46,47,48,3:45)]

data<-data[-2]

cor(data$P6,data$revenue)

library(randomForest)

set.seed(1234)
regressor=randomForest(x=data[-47],y=data$revenue,ntree=10)

class(regressor)

importance(regressor)

cm<-data.frame(cor(data[10:46]))
write.csv(cm,"restaurant.csv")

install.packages("Boruta")
library(Boruta)
str(data)
set.seed(123)
boruta.train<-Boruta(revenue~.-Id,data = data,doTrace=2)
boruta.train$finalDecision

#Back elemination
reg<-lm(revenue~.,data = data)
summary(reg)

outlierVal<-boxplot.stats(data$P1)$out
outlierVal

fun<-function(x)
{
  qnt<-quantile(x,c(0.25,0.75))
  h=1.5*IQR(x)
  x[x<qnt[1]-h]<-NA
  x[x>qnt[2]+h]<-NA
  x
}
data$P8<-fun(data$P8)

write.csv(data,"train2.csv")

                   