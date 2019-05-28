data<-read.csv("train.csv")
unique(data$Type)
aggregate(data.frame(count=data$P1),list(value=data$P1),length)
head(data,2)
unique(data$P1)
unique(data$P2)
unique(data$P3)
barplot(data$P1)
hist(data$P2)
boxplot(data$P1)
boxplot(data$p2)
table(data$P2)
summary(data$p2)
summary(data)
sort(data$p2)
data$P2
sort(data$P2)
outlierVal<-boxplot.stats(data$revenue)$out
outlierVal<-boxplot.stats(data$P2)$out
outlierVal<-boxplot.stats(data$P11)$out
outlierVal<-boxplot.stats(data$P12)$out
outlierVal<-boxplot.stats(data$P13)$out
outlierVal<-boxplot.stats(data$P14)$out
outlierVal<-boxplot.stats(data$P15)$out
outlierVal<-boxplot.stats(data$P16)$out

sort(unique(outlierVal))
aggregate(data.frame(count=outlierVal),list(value=outlierVal),length)

boxplot(data$P1 ~ data$P5,data=data)
plot(data$P2,data$P1)
plot(data$Type,data$revenue)


rev<-data[43]
var(rev)

boxplot(data$P2)
m<-mean(data$P2)
q3=quantile(data$P2,0.75)
w1=q3+(1.5*IQR(data$P2))
w2=q3-(1.5*IQR(data$P2))
summary(data$P2)
for(i in 1:137)
{
  if(data$P2[i]<w2|| data$P2[i]>w1)
  {
    data$P2[i]=m
  }
}
boxplot(data$P2)
summary(data$P2)

#PCA
data<-read.csv("train.csv")
data2<-data[6:43]
library(caTools)
set.seed(123)
split=sample.split(data2$revenue,SplitRatio=0.8)
train=subset(data2,split==TRUE)
#head(train)
test=subset(data2,split==F)
#head(test)
str(train)
#feature Scaling
train=scale(train)
head(train)
test=scale(test)

library(caret)
library(lattice)
library(ggplot2)
library(e1071)

#pca=preProcess(x=train[,-38],method='pca',pcaComp = 10)
library(MASS)
lda = lda(formula =revenue ~ . , data = as.data.frame(training_set))
train_set=predict(pca,train)
head(train_set)
test_set=predict(pca,test)
head(test_set)
train_set=train_set[,c(2,3,4,5,6,7,8,9,10,1)]
head(train_set)
test_set=test_set[,c(2,3,4,5,6,7,8,9,10,1)]
head(test_set)

#LinearRegression
#fitting linear regression 
r<-lm(formula=revenue~.,data=as.data.frame(train_set))
r

#predicting
y_pred=predict(r,newdata = as.data.frame(test_set))
y_pred

#error
mse<-sum((y_pred-test_set[ ,10])^2)/nrow(test_set)
mse
