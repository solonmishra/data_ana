train<-read.csv("train3.csv")
test<-read.csv("testData.csv")



fun<-function(x)
{
  qnt<-quantile(x,c(0.25,0.75))
  h=1.5*IQR(x)
  x[x<qnt[1]-h]<-NA
  x[x>qnt[2]+h]<-NA
  x
}
boxplot(test$P37)
test$P37<-fun(test$P37)

test$P37[is.na(test$P37)]<-mean(test$P37,na.rm=T)
boxplot(test$P37)

boxplot.stats(test$P37)$out

library(Boruta)
set.seed(123)
b<-Boruta(revenue~.,data = train, doTrace = 1)
b$finalDecision
t1<-train[c(1,3,9,10,14,22,29,33,36,44,46)]
head(test,2)
t2<-test[c(8,7,10,11,15,23,30,34,37,45)]

library(randomForest)
set.seed(123)
model<-randomForest(revenue~.,data = t1)
model

#prediction
y_pred<-predict(model,newdata = t2)
head(y_pred)

#de-nomalise
d1<-read.csv("train.csv")
y_pred1<-(y_pred*(max(d1$revenue)-min(d1$revenue)))+min(d1$revenue)
head(y_pred1)

d2<-read.csv("testO.csv")
sub<-data.frame(Id=d2$Id,Prediction=y_pred1)
write.csv(sub,"submission4.csv")
