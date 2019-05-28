d1<-read.csv("train.csv")
d2<-read.csv("testO.csv")
train<-read.csv("train3.csv")
test<-read.csv("testData.csv")
head(test,2)

library(randomForest)
library(ggplot2)
set.seed(123)
model<-randomForest(revenue~.,data = train)
model
plot(model)
ggsave("D://Rmodel.jpeg")
importance(model)
varImpPlot(model)
ggsave("D://varImpPlot.jpeg")
abline(v=0.05)

train1<-train[c(1,2,3,9,10,14,16,20,27,28,29,30,31,36,37,46)]
head(train1,2)

set.seed(123)
model<-randomForest(revenue~.,data = train1)
model

test1<-test[c(8,9,7,10,11,15,17,21,28,29,30,31,32,37,38)]
head(test1,2)
#prediction
y_pred<-predict(model,newdata = test1)
y_pred

#de-nomalise
y_pred1<-(y_pred*(max(d1$revenue)-min(d1$revenue)))+min(d1$revenue)
y_pred1

sub<-data.frame(Id=d2$Id,Prediction=y_pred1)
write.csv(sub,"submission.csv")
