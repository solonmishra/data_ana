library(Boruta)
library(randomForest)
train<-read.csv("train3.csv")
test<-read.csv("testData.csv")
head(test,2)

set.seed(123)
boruta.train <- Boruta(revenue~., data = train, doTrace = 2)
boruta.train$finalDecision

train1<-train[c(1,3,9,10,25,28,31,36,46)]
test1<-test[c(8,7,10,11,26,29,32,37)]
write.csv(train1,"trainBoruta")
write.csv(test1,"testBoruta")

set.seed(123)
model<-randomForest(revenue~.,data = train1)
model

#prediction
y_pred<-predict(model,newdata = test1)
y_pred

#de-nomalise
d1<-read.csv("train.csv")
y_pred1<-(y_pred*(max(d1$revenue)-min(d1$revenue)))+min(d1$revenue)
y_pred1

d2<-read.csv("testO.csv")
sub<-data.frame(Id=d2$Id,Prediction=y_pred1)
write.csv(sub,"submission2.csv")
