train1<-read.csv("trainBoruta")
test1<-read.csv("testBorta")

library('MASS')
library(elmNN)
model<-elmtrain(revenue~.,data=train1,actfun = "sig",nhid=10)
head(model$predictions)

#training error
mse<-sum((model$predictions-train1[,8])^2)/nrow(train1)
mse

#Testing the model
p<-predict(model,newdata = test1)
head(p)

#de-nomalise
d1<-read.csv("train.csv")
y_pred1<-(model$predictions*(max(d1$revenue)-min(d1$revenue)))+min(d1$revenue)
head(y_pred1)
nrow(y_pred1)

d2<-read.csv("testO.csv")
sub<-data.frame(Id=d2$Id,Prediction=y_pred1)
write.csv(sub,"submission2.csv")