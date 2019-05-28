library(caret)
library(lattice)
library(ggplot2)
library(e1071)
head(test1,2)
test1<-test[c(10:46)]
pca=preProcess(x=train1,method='pca',pcaComp = 3)
pca
train_set=predict(pca,train)
head(train_set)
train_set=train_set[c(1:8,10:12,9)]#re-arranging
head(train_set)

test_set=predict(pca,test)
head(test_set)

library(randomForest)
set.seed(123)
model<-randomForest(revenue~.,data = train_set)
model

#prediction
y_pred<-predict(model,newdata = test_set)
y_pred

#de-nomalise
y_pred1<-(y_pred*(max(d1$revenue)-min(d1$revenue)))+min(d1$revenue)
y_pred1

sub<-data.frame(Id=d2$Id,Prediction=y_pred1)
write.csv(sub,"submission1.csv")
