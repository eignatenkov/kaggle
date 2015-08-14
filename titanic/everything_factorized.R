#tweaking_gbm_verbose
library(caret)
library(randomForest)
#read the data
titanic<-read.csv('c:/docs/private/titanic/train.csv')
test<-read.csv('c:/docs/private/titanic/test.csv')

#forget ticket number, name & passengerID from training data
titanic<-titanic[,c(-1,-4,-9)]

#filling in missing ages with the mean
meanAge<-mean(titanic$Age,na.rm=TRUE)       
titanic$Age[is.na(titanic$Age)]<-meanAge    
test$Age[is.na(test$Age)]<-mean(test$Age,na.rm=TRUE)

#fill in missing Fare in one of the test data
test[153,9]<-mean(test$Fare[test$Pclass==3],na.rm=TRUE)

### factorize parch and SibSp variables
titanic$Parch<-as.factor(titanic$Parch)
titanic$SibSp<-as.factor(titanic$SibSp)
##something to do with parch = 9 in test data
test$Parch[test$Parch==9]=8
test$Parch<-as.factor(test$Parch)
test$SibSp<-as.factor(test$SibSp)

### factorize Survived variable
titanic$Survived<-as.factor(titanic$Survived)

#forget about cabin for a while
fit1<-randomForest(Survived~.,data=titanic[,-c(8)])
fit2<-train(Survived~.,data=titanic[,-c(8)],method='glm')
fit3<-train(Survived~.,data=titanic[,-c(8)],method='rf',verbose=FALSE)
fit4<-train(Survived~.,data=titanic[,-c(8)],method='gbm',verbose=FALSE)
fit5<-train(Survived~Sex*Fare*Pclass,data=titanic[,-c(8)],method='gbm',verbose=FALSE)
fit6<-train(Survived~.,data=titanic[,-8],method="rpart")
fit7<-train(Survived~.,data=titanic[,-8],method="lda")
fit8<-train(Survived~.,data=titanic[,-8],method="ada")
fit9<-train(Survived~Sex*Fare*Pclass,data=titanic[,-c(8)],method='glm')

##confusionMatrix(predict(fit6,newdata=titanic),titanic$Survived)



##factor levels in test data for random forest
levels(test$Embarked)<-c("","C","Q","S")

pTest1<-as.integer(predict(fit1,newdata=test))-1
pTest2<-as.integer(predict(fit2,newdata=test))-1
pTest3<-as.integer(predict(fit3,newdata=test))-1
pTest4<-as.integer(predict(fit4,newdata=test))-1
pTest5<-as.integer(predict(fit5,newdata=test))-1
pTest6<-as.integer(predict(fit6,newdata=test))-1
pTest7<-as.integer(predict(fit7,newdata=test))-1
pTest8<-as.integer(predict(fit8,newdata=test))-1
pTest9<-as.integer(predict(fit9,newdata=test))-1

result<-data.frame(test$PassengerId,((pTest1+pTest2+pTest3+pTest4+pTest5+pTest6+pTest7+pTest8+pTest9)>4)*1)
colnames(result)<-c("PassengerID","Survived")
write.csv(result,'c:/docs/private/titanic/aggr.csv',row.names=FALSE)