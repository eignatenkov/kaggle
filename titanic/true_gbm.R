library(gbm)
library(caret)
titanic<-read.csv('~/kaggle/titanic/train.csv')
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

boost.titanic=gbm(Survived~.,data=titanic)
                  #,interaction.depth = 2,cv.folds = 10)

confusionMatrix(predict(boost.titanic,newdata=titanic,n.trees=100),titanic$Survived)
#pTest3<-predict(fit6,newdata=test,type="response")
#pred=ifelse(pTest3<.5,0,1)
#result<-data.frame(test$PassengerId,pred)
#colnames(result)<-c("PassengerID","Survived")
#write.csv(result,'~/kaggle/titanic/gbm.csv',row.names=FALSE)