titanic<-read.csv('~/kaggle/titanic/train.csv')
test<-read.csv('~/kaggle/titanic/test.csv')
#forget ticket number, name & passengerID from training data
titanic<-titanic[,-c(1,4,9,11)]

#filling in missing ages with the mean
meanAge<-mean(titanic$Age,na.rm=TRUE)       
titanic$Age[is.na(titanic$Age)]<-meanAge    
test$Age[is.na(test$Age)]<-mean(test$Age,na.rm=TRUE)

#fill in missing Fare in one of the test data
test[153,9]<-mean(test$Fare[test$Pclass==3],na.rm=TRUE)

### factorize parch  variables
titanic$Parch<-as.factor(titanic$Parch)
### empty Embarked > "S"
titanic$Embarked[titanic$Embarked==""]="S"
titanic$Embarked=factor(titanic$Embarked)
### factorize Pclass
titanic$Pclass=factor(titanic$Pclass)
test$Pclass=factor(test$Pclass)
##something to do with parch = 9 in test data
test$Parch[test$Parch==9]=6
test$Parch<-factor(test$Parch)

### factorize Survived variable
titanic$Survived<-as.factor(titanic$Survived)

#get rid of not needed variables

library(e1071)
tune.out=tune(svm,Survived~.,data=titanic,kernel="radial",
              ranges=list(cost=10^seq(-3,3,1),gamma=10^seq(-3,3,1)))

best.mod=tune.out$best.model
table(predict(best.mod,titanic),titanic$Survived)
#Another search for best parameters
tune.out=tune(svm,Survived~.,data=titanic,kernel="radial",
              ranges=list(cost=seq(1,101,10),gamma=seq(0,1,.1)))

tmpmodel=svm(Survived~.,data=titanic,kernel="radial",cost=1,gamma=.2)
#table(predict(tmpmodel,titanic),titanic$Survived)
pred=predict(best.mod,test)
result<-data.frame(test$PassengerId,pred)
colnames(result)<-c("PassengerID","Survived")
write.csv(result,'~/kaggle/titanic/svm.csv',row.names=FALSE)
