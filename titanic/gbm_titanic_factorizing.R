library(gbm)
library(caret)
library(dplyr)

train<-read.csv('~/kaggle/titanic/train.csv')
test<-read.csv('~/kaggle/titanic/test.csv')

survived=train$Survived
train=select(train,-Survived)
end_trn=nrow(train)

all=rbind(train,test)
end=nrow(all)

all=select(all
           , Pclass
           , Sex
           , Age
           , SibSp
           , Parch
           , Fare
           , Embarked)

head(all)

#filling in missing ages with the mean
meanAge<-mean(all$Age,na.rm=TRUE)   
all$Age[is.na(all$Age)]<-meanAge
all$Fare[is.na(all$Fare)]<-mean(all$Fare[all$Pclass==3],na.rm=TRUE)
all$Parch[all$Parch>6]<-6
all$Parch<-as.factor(all$Parch)
all$SibSp<-as.factor(all$SibSp)
all$Pclass<-as.factor(all$Pclass)
ntrees=5000

model=gbm.fit(
  x=all[1:end_trn,]
  , y = survived
  , distribution = "bernoulli"
  , n.trees = ntrees
  , shrinkage= .01
  , interaction.depth=3
  , nTrain=round(end_trn*.8)
  , verbose = TRUE
)

summary(model)
gbm.perf(model)

testPredictions=predict(object=model,newdata=all[(end_trn+1):end,]
                        , n.trees = gbm.perf(model, plot.it=FALSE)
                        ,type = "response")

trainPredictions=predict(object=model,newdata=all[1:end_trn,]
                        , n.trees = gbm.perf(model, plot.it=FALSE)
                        ,type = "response")

testPredictions=round(testPredictions)
trainPredictions=round(trainPredictions)

result<-data.frame(test$PassengerId,testPredictions)
colnames(result)<-c("PassengerID","Survived")
write.csv(result,'~/kaggle/titanic/gbm_titanic_factorizing.csv',row.names=FALSE)