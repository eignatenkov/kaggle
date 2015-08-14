train<-read.csv('~/Dropbox/kaggle/liberty/train.csv')
test <- read_csv('~/Dropbox/kaggle/liberty/test.csv')

extractFeatures <- function(data) {
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
  for (col in character_cols) {
    data[,col] <- as.factor(data[,col])
  }
  return(data)
}

trainFea <- extractFeatures(train)
testFea  <- extractFeatures(test)
library(randomForest)
gini<-function(y_true,y_pred){
  if (length(y_true)!=length(y_pred)) return("different lengths")
  n_samples=length(y_true)
  true_order=sort(y_true, decreasing = TRUE)
  pred_order=sort(y_pred, decreasing = TRUE)
  
  L_true=cumsum(true_order)/sum(true_order)
  L_pred=cumsum(pred_order)/sum(pred_order)
  L_ones=seq(0,1, length.out = n_samples)

  G_true = sum(L_ones - L_true)
  G_pred = sum(L_ones - L_pred)
  
  return(G_pred/G_true)
}

#we'll use validation set to estimate our models

valtrain=sample(1:nrow(train),nrow(train)*2/3)

rf <- randomForest(Hazard~., data=train[2:34], subset = valtrain, ntree=100, imp=TRUE, sampsize=10000, do.trace=TRUE)

predictions=predict(rf, train[-valtrain,3:34])

gini(train[-valtrain,2], predictions)

stupidrf<-randomForest(trainFea[,3:34], trainFea$Hazard, subset = valtrain, ntree=100, imp=TRUE, do.trace=TRUE)

stupidpred=predict(stupidrf, trainFea[-valtrain,3:34])

gini(trainFea[-valtrain,2], stupidpred)

rostercoeff<- function(base, real) {
  coeff = seq(1,1/6, length.out = 6)
  sum(round(sort(real,decreasing=TRUE)*coeff))/sum(round(sort(base,decreasing=TRUE)*coeff))
}
