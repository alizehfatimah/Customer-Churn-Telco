
data <- read.csv(file="Telco-Customer-Churn.csv")

#removing null or empty values from the data frame

for(i in 1:ncol(data)){
  data[is.na(data[, i]), i]<-names(which.max(table(data[i],useNA = "no")))
}

#split data on the base of Churn 
sample_data<-sample.split(data$Churn,SplitRatio = 0.7)
#train data where label is True
train_set<-subset(data,sample_data)
#test data where label is flase
test_set<-subset(data,!sample_data)
#library for naive bayesian
library(e1071)
navie_classifier<-naiveBayes(Churn~.,data=train_set) #get naive classifier 
navie_predictor<-predict(navie_classifier,test_set) # put naive classifier iwith test test to predict 

#making Confusion matrix to check accuracy
library(caret)
cf <- confusionMatrix(navie_predictor,test_set$Churn)
cf
