file_data<- read.csv(file="Telco-Customer-Churn.csv")
for(i in 1:ncol(file_data)){
  file_data[is.na(file_data[, i]), i]<-names(which.max(table(file_data[i],useNA = "no")))
}
apply(file_data,2,function(x) sum(is.na(x)))



file_data$customerID = NULL
file_data$X=NULL
as.factor(file_data$gender)
as.factor(file_data$Partner)
as.factor(file_data$Dependents)
as.factor(file_data$PhoneService)
as.factor(file_data$MultipleLines)
as.factor(file_data$InternetService)
as.factor(file_data$StreamingTV)
as.factor(file_data$StreamingMovies)
as.factor(file_data$Contract)
as.factor(file_data$PaperlessBilling)
as.factor(file_data$PaymentMethod)
data$Churn = as.factor(data$Churn)


library(caTools)
sample = sample.split(file_data,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(file_data,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(file_data, sample==FALSE)

train$Churn <- as.factor(train$Churn)    

test$Churn <- as.factor(test$Churn)

library(randomForest)
model = randomForest(Churn ~ . , data=train, ntree=100,importance=TRUE)


predct1 = predict(model,newdata = test ,type = 'class')

confusionMatrix(table(predct1,test$Churn))
