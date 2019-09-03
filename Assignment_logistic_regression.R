file_data<- read.csv(file="Telco-Customer-Churn.csv")
file_data$customerID <- NULL
file_data$X<-NULL
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
as.factor(file_data$Churn)


library(caTools)
sample <- sample.split(file_data,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train <-subset(file_data,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test<-subset(file_data, sample==FALSE)

model <- glm(Churn ~ . , data=train,family=binomial(link="logit"))
predictTrain <- predict(model, test,type="response")
summary(predictTrain)
tapply(predictTrain, test$Churn, mean)
conf_matrix_table<-table(test$Churn, predictTrain > 0.5)
conf_matrix_table
#What is the accuracy? 
diag<-diag(conf_matrix_table)
n <-sum(conf_matrix_table)
accuracy = sum(diag) / n 
accuracy
#Which variables are significant in the model?
#take all  points even  which have single * 

summary(model)



