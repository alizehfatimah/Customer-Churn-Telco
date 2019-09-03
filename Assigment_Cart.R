data <- read.csv(file="Telco-Customer-Churn.csv") #file having no customer id to plot tree 
data$Churn <- as.factor(data$Churn) #make factor of on class label Churn
library(caTools)
sample<- sample.split(data,SplitRatio = 0.7) #split data on ratio of 70 
train <-subset(data,sample ==TRUE) #make train set on true 
test<-subset(data, sample==FALSE)#making test test on false 

library(rpart)
model <-rpart(Churn ~ . , data=train,method = "class") #train model on class label churn to train the model 

#3.1. Plot the tree and identify which variables appear in the tree.
library(rpart.plot)
prp(model,type = 5,fallen.leaves = FALSE,branch = 0) #plot the tree of the model that we train 

predction <- predict(model,newdata = test ,type = 'class') #predict test data on the basis of the train model 
library(caret)
cf<-confusionMatrix(predction, test$Churn) #making confusion matrix to test the results 


#3.2. Write all the decision rules that you get from the tree.
model # in this rules are those wo have *
#write down the rules from that 


cf #printing confusion matrix to see the results 

#3.3. What is the accuracy of your CART model on the test set?
round(cf$overall['Accuracy'],4)

conf_matrix_table<-table(predction,test$Churn)
#3.4. What is the true positive rate of the CART model on the test set?
round(sensitivity(conf_matrix_table),4)

#3.5. What is the false positive rate of the CART model on the test set?
false_positive<-1-round(sensitivity(conf_matrix_table),4)
false_positive

#3.6. What does the CART model predict for customer with a one-year contract and a tenure of 12?



