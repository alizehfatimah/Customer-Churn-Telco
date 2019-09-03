# Loading necessary libraries and dataset
library(readr)
data <-  read.csv(file="Telco-Customer-Churn.csv")
for(i in 1:ncol(data)){
  data[is.na(data[, i]), i]<-names(which.max(table(data[i],useNA = "no")))
}
print(any(is.na(data))) #check if there are any null values in the dataframe

# Training and test split
library(caTools)
original_outcomes <-table(data$Churn)
sample <- sample.split(data$Churn,SplitRatio = 0.7)
train <- subset(data,sample)
test <-subset(data,!sample)

# 2.1. Our baseline model in classification is to always predict the most frequent outcome in the training set. What is the most frequent outcome?
test_outcome <- table(test$Churn)
train_outcome <- table(train$Churn)
most_freq_outcome <- names(which.max(train_outcome))

paste('The most frequent outcome in training set is :',most_freq_outcome)

# 2.2. What is the accuracy of this baseline model on the training set?
train_accuracy <- unname(train_outcome[most_freq_outcome])/sum(unname(train_outcome))

paste('The accuracy of this baseline model on the training set is:',round(train_accuracy * 100,4))

# 2.3. What is the accuracy of this baseline model on the test set?
test_accuracy <- unname(test_outcome[most_freq_outcome])/sum(unname(test_outcome))

paste('The accuracy of this baseline model on the test set is:',round(test_accuracy * 100,4))

# 2.4. What is the true positive (TP) rate of the baseline model on the test set? 

#2.5. What is the false positive (FP) rate of the baseline model on the test set?
