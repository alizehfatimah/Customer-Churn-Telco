data <- read.csv(file="C:/Users/dell/Documents/Telco-Customer-Churn.csv")

for(i in 1:ncol(data)){
  data[is.na(data[, i]), i]<-names(which.max(table(data[i],useNA = "no")))
}
paste("Total Records : ",nrow(data))
Fe_Male<- subset( data, gender == "Female")
paste("FeMale propotoin  : ",round((nrow(Fe_Male)/nrow(data)) *100,4)," Percent")


Male<- subset( data, gender == "Male")
paste("Male propotoin  :",round((nrow(Male)/nrow(data) )*100,4), "percent ")
Senior_citizen <- subset( data, SeniorCitizen == "1")
paste("Senir Citizen  propotoin  :",round((nrow(Senior_citizen)/nrow(data) )*100,4), "percent ")

cattypes <- c(levels(data$Contract)) #find unique values in the column and place them in a list

i <- 1
ac<-c(cattypes)
for (ctype in cattypes){
  
  val = nrow(subset(data, subset = Contract == ctype)) #pick out the name of the contract in the list and find the number of rows
  
  ac[[i]] <- val #enter the number of rows into a list
  
  i=i+1 #increment position
  
}

paste(cattypes[which.max(ac)],"with", max(ac) ,"users." ) 

intertypes <- c(levels(data$InternetService)) #find unique values in the column and place them in a list

i <- 1
ac<-c(intertypes )

for (itype in intertypes){
  
  val = nrow(subset(data, subset = InternetService == itype)) #pick out the name of the internet service in the list and find the number of rows
  
  ac[[i]] <- val #enter the number of rows into a list
  
  i=i+1 #increment position
  
}

paste(intertypes[which.max(ac)],"with", max(ac) ,"users.")


pmtypes <- c(levels(data$PaymentMethod)) #find unique values in the column and place them in a list

i <- 1
ac<-c(pmtypes )

for (ptype in pmtypes){
  
  val = nrow(subset(data, subset = PaymentMethod == ptype)) #pick out the name of the payment method in the list and find the number of rows
  
  ac[[i]] <- val #enter the number of rows into a list
  
  i=i+1 #increment position
  
}
paste("The least common payment method was",pmtypes[which.min(ac)],"with", min(ac) ,"users.")


females <- subset(data, subset = gender == "Female") #create a subset of the dataframe where gender is female
males <- subset(data, subset = gender == "Male") #create a subset of the dataframe where gender is male
paste("The average tenure of males is",round(mean(males$tenure),4),"months whereas for females it is",round(mean(females$tenure),4),"months. For the whole dataset the average tenure is ",round(mean(data$tenure),4),"months.")

strTV <- nrow(subset(data, subset = StreamingTV == "Yes")) #make a subset of rows where streaming TV is yes and find number of rows

strMovies <- nrow(subset(data, subset = StreamingMovies == "Yes")) #make a subset of rows where streaming movies is yes and find number of rows

paste("Is Streaming TV favored over the Streaming Movies service?", strTV > strMovies) #is streaming TV preferred or streaming movies?

paste(nrow(subset(data, subset = Churn == "Yes")),"customers churned.")

paste("The mean monthly charges for such customers was ",round(mean(subset(data, subset = Contract == "Month-to-month")$MonthlyCharges),4))

paste("For males, the average of total charges is ",round(mean(subset(data,gender == "Male")$TotalCharges, na.rm = TRUE),4),", whereas for females the average was,",round(mean(subset(data,gender == "Female")$TotalCharges, na.rm = TRUE),4))