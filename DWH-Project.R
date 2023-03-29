df <- read.csv("Telco-Customer-Churn.csv")
columns <- colnames(df) #find all the columns in the dataframe
colnums <- ncol(df)
ac <- list()


findnull <- function(dataf, dfcol){
  
  dfcoltypes <- c(levels(dataf$dfcol)) #find unique values in the column and place them in a list
  
  i <- 1 
  for (ctype in dfcoltypes){
    val <- nrow(subset(dataf, subset = dfcol == ctype)) #pick out the name of the type in the list and find the number of rows
    
    ac[[i]] <- val #enter the number of rows into a list
    i <- i+1 #increment position
  }
  print(ac)
  modeval <- dfcoltypes[which.max(ac)] #find the location of the mode in the list and use it as an index in the type list to return the name 
  #print(modeval)
  
  #dfcol <- replace_na(dfcol, modeval)
  
 return(dfcol) 
}


for (val in columns){
  df$val <- findnull(df, val)
}

print('success')

