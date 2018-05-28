library(dummies)
library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(C50)


set.seed(100)

## DATA IMPORT
bankdata = read.csv(file = "../bank-full.csv", header = TRUE, sep = ';')

# Create Training, Testing Data (70/30)
bankdatasample <- sample.int(n = nrow(bankdata), size = floor(.70*nrow(bankdata)), replace = F)
bank_traindata <- bankdata[bankdatasample, ]
bank_testdata  <- bankdata[-bankdatasample, ]

# Fit the Decison Tree using Training Data
dtmodel = C5.0(x=bank_traindata[,-17],y=bank_traindata[,17])

# Varying the data-size vs./ Training and Testing Errors
indices = c(5000,10000,15000,20000,25000,30000,31647)
Tr_err_DT = c(0,0,0,0,0,0,0)
Tst_err_DT = c(0,0,0,0,0,0,0)

# Looping to calculate Training and Testing Errors for different Training Datasizes
for(i in 1:length(indices))
  {
	# Create Training Subset
    train_subset = bank_traindata[1:indices[i],]
	
	# Model fitting on Training Data 
    dtmodel = C5.0(x=train_subset[,-17],y=train_subset[,17])
	
	# Training and test prediction
    train = predict(dtmodel, newdata = train_subset,type="class")
    predicted = predict(dtmodel, newdata = bank_testdata, type= "class")
    
	# Training and testing errors
    Tr_err_DT[i] = 1- sum( train == train_subset$y ) / length( train )
    Tst_err_DT[i] = 1- sum( predicted == bank_testdata$y ) / length( predicted )
    
  }
# Create Data Frame to store Training and testing errors
subdata_dt_df = data.frame(indices,Tr_err_DT,Tst_err_DT)  
  
# Plot Training and Test errors as a function of data-size
print(ggplot(subdata_dt_df, aes(indices)) +                    
          geom_line(aes(y=Tr_err_DT), colour="red", size = 2) +  
          geom_line(aes(y=Tst_err_DT), colour="blue", size = 2) + 
          labs(title = "Decision Tree Error Curves [Train (red) & Test (blue)]", 
               y = "Train/Test_Error", x = "Data Size") +
          scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error")))



#PRUNING

cf = c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.01,0.001)
Training_Error = c(0,0,0,0,0,0,0,0,0,0,0,0)
Testing_Error = c(0,0,0,0,0,0,0,0,0,0,0,0)

# Looping through different pruning levels to calculate training and testing errors
for(i in 1:length(cf))
{ 
	# Fitting the prunded models 
	prunedmodel = C5.0(x=bank_traindata[,-17],y=bank_traindata[,17],control = C5.0Control(CF= cf[i]))

	# Prediciting for train and test data
	train = predict(prunedmodel, newdata = bank_traindata,type="class")
	predicted = predict(prunedmodel, newdata = bank_testdata, type= "class")

	# Calculate the training and testing errors
	Training_Error[i] = 1- sum( train == bank_traindata$y ) / length( train )
	Testing_Error[i] = 1- sum( predicted == bank_testdata$y ) / length( predicted)
}

print(Training_Error)
print(Testing_Error)

# Create Dataframe combining training and testing errors
pruned_df <- data.frame(cf, Training_Error, Testing_Error)

# Plot the training and testing erros as a function of pruning levels
ggplot(pruned_df, aes(cf)) +                    
   geom_line(aes(y=Training_Error), colour="red", size = 2, label = "Train") +  
  geom_line(aes(y=Testing_Error), colour="blue", size = 2) + 
  labs(title = "pruning vs error (Train (Red) & Test (Blue))", 
       y = "Train/Test_Error", x = "Pruning Confidence Level")  +
  scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error"))


# Choose the optimum pruning level  
cf1=c(0.5)

# Varying the data-size vs./ Training and Testing Errors
ind = c(5000,10000,15000,20000,25000,30000,31647)
Tr_err = c(0,0,0,0,0,0,0)
Tst_err = c(0,0,0,0,0,0,0)
# cf_min = 0.2


# Looping through different pruning levels, and varying training data to calculate training and testing errors
for(j in 1: length(cf1)){
for(i in 1:length(ind))
{
  # Create training subsets
  train_subset = bank_traindata1[1:ind[i],]

  # Fitting the prunded models
  prunedmodel = C5.0(x=train_subset[,-17],y=train_subset[,17],control = C5.0Control(CF= cf1[j]))
  
  # Prediciting for train and test data
  train = predict(prunedmodel, newdata = train_subset,type="class")
  predicted = predict(prunedmodel, newdata = bank_testdata, type= "class")
  
  # Calculate the training and testing errors
  Tr_err[i] = 1- sum( train == train_subset$y ) / length( train )
  Tst_err[i] = 1- sum(predicted == bank_testdata$y ) / length( predicted )

}
  # Create Data Frame storing training and testing errors 
  subdata_df = data.frame(ind,Tr_err,Tst_err)  
  
  # Plot Training and Test errors as a function of training data-size
  print(ggplot(subdata_df, aes(ind)) +                    
  geom_line(aes(y=Tr_err), colour="red", size = 2) +  
  geom_line(aes(y=Tst_err), colour="blue", size = 2) + 
  labs(title = "data-size vs error (Train (Red) & Test (Blue))", 
       y = "Train/Test_Error", x = "Data Size")  + 
  scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error")))
}
  
 