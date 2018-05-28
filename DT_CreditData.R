library(dummies)
library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(C50)
library(xgboost)

set.seed(100)

## DATA IMPORT
creditdata = read.csv(file = "../credit-data.csv", header = TRUE, sep = ',')

# Create Training, Testing Data (70/30)
creditdatasample <- sample.int(n = nrow(creditdata), size = floor(.70*nrow(creditdata)), replace = F)
credit_traindata <- creditdata[creditdatasample, ]
credit_testdata  <- creditdata[-creditdatasample, ]

# Fit the Decison Tree using Training Data
dtmodel = C5.0(x=credit_traindata[,-16],y=credit_traindata[,16])

# Varying the data-size vs./ Training and Testing Errors
dIndx = c(50,100,150,200,250,300,350,400,482)
Train_err_DT = c(0,0,0,0,0,0,0,0,0)
Test_err_DT = c(0,0,0,0,0,0,0,0,0)

# Looping to calculate Training and Testing Errors for different Training Datasizes
for(i in 1:length(dIndx))
{
  # Create Training Subset
  train_subset = credit_traindata[1:dIndx[i],]
  
  # Model fitting on Training Data 
  dtmodel = C5.0(x=train_subset[,-16],y=train_subset[,16])
 
  # Training and test prediction
  train = predict(dtmodel, newdata = train_subset,type="class")
  predicted = predict(dtmodel, newdata = credit_testdata, type= "class")
 
  # Training and testing errors 	
  Train_err_DT[i] = 1- sum(train == train_subset$A16 ) / length( train )
  Test_err_DT[i] = 1- sum(predicted == credit_testdata$A16 ) / length( predicted )
  
}

# Create Data Frame to store Training and testing errors
crdata_dt_df = data.frame(dIndx,Train_err_DT,Test_err_DT)  

# Plot Training and Test errors as a function of data-size
print(ggplot(crdata_dt_df, aes(dIndx)) +                    
        geom_line(aes(y=Train_err_DT), colour="red", size = 2) +  
        geom_line(aes(y=Test_err_DT), colour="blue", size = 2) + 
        labs(title = "Decision Tree Error Curves [Train (Red) & Test (Blue)]", 
             y = "Train/Test_Error", x = "Data Size") +
        scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error")))



#PRUNING

# Define different levels of pruning
cfcred = c(1,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.5,0.4,0.3,0.2)
Tr_pruning_Error = c(0,0,0,0,0,0,0,0,0,0,0,0)
Tst_pruning_Error = c(0,0,0,0,0,0,0,0,0,0,0,0)

# Looping through different pruning levels to calculate training and testing errors
for(i in 1:length(cfcred))
{
  prunedmodel = C5.0(x=credit_traindata[,-16],y=credit_traindata[,16],control = C5.0Control(CF= cfcred[i]))
  #summary(prunedmodel)
  train = predict(prunedmodel, newdata = credit_traindata,type="class")
  predicted = predict(prunedmodel, newdata = credit_testdata, type= "class")
  
  Tr_pruning_Error[i] = 1- sum( train == credit_traindata$A16 ) / length( train )
  Tst_pruning_Error[i] = 1- sum( predicted == credit_testdata$A16 ) / length( predicted)
}

#print(Training_Error)
#print(Testing_Error)

# Create Dataframe combining training and testing errors
cr_pruned_df <- data.frame(cfcred, Training_Error, Testing_Error)

# Plot the training and testing erros as a function of pruning levels
ggplot(cr_pruned_df, aes(cfcred)) +                   
  geom_line(aes(y=Tr_pruning_Error), colour="red", size = 2, label = "Train") +  
  geom_line(aes(y=Tst_pruning_Error), colour="blue", size = 2) + 
  labs(title = "pruning vs error (Train (Red) & Test (Blue))", 
       y = "Train/Test_Error", x = "Pruning Confidence Level")  +
  scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error"))


# optimum level of pruning obtained at cf=0.9

dIndx = c(50,100,150,200,250,300,350,400,482)
Tr_err = c(0,0,0,0,0,0,0,0,0)
Tst_err = c(0,0,0,0,0,0,0,0,0)

# Looping to calculate training and testing errors by varying training data-size
  for(i in 1:length(dIndx))
  {
    # Create training subsets
	train_subset = credit_traindata[1:dIndx[i],]
    
	# Fitting the prunded models
    prunedmodel = C5.0(x=train_subset[,-16],y=train_subset[,16],control = C5.0Control(CF= 0.9))

	# Prediciting for train and test data
    train = predict(prunedmodel, newdata = train_subset,type="class")
    predicted = predict(prunedmodel, newdata = credit_testdata, type= "class")
    
	# Calculate the training and testing errors
    Tr_err[i] = 1- sum(train == train_subset$A16 ) / length( train )
    Tst_err[i] = 1- sum(predicted == credit_testdata$A16 ) / length( predicted )
    
  }
  # Create Data Frame storing training and testing errors 
  pruned_data_df = data.frame(dIndx,Tr_err,Tst_err)  
  
  # Plot Training and Test errors as a function of training data-size
  print(ggplot(pruned_data_df, aes(dIndx)) +                    
          geom_line(aes(y=Tr_err), colour="red", size = 2) +  
          geom_line(aes(y=Tst_err), colour="blue", size = 2) + 
          labs(title = "data-size vs error [Train (Red) & Test (Blue)]", 
               y = "Train/Test_Error", x = "Data Size")  + 
          scale_color_discrete(name = "Legend", labels = c("Train Error", "Test Error")))