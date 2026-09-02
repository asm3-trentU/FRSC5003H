# Example 1 for Classification Lecture - Poison Shrooms
# Arun S. Moorthy; arunmoorthy@trentu.ca
#
# First Draft: March 13, 2024
# Updated: March 2, 2026
#===============================================================================

rm(list=ls()) # This command clears the work space -- it's a holdover from my days writing compiled code 

# ==============================================================================

# This block of code  checks whether the packages I'd like to use (listed in the 
# array "necessaryPackages") are already installed on my computer. If not, the 
# packages will be installed and loaded. If they are installed, the packages 
# will be loaded.

necessaryPackages = c("party","caret") 

for(i in necessaryPackages){
  if(paste0(i) %in% installed.packages()==FALSE){
    install.packages(paste0(i))
    library(i,character.only=TRUE)
  } else {
    library(i,character.only=TRUE)
  }
}


# ==============================================================================

# This block of code loads up the necessary data set and collects dimension info

shroom_data = read.csv("Shroom_example.csv",stringsAsFactors = TRUE) # This makes sure that categorical variables are not just characters
nObs = dim(shroom_data)[1]
nFeatures = dim(shroom_data)[2]


# ==============================================================================

# This block of code splits the data into training and testing sets

trainRowNumbers = createDataPartition(shroom_data$edible,p=0.75,list=FALSE,times=1) # This command selects ~75% of data as training data, maintaining the balance of edible and poisonous data

trainData = shroom_data[trainRowNumbers,] # Selects the rows that were identified in "trainRowNumbers

testData = shroom_data[-trainRowNumbers,] # Removes the rows that were identified in "trainRowNumbers"

trainX = trainData[,2:23] # selects columns 2 to 23 as our features/predictors/independent variable
trainY = trainData[,1] # selects column 1 as our response/dependent variable

# ==============================================================================

# This block of code helps us build some intuition via plotting

plot(as.factor(trainData$cap.shape),as.factor(trainData$edible),xlab="cap shape",ylab="edible")
plot(as.factor(trainData$cap.color),as.factor(trainData$edible),xlab="cap color",ylab="edible")
plot(as.factor(trainData$bruises),as.factor(trainData$edible),xlab="bruises",ylab="edible")
plot(as.factor(trainData$odor),as.factor(trainData$edible),xlab="odor",ylab="edible")
plot(as.factor(trainData$stalk.color.above.ring),as.factor(trainData$edible),xlab="stalk color above ring",ylab="edible")

# ==============================================================================

#Decision tree as a function of odor
fit_odor<-ctree(edible~odor,data=trainData) # This trains the model with the trainData
summary(fit_odor) # This is a summary of the model
plot(fit_odor,uniform=TRUE) # This is a plot of the model

odor_train_predictions = predict(fit_odor,newdata=trainData) # Make a prediction using the model and train data
encoded_train_predictions = ifelse(odor_train_predictions=="p",0,1)
encoded_train_true = ifelse(trainData$edible=="p",0,1)
odor_train_Accuracy = sum(encoded_train_true==encoded_train_predictions)/length(encoded_train_predictions)
odor_train_R2 = cor(encoded_train_predictions,encoded_train_true)^2

odor_test_predictions = predict(fit_odor,newdata=testData) # Make a prediction using the model and train data
encoded_test_predictions = ifelse(odor_test_predictions=="p",0,1)
encoded_test_true = ifelse(testData$edible=="p",0,1)
odor_test_Accuracy = sum(encoded_test_true==encoded_test_predictions)/length(encoded_test_predictions)
odor_test_R2 = cor(encoded_test_predictions,encoded_test_true)^2



#Decision tree as a function of all inputs
fit_all<-ctree(edible~.,data=trainData) # This trains the model with the trainData
summary(fit_all) # This is a summary of the model
plot(fit_all,uniform=TRUE) # This is a plot of the model

all_train_predictions = predict(fit_all,newdata=trainData) # Make a prediction using the model and train data
encoded_train_predictions = ifelse(all_train_predictions=="p",0,1)
encoded_train_true = ifelse(trainData$edible=="p",0,1)
all_train_Accuracy = sum(encoded_train_true==encoded_train_predictions)/length(encoded_train_predictions)
all_train_R2 = cor(encoded_train_predictions,encoded_train_true)^2

all_test_predictions = predict(fit_all,newdata=testData) # Make a prediction using the model and train data
encoded_test_predictions = ifelse(all_test_predictions=="p",0,1)
encoded_test_true = ifelse(testData$edible=="p",0,1)
all_test_Accuracy = sum(encoded_test_true==encoded_test_predictions)/length(encoded_test_predictions)
all_test_R2 = cor(encoded_test_predictions,encoded_test_true)^2





