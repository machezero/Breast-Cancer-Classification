library(readr)
dataR <- read_csv("C:/Users/GGEZ/Desktop/dataR2.csv") #Add data .csv

data <- dataR[c(1:3,10)] #select data

data$Class <- as.factor(ifelse(data$Classification > 1, 'Patients', 'Healthy')) # Creating a new column for the classification
data$Classification <- data$Class# 1:Healthy 2:Patients
data$Class <- NULL
str(data) # Checking column names in the dataframe
summary(data) # Summary of the dataframe

# Train-Test split
library(caTools)

set.seed(1000)
split = sample.split(data$Classification, SplitRatio = 0.80)# Dividing dataframe into train and test sets
train = subset(data, split==TRUE)# 80% of data is for training
test = subset(data, split==FALSE)# 20% of data is for testing

# Control Parameters for training
library(caret)
fitControl <- trainControl(method="cv", # CV stands for Cross-Validaton
                           number = 5, # 5-fold CV
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
## Modeling
# SVM-Radial
model_svm <- train(Classification~.,
                   train,
                   method="svmRadial", # Applying SVM classification using Radial Kernel
                   metric="ROC",
                   preProcess=c('center', 'scale'),
                   trace=FALSE,
                   trControl=fitControl)
pred_svm <- predict(model_svm, test)
cm_svm <- confusionMatrix(pred_svm, test$Classification)
cm_svm
