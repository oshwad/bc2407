library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(car)
library(caTools)


setwd("~/GitHub/bc2407")

trainset = fread('data-trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset = fread('data-testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)

#---------------------------------------------------------------------------------------------
#Logistic Regression

#perform log regression on train set
lr1 = glm(readmitted ~ . , family = binomial, data = trainset)
summary(lr1)

#check for vif
vif(lr1)
#no need to remove any variables due to vif

#use the step function to determine the variables to use in the model
s1 = step(lr1)

#predict y with test set
s1.predict = predict(s1, newdata = testset, type = "response")

#create confusion matrix
threshold = 0.5
s1.predict.f = factor(ifelse(s1.predict > threshold, "1", "0"))
confusionMatrix(s1.predict.f, reference = testset$readmitted)

#accuracy:0.6669

summary(s1)
#use the statistically significant variables from log reg for neural network

#trainset error
train.predict = predict(s1, newdata = trainset, type = "response")
train.predict.f = factor(ifelse(train.predict > threshold, "1", "0"))
confusionMatrix(train.predict.f, reference = trainset$readmitted)

