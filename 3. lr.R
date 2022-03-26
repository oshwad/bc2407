library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)

setwd("~/GitHub/bc2407")

trainset = fread('trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset = fread('testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)

#---------------------------------------------------------------------------------------------
#Logistic Regression

#perform log regression on train set
lr1 = glm(readmitted ~ . , family = binomial, data = trainset)
summary(lr1)

#use the step function to determine the variables to use in the model
s1 = step(lr1)
summary(s1)
s1$coefficients

#variables to use after backward elimination
#age, A1Cresult, diabetesMed, insulin, metformin, time_in_hospital
#num_procedures, num_medications, number_emergency, number_inpatient, number_diagnoses

#check for vif
vif(lr1)
#no need to remove any variables due to vif

#do log regression with variables identified using the step function
lr2 = glm(readmitted ~ age + A1Cresult + diabetesMed + insulin + 
            metformin + time_in_hospital + num_procedures + num_medications + 
            number_emergency + number_inpatient + number_diagnoses, family = binomial, 
          data = trainset)
summary(lr2)

#predict y with test set
lr2.predict = predict(lr2, newdata = testset, type = "response")

#create confusion matrix
threshold = 0.5
lr2.predict.f = factor(ifelse(lr2.predict > threshold, "1", "0"))
confusionMatrix(lr2.predict.f, reference = testset$readmitted)

#accuracy:0.8874


#use the significant variables from log reg for neural network
#age, A1Cresult, diabetesMed, insulin, metformin, time_in_hospital, num_procedures
#num_medications, number_emergency, number_inpatient, number_diagnoses

