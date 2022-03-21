library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)

setwd("~/GitHub/bc2407")
dt1 <- fread('dt1-cleaned.csv', stringsAsFactors = T)
sapply(dt1, class)
dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
sapply(dt1, class)

trainset = fread('trainset.csv')
testset = fread('testset.csv')


#---------------------------------------------------------------------------------------------
#Logistic Regression

#perform log regression on train set
lr1 = glm(readmitted ~ . , family = binomial, data = trainset)
summary(lr1)

#statistically significant variables
#diabetesMed, insulin, metformin, glipizide, time_in_hospital,
#num_medications, number_emergency, number_inpatient, number_diagnoses

#check for vif
vif(lr1)

#do log regression with statistically significant variables
lr2 = glm(readmitted ~ diabetesMed + insulin + metformin + glipizide + time_in_hospital 
          + num_medications + number_emergency + number_inpatient + number_diagnoses, family = binomial, data = trainset)
summary(lr2)

#predict y with test set
lr2.predict = predict(lr2, newdata = testset, type = "response")

#create confusion matrix
threshold = 0.5
lr2.predict.f = factor(ifelse(lr2.predict > threshold, "1", "0"))
confusionMatrix(lr2.predict.f, reference = testset$readmitted)
#accuracy:0.6664


#---------------------------------------------------------------------------------------------
#CART

#get max tree using trainset
set.seed(123)
c1 = rpart(readmitted ~ ., data = trainset, method = 'class', control = rpart.control(cp=0))

printcp(c1)
plotcp(c1)

#find cp value for pruning
CVerror.cap = c1$cptable[which.min(c1$cptable[,"xerror"]),"xerror"] +
  c1$cptable[which.min(c1$cptable[,"xerror"]),"xstd"]

i<-1;j<-4
while (c1$cptable[i,j]>CVerror.cap){
  i<-i+1
}

cp.opt = ifelse(i>1,sqrt(c1$cptable[i,1]*c1$cptable[i-1,1]),1)

print(cp.opt)

# prune to get optimal tree
c2 = prune(c1,cp=cp.opt)
printcp(c2)

#regression tree (optimal)
rpart.plot(c2,nn=T)

#predict y with test set
cart.predict = predict(c2, newdata=testset, type = 'class')

#create confusion matrix
confusionMatrix(cart.predict, reference = testset$readmitted)
#accuracy: 0.6628

#compare the accuracy of log reg and cart
#accuracy of log reg: 0.6664
#accuracy of cart: 0.6628

#log is more accurate

#use the significant variables from log reg for neural network
#diabetesMed, insulin, metformin, glipizide, time_in_hospital, num_medications
#number_emergency, number_inpatient, number_diagnoses
