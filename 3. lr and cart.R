library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)
library("ROSE")

setwd("~/GitHub/bc2407")
dt1 <- fread('dt1-cleaned.csv', stringsAsFactors = T)
sapply(dt1, class)
dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
sapply(dt1, class)
dt2 <- ROSE(readmitted~., data = dt1, N = nrow(dt1), seed=111)$data
table(dt2$readmitted)

#create trainset and testset with 70-30 split
set.seed(123)
train = sample.split(Y = dt2$readmitted, SplitRatio = 0.7)
trainset = subset(dt2, train == T)
testset = subset(dt2, train == F)
#remove patient number 
trainset = trainset [, c(-1)]
testset = testset [, c(-1)]

#---------------------------------------------------------------------------------------------
#Logistic Regression

#perform log regression on train set
lr1 = glm(readmitted ~ . , family = binomial, data = trainset)
summary(lr1)

#gender, age, diabetesMed, insulin, metformin, admission_type_id, admission_source_id, time_in_hospital,
#num_lab_procedures, num_medications, number_emergency, number_inpatient, number_diagnoses, repeat_patient

#check for vif
vif(lr1)

#do log regression with statistically significant variables
lr2 = glm(readmitted ~ gender + age + diabetesMed + insulin + metformin + admission_type_id 
          + admission_source_id + time_in_hospital + num_lab_procedures + num_medications
          + number_emergency + number_inpatient + number_diagnoses + repeat_patient, family = binomial, data = trainset)
summary(lr2)

#predict y with testset
lr2.predict = predict(lr2, newdata = testset, type = "response")
result = data.table(testset$readmitted, lr2.predict)
View(result)

#create confusion matrix
threshold1 = 0.5
lr2.predict.test <- ifelse(lr2.predict > threshold1, "1", "0")
table1 <- table(Testset.Actual = testset$readmitted, lr2.predict.test, deparse.level = 2)
table1

#calculate accuracy
(7943+11070)/nrow(testset)
#accuracy = 0.7025

#---------------------------------------------------------------------------------------------
#CART 

set.seed(2022)

#get max tree using trainset
c1 = rpart(readmitted ~ ., data = trainset, method = 'class', control = rpart.control(cp=0))

printcp(c1)
plotcp(c1)

#find cp value for pruning
CVerror.cap<-c1$cptable[which.min(c1$cptable[,"xerror"]),"xerror"]+
  c1$cptable[which.min(c1$cptable[,"xerror"]),"xstd"]

i<-1;j<-4
while (c1$cptable[i,j]>CVerror.cap){
  i<-i+1
}

cp.opt=ifelse(i>1,sqrt(c1$cptable[i,1]*c1$cptable[i-1,1]),1)

print(cp.opt)

# prune to get optimal tree
c2<-prune(c1,cp=cp.opt)
printcp(c2)

#regression tree (optimal)
rpart.plot(c2,nn=T)

#predict y with testset
cart.predict<-predict(c2,newdata=testset, type = 'class')
cart.predict
result1<-data.frame(testset$readmitted,cart.predict)

confusionMatrix(cart.predict, reference = testset$readmitted)
#accuracy=0.7248

#compare the accuracy of log reg and cart
#accuracy of log reg: 0.7025
#accuracy of cart: 0.7248

#cart is more accurate

#use the significant variables from cart in neural network
#significant variables: repeat patient, number_inpatient, number_emergency, number_outpatient, num_medications, time_in_hospital
