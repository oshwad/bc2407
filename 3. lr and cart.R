library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)

dt2 <- fread('dt2-cleaned.csv', stringsAsFactors = T)
sapply(dt2, class)
dt2[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
sapply(dt2, class)

set.seed(2022)
train = sample.split(Y = dt2$readmitted, SplitRatio = 0.7)
trainset = subset(dt2, train == T)
testset = subset(dt2, train == F)
#remove patient number 
trainset = trainset [, c(-1)]
testset = testset [, c(-1)]

#perform log regression with train set
lr1 = glm(readmitted ~ . , family = binomial, data = trainset)
summary(lr1)

#gender, age, diabetesMed, insulin, metformin, admission_type_id, admission_source_id, time_in_hospital,
#num_lab_procedures, num_medications, number_emergency, number_inpatient, number_diagnoses, repeat_patient


vif(lr1)

lr2 = glm(readmitted ~ gender + age + diabetesMed + insulin + metformin + admission_type_id 
          + admission_source_id + time_in_hospital + num_lab_procedures + num_medications
          + number_emergency + number_inpatient + number_diagnoses + repeat_patient, family = binomial, data = trainset)
summary(lr2)

testset1 =  testset[ !(testset$admission_source_id == 10),] 
summary(testset1$admission_source_id)

lr2.predict = predict(lr2, newdata = testset1, type = "response")
result = data.table(testset1$readmitted, lr2.predict)
View(result)

threshold1 = 0.5
lr2.predict.test <- ifelse(lr2.predict > threshold1, "1", "0")
table1 <- table(Testset.Actual = testset1$readmitted, lr2.predict.test, deparse.level = 2)
table1



#CART 

set.seed(2022)

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
cart.predict<-predict(c2,newdata=testset1, type = 'class')
cart.predict
result1<-data.frame(testset1$readmitted,cart.predict)

confusionMatrix(cart.predict, reference = testset1$readmitted)
