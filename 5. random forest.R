library(data.table)
library(caTools)
library(randomForest)
library(caret)

setwd("~/GitHub/bc2407")
trainset <- fread('trainset.csv', stringsAsFactors = T)
testset <- fread('testset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset <- rbind(testset[1, ] , trainset)
testset <- trainset[-1,]

y = trainset$readmitted
x = data.table(trainset[,c("gender", "age", "change", "A1Cresult", "diabetesMed", "insulin", "metformin", "glipizide", "glyburide", "pioglitazone", "admission_type_id", "admission_source_id", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")])
set.seed(1)  #for bootstrap sampling & RSF selection
rf <- randomForest(x, y = y, ntree = 500, importance = T, do.trace = TRUE)
saveRDS(rf, file = 'random-forest-model.rds')

#metrics of trainset
rf <- readRDS('random-forest-model.rds')
rf  #using ntree = 500 & default RSF size = floor(sqrt(ncol(x))) = 4
  # OOB error rate = 41.78%
  # false negative rate = 44.70%
  # false positive rate = 38.87%
plot(rf)
var.impt <- importance(rf)
var.impt
varImpPlot(rf, type = 1) #mean decrease in accuracy
varImpPlot(rf, type = 2) #mean decrease in gini

#predict on testset
rf.pred <- predict(rf, newdata = testset, type = 'class')
confusionMatrix(rf.pred, reference = testset$readmitted)



########################################################VERSION 2############################




dt3 <- dt1
dt3 <- dt3 %>% group_by() %>% slice_sample(n=2000)
## SPLIT New Dataset into Train & Test Sets ##
train=sample.split(Y=dt3$readmitted,SplitRatio = 0.7)
trainset=subset(dt3,train==T)
testset=subset(dt3,train==F)

m.rf <- randomForest(readmitted ~ . , data=trainset, importance=T)
m.rf

#error plot
plot(m.rf)
#if categorical, there is FP, FN, overall error

m.rf.y <- predict(m.rf, newdata = testset)
ConfusionMatrixRF <- table(m.rf.y,testset$readmitted)
ConfusionMatrixRF


var.impt.rf <- importance(m.rf)
print(var.impt.rf)
