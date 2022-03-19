library(data.table)
library("ROSE")
library(caTools)
library(randomForest)
library(caret)

setwd("~/GitHub/bc2407")
dt1 <- fread('dt1-cleaned.csv', stringsAsFactors = T)
dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
dt2 <- ROSE(readmitted~., data = dt1, N = nrow(dt1), seed=111)$data
table(dt2$readmitted)

set.seed(2022)
train = sample.split(Y = dt2$readmitted, SplitRatio = 0.7)
trainset = subset(dt2, train == T)
testset = subset(dt2, train == F)
#remove patient number 
trainset = trainset [, c(-1)]
testset = testset [, c(-1)]

y = trainset$readmitted
x = trainset[,names(trainset) != "readmitted"]
set.seed(1)  #for bootstrap sampling & RSF selection
rf <- randomForest(x, y = y, ntree = 500, importance = T, do.trace = TRUE)
saveRDS(rf, file = 'random-forest-model.rds')

#metrics of trainset
rf <- readRDS('random-forest-model.rds')
rf  #using ntree = 500 & default RSF size = floor(sqrt(ncol(x))) = 4
  # OOB error rate = 24.83%
  # false negative rate = 18.32%
  # false positive rate = 31.32%
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
