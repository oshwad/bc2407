library(data.table)
library(caTools)
library(randomForest)
library(caret)

setwd("~/GitHub/bc2407")
trainset <- fread('trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset <- fread('testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)

set.seed(2022)  #for bootstrap sampling & RSF selection
rf <- randomForest(readmitted ~ ., data = trainset, ntree = 500, importance = T, do.trace = TRUE)
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

# fixing the new factors error -----
for (i in 1:ncol(testset)){
  print(summary(testset[[i]]))
  print(summary(trainset[[i]]))
}

summary(testset$admission_source_id)
summary(trainset$admission_source_id)
summary(testset$admission_type_id)
summary(trainset$admission_type_id)

testset <- testset[testset$admission_source_id != '10',]
testset <- testset[testset$admission_type_id != '7',]
testset <- droplevels(testset)

testset <- rbind(trainset[1, ], testset)
testset <- testset[-1,]

#predict on testset
rf.pred <- predict(rf, newdata = testset, type = 'class')
confusionMatrix(rf.pred, reference = testset$readmitted)
