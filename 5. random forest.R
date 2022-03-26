library(data.table)
library(caTools)
library(randomForest)
library(caret)

setwd("~/GitHub/bc2407")
trainset <- fread('trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset <- fread('testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)

set.seed(1)  #for bootstrap sampling & RSF selection
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

#predict on testset
rf.pred <- predict(rf, newdata = testset, type = 'class')
confusionMatrix(rf.pred, reference = testset$readmitted)




# fixing the new factors error -----
for (i in 1:ncol(testset)){
  print(summary(testset[[i]]))
  print(summary(us[[i]]))
}

summary(testset$admission_source_id)
summary(us$admission_source_id)
summary(testset$admission_type_id)
summary(us$admission_type_id)

testset <- testset[testset$admission_source_id != '10',]
testset <- testset[testset$admission_type_id != '7',]
testset <- droplevels(testset)

testset <- rbind(ds[1, ] , testset)
testset <- testset[-1,]




# down/up sample function -----

y = 'readmitted'
x = (names(trainset) != "readmitted")

#downSample function
set.seed(1)
ds <- downSample(trainset[,..x], trainset$readmitted, yname = 'readmitted')
summary(ds)
set.seed(1)  #for bootstrap sampling & RSF selection
rf_ds <- randomForest(readmitted ~ ., data = ds, ntree = 500, importance = T, do.trace = TRUE)
rf_ds
rf_pred_ds <- predict(rf_ds, newdata = testset, type = 'class')
confusionMatrix(rf_pred_ds, reference = testset$readmitted)

#upSample function
set.seed(1)
us <- upSample(trainset[,..x], trainset$readmitted, yname = 'readmitted')
summary(us)
set.seed(1)  #for bootstrap sampling & RSF selection
rf_us <- randomForest(readmitted ~ ., data = us, ntree = 500, importance = T, do.trace = TRUE)
rf_us
rf_pred_us <- predict(rf_us, newdata = testset, type = 'class')
confusionMatrix(rf_pred_us, reference = testset$readmitted)



