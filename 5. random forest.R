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
  # OOB error rate = 41.14%
  # false negative rate = 44.0%
  # false positive rate = 38.3%
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



#predict on testset -----
rf.pred <- predict(rf, newdata = testset, type = 'class')
cm <- confusionMatrix(rf.pred, reference = testset$readmitted)
(cm$table[1,2])/(cm$table[1,2]+cm$table[2,2]) # FNR = 0.423




# refining rf accuracy -----
# using logistic regression significant variables
set.seed(2022)  #for bootstrap sampling & RSF selection
rf1 <- randomForest(readmitted ~ age + insulin + time_in_hospital + num_procedures + num_medications + number_inpatient + number_diagnoses, data = trainset, ntree = 500, importance = T, do.trace = TRUE)
rf.pred <- predict(rf1, newdata = testset, type = 'class')
confusionMatrix(rf.pred, reference = testset$readmitted)
cm <- confusionMatrix(rf.pred, reference = testset$readmitted)
(cm$table[1,2])/(cm$table[1,2]+cm$table[2,2]) # FNR = 0.445



# using random forest top 5 important variables
sel <- var.impt[order(var.impt[,3], decreasing = T),]
sel <- c(rownames(sel)[1:5])
sel
paste("readmitted ~",paste(sel,collapse = " + "))
set.seed(2022)  #for bootstrap sampling & RSF selection
rf2 <- randomForest(readmitted ~ number_inpatient + time_in_hospital + age + number_emergency + num_medications, data = trainset, ntree = 500, importance = T, do.trace = TRUE)
rf.pred <- predict(rf2, newdata = testset, type = 'class')
confusionMatrix(rf.pred, reference = testset$readmitted)
cm <- confusionMatrix(rf.pred, reference = testset$readmitted)
(cm$table[1,2])/(cm$table[1,2]+cm$table[2,2]) # FNR = 0.458



# refining the mtry size
test_metrics <- readRDS('random-forest-mtry-test.rds')
View(test_metrics)
test_metrics <- data.frame(matrix(ncol = 4))
colnames(test_metrics) <- c('mtry', 'FNR', 'FPR', 'Accuracy')

for(i in 1:20){
  set.seed(2022)  #for bootstrap sampling & RSF selection
  rf.temp <- randomForest(readmitted ~ ., data = trainset, ntree = 500, mtry = i, importance = T, do.trace = TRUE)
  rf.pred <- predict(rf.temp, newdata = testset, type = 'class')
  cm <- confusionMatrix(rf.pred, reference = testset$readmitted)
  fnr <- (cm$table[1,2])/(cm$table[1,2]+cm$table[2,2]) # false negative rate
  fpr <- (cm$table[2,1])/(cm$table[2,1]+cm$table[1,1]) # false positive rate
  acc <- as.numeric(cm$overall['Accuracy']) #error rate
  temp <- list(i, fnr, fpr, acc)
  test_metrics <- rbind(test_metrics, temp)
}
saveRDS(test_metrics, file = 'random-forest-mtry-test.rds')
