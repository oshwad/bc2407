library(data.table)
library(neuralnet)
library(fastDummies)
library(caret)

setwd("~/GitHub/bc2407")
trainset <- fread('data-trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset <- fread('data-testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)

#create dummy variables
trainset <- dummy_cols(trainset, select_columns = cols.fac,remove_selected_columns = TRUE,remove_first_dummy = T)
testset <- dummy_cols(testset, select_columns = cols.fac,remove_selected_columns = TRUE,remove_first_dummy = T)

#replacing column names
count = 10
for (i in 10:18){
  colnames(trainset)[i] <- (paste('age', count, count+10, sep = ''))
  count = count + 10
}
count = 10
for (i in 10:18){
  colnames(testset)[i] <- (paste('age', count, count+10, sep = ''))
  count = count + 10
}

#start neuralnet model
set.seed(2022)
sample_rows = sample(1:nrow(trainset), size=10000)

set.seed(2022)
nnModel <- neuralnet(readmitted_1~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[sample_rows,], hidden = c(2,1),act.fct="tanh", linear.output = FALSE,stepmax=1e7)

saveRDS(nnModel, file = 'r-neural-network-model.rds')
nnModel <- readRDS('r-neural-network-model.rds')

nnModel$startweights   # starting weights used
nnModel$weights        # Final optimised weights

unique(nnModel$net.result)    # predicted outputs
nnModel$result.matrix  # summary

# View final weights Neural Network diagram
plot(nnModel)

# Prediction
nnPred <- predict(nnModel,newdata = testset)
nnPred
# Test set Confusion matrix
nnPredCases <- as.numeric(nnPred>0.5)
t_testset <- table(nnPredCases, testset$readmitted_1)
t_testset

# Train set Confusion Matrix
predicted.cases <- ifelse(unlist(nnModel$net.result[[1]][,1]) > 0.5, 1, 0)  
length(predicted.cases)
trainset2 <- trainset[sample_rows,]
t_trainset <- table(predicted.cases,trainset2$readmitted_1)  
confusionMatrix(t_trainset)
