library(data.table)
library(neuralnet)
library(fastDummies)


setwd("~/GitHub/bc2407")
trainset <- fread('trainset.csv', stringsAsFactors = T)
trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(trainset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
testset <- fread('testset.csv', stringsAsFactors = T)
testset[,c('admission_type_id', 'admission_source_id', 'readmitted')] <- lapply(testset[,c('admission_type_id', 'admission_source_id', 'readmitted')], factor)
nn_trainset <- trainset
nn_testset <- testset

cols.fac <- c("gender","age","admission_type_id","admission_source_id","A1Cresult",
              "metformin","glipizide","glyburide",
              "pioglitazone","insulin","change","diabetesMed","readmitted")

#create dummy variables
nn_trainset <- dummy_cols(trainset, select_columns = cols.fac, remove_selected_columns = T, remove_first_dummy = T)
nn_testset <- dummy_cols(testset, select_columns = cols.fac, remove_selected_columns = T, remove_first_dummy = T)

count = 10
for (i in 10:18){
  colnames(nn_trainset)[i] <- (paste('age', count, count+10, sep = ''))
  count = count + 10
}

count = 10
for (i in 10:18){
  colnames(nn_testset)[i] <- (paste('age', count, count+10, sep = ''))
  count = count + 10
}

set.seed(2022)
sample_rows = sample(1:nrow(nn_trainset), size=4000)
set.seed(2022)
nnModel <- neuralnet(readmitted_1 ~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = nn_trainset[sample_rows,], hidden = c(1), err.fct = "ce", linear.output = FALSE)

nnModel$startweights   # starting weights used
nnModel$weights        # Final optimised weights

unique(nnModel$net.result)    # predicted outputs. 
nnModel$result.matrix  # summary.

# View final weights Neural Network diagram
plot(nnModel)

# Prediction
nnPred <- predict(nnModel,newdata = testset)
nnPred
# Test set Confusion matrix
nnPredCases <- as.numeric(nnPred>0.5)
t <- table(nnPredCases, testset$readmitted_0)
t

