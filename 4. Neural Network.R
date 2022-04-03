library(data.table)
library(neuralnet)



#Neural Network



set.seed(2022) 

train = sample.split(Y = dt1$readmitted, SplitRatio = 0.9)
trainset = subset(dt1, train == T)
testset = subset(dt1, train == F)

#correct class imbalance in train set -> sampling from majority
trainset <- downSample(trainset[,1:20], trainset$readmitted, yname = 'readmitted')

summary(trainset$readmitted)



cols.fac <- c("gender","age","admission_type_id","admission_source_id","A1Cresult",
              "metformin","glipizide","glyburide",
              "pioglitazone","insulin","change","diabetesMed","readmitted")

#convert to factor
for(col in cols.fac)
  set(trainset, j = col, value = as.factor(trainset[[col]]))

for(col in cols.fac)
  set(testset, j = col, value = as.factor(testset[[col]]))
#create dummy variables
trainset <- dummy_cols(trainset, select_columns = cols.fac,remove_selected_columns = TRUE,remove_first_dummy = T)
testset <- dummy_cols(testset, select_columns = cols.fac,remove_selected_columns = TRUE,remove_first_dummy = T)

trainset[,"readmitted_1"] <- 0
testset[,"readmitted_1"] <- 0
for (i in 1:nrow(trainset)) {
  if (trainset$readmitted_0[i] == 0) {
    trainset$readmitted_1[i] = 1
  }
}

for (i in 1:nrow(testset)) {
  if (testset$readmitted_0[i] == 0) {
    testset$readmitted_1[i] = 1
  }
}

#replacing column names
colnames(trainset)[10] <- "age1020"
colnames(trainset)[11] <- "age2030"
colnames(trainset)[12] <- "age3040"
colnames(trainset)[13] <- "age4050"
colnames(trainset)[14] <- "age5060"
colnames(trainset)[15] <- "age6070"
colnames(trainset)[16] <- "age7080"
colnames(trainset)[17] <- "age8090"
colnames(trainset)[18] <- "age90100"

#replacing column names
colnames(testset)[10] <- "age1020"
colnames(testset)[11] <- "age2030"
colnames(testset)[12] <- "age3040"
colnames(testset)[13] <- "age4050"
colnames(testset)[14] <- "age5060"
colnames(testset)[15] <- "age6070"
colnames(testset)[16] <- "age7080"
colnames(testset)[17] <- "age8090"
colnames(testset)[18] <- "age90100"





#start neuralnet model
set.seed(2022)

sample_rows = sample(1:nrow(trainset), size=10000)

set.seed(2022)

nnModel <- neuralnet(readmitted_1~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[sample_rows,], hidden = c(2,1),act.fct="tanh", linear.output = FALSE,stepmax=1e7)
saveRDS(nnModel, file = 'r-Neural-Network-model.rds')
nnModel <- readRDS('r-Neural-Network-model.rds')


#3 other tests with changes to their hyperparameters
#nnModel <- neuralnet(readmitted_1~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[sample_rows,], hidden = c(2),act.fct="tanh", linear.output = FALSE,stepmax=1e7)
#nnModel <- neuralnet(readmitted_1~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[sample_rows,], hidden = c(1),act.fct="tanh", linear.output = FALSE,stepmax=1e7)
#nnModel <- neuralnet(readmitted_1~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[sample_rows,], hidden = c(1,1),act.fct="tanh", linear.output = FALSE,stepmax=1e7)


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
t_testset <- table(nnPredCases, testset$readmitted_1)
t_testset

# Train set Confusion Matrix
predicted.cases <- ifelse(unlist(nnModel$net.result[[1]][,1]) > 0.5, 1, 0)  
length(predicted.cases)
trainset2 <- trainset[sample_rows,]
t_trainset <- table(predicted.cases,trainset2$readmitted_1)  
confusionMatrix(t_trainset)
