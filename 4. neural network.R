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
trainset <- dummy_cols(trainset, select_columns = cols.fac,remove_selected_columns = TRUE)
testset <- dummy_cols(testset, select_columns = cols.fac,remove_selected_columns = TRUE)



#replacing column names
colnames(trainset)[11] <- "age1020"
colnames(trainset)[12] <- "age2030"
colnames(trainset)[13] <- "age3040"
colnames(trainset)[14] <- "age4050"
colnames(trainset)[15] <- "age5060"
colnames(trainset)[16] <- "age6070"
colnames(trainset)[17] <- "age7080"
colnames(trainset)[18] <- "age8090"
colnames(trainset)[19] <- "age90100"

#replacing column names
colnames(testset)[11] <- "age1020"
colnames(testset)[12] <- "age2030"
colnames(testset)[13] <- "age3040"
colnames(testset)[14] <- "age4050"
colnames(testset)[15] <- "age5060"
colnames(testset)[16] <- "age6070"
colnames(testset)[17] <- "age7080"
colnames(testset)[18] <- "age8090"
colnames(testset)[19] <- "age90100"


#start neuralnet model
set.seed(2022)
# nndt <- subset(trainset,select=c("age1020","age2030","age3040","age4050","age5060","age6070","age7080","age8090","age90100","diabetesMed_Yes", "insulin_No","insulin_Steady","insulin_Up","metformin_No","metformin_Steady","metformin_Up","time_in_hospital","num_procedures","num_medications","number_emergency","number_inpatient","number_diagnoses" ,"readmitted_0"))
# 
# testset1 <- subset(testset,select=c("age1020","age2030","age3040","age4050","age5060","age6070","age7080","age8090","age90100","diabetesMed_Yes", "insulin_No","insulin_Steady","insulin_Up","metformin_No","metformin_Steady","metformin_Up","time_in_hospital","num_procedures","num_medications","number_emergency","number_inpatient","number_diagnoses" ,"readmitted_0"))




# # train <- sample.split(Y = nndt$readmitted_0, SplitRatio = 0.7)
# # trainset <- subset(nndt, train == T)
# # testset <- subset(nndt, train == F)
# # x_train <- subset(subset(nndt, select = -c(readmitted_0)), train == T)
# x_test <- subset(subset(nndt, select = -c(readmitted_0)), train == F)
# # y_train <- subset(subset(nndt, select = c(readmitted_0)), train == T)
# y_test <- subset(subset(nndt, select = c(readmitted_0)), train == F)

trainSize<-4000

nnModel <- neuralnet(readmitted_0~ age1020+age2030+age3040+age4050+age5060+age6070+age7080+age8090+age90100+diabetesMed_Yes+insulin_No+insulin_Steady+insulin_Up+metformin_No+metformin_Steady+metformin_Up+time_in_hospital+num_procedures+num_medications+number_emergency+number_inpatient+number_diagnoses, data = trainset[1:trainSize,], hidden = c(2), err.fct = "ce", linear.output = FALSE)

#nnModel <- neuralnet(readmitted_0~ ., data = trainset[1:trainSize], hidden = c(2,1), err.fct = "ce", linear.output = FALSE)

#nnModel <- neuralnet(readmitted_0~ ., data = trainset[1:trainSize], hidden = c(1,1), err.fct = "ce", linear.output = FALSE)

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

