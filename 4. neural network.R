library(data.table)

library(neuralnet)


#Neural Network
set.seed(2014) 

cols.fac <- c("gender","age","admission_type_id","admission_source_id","A1Cresult",
              "metformin","glipizide","glyburide",
              "pioglitazone","insulin","change","diabetesMed","readmitted")

#convert to factor
for(col in cols.fac)
  set(dt1, j = col, value = as.factor(dt1[[col]]))

#create dummy variables
dt1 <- dummy_cols(dt1, select_columns = cols.fac,remove_selected_columns = TRUE,remove_first_dummy=TRUE)


# replacing column names
colnames(new_df)[11] <- "age1020"
colnames(new_df)[12] <- "age2030"
colnames(new_df)[13] <- "age3040"
colnames(new_df)[14] <- "age4050"
colnames(new_df)[15] <- "age5060"
colnames(new_df)[16] <- "age6070"
colnames(new_df)[17] <- "age7080"
colnames(new_df)[18] <- "age8090"
colnames(new_df)[19] <- "age90100"
colnames(new_df)[21] <- "A1Cresultmorethan8"
colnames(new_df)[1] <- "Intercept"


#start neuralnet model
set.seed(200)
nndt <- subset(dt1,select=c("num_procedures", "time_in_hospital", "num_medications", "insulin_No","insulin_Up", "insulin_Steady" ,"readmitted_0"))


train <- sample.split(Y = nndt$readmitted_0, SplitRatio = 0.7)
trainset <- subset(nndt, train == T)
testset <- subset(nndt, train == F)
x_train <- subset(subset(nndt, select = -c(readmitted_0)), train == T)
x_test <- subset(subset(nndt, select = -c(readmitted_0)), train == F)
y_train <- subset(subset(nndt, select = c(readmitted_0)), train == T)
y_test <- subset(subset(nndt, select = c(readmitted_0)), train == F)

trainSize<-500

nnModel <- neuralnet(readmitted_0~ ., data = trainset[1:trainSize], hidden = c(2), err.fct = "ce", linear.output = FALSE)

nnModel$startweights   # starting weights used
nnModel$weights        # Final optimised weights

unique(nnModel$net.result)    # predicted outputs. 
nnModel$result.matrix  # summary.

# View final weights Neural Network diagram
plot(nnModel)

# Prediction
nnPred <- predict(nnModel,x_test)
nnPred
# Test set Confusion matrix
nnPredCases <- as.numeric(nnPred>0.5)
t <- table(nnPredCases,y_test$readmitted_0)
confusionMatrix(t)

# Train set Confusion Matrix
predicted.cases <- ifelse(unlist(nnModel$net.result[[1]][,2]) > 0.5, 1, 0)  
length(predicted.cases)
t <- table(predicted.cases,y_test$readmitted_Yes[1:trainSize])  
confusionMatrix(t)
