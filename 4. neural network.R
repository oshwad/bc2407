library(data.table)
library("ROSE")
library(neuralnet)

setwd("~/GitHub/bc2407")
dt1 <- fread('dt1-cleaned.csv', stringsAsFactors = T)
dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
dt2 <- ROSE(readmitted~., data = dt1, N = nrow(dt1), seed=111)$data
table(dt2$readmitted)

#Neural Network
set.seed(2014) 

m1 <- model.matrix(  
  ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + number_diagnoses + gender + age + change + A1Cresult + diabetesMed + insulin + metformin + glipizide + glyburide + pioglitazone + admission_type_id + admission_source_id,  
  data = dt1
)
new_df <- as.data.frame(m1)
lapply(new_df,as.numeric)
new_df['readmitted'] <- dt1$readmitted
new_df$readmitted = dt1$readmitted

dt2 <- new_df %>% group_by() %>% slice_sample(n=1000)


## SPLIT New Dataset into Train & Test Sets ##
train=sample.split(Y=dt2$readmitted,SplitRatio = 0.7)
trainset=subset(dt2,train==T)
testset=subset(dt2,train==F)

## CHECK Distribution of Train & Test Sets ##
summary(trainset$readmitted) 
summary(testset$readmitted)


nn1 <- neuralnet(readmitted ~ age1020 + age2030 + age3040 + age4050 + age5060 + age6070 + age7080 + age8090 + age90100 + num_procedures + time_in_hospital + num_medications + insulinNo + insulinUp + insulinSteady,  data=trainset, hidden=2, err.fct="ce", linear.output=FALSE)


par(mfrow=c(1,1))
plot(nn1)

nn1$net.result  # predicted outputs
nn1$result.matrix  # summary1$startweights
nn1$weights

# The generalized weight is defined as the contribution of the ith input variable to the log-odds:
nn1$generalized.weights
## Easier to view GW as plots instead

par(mfrow=c(2,2))
gwplot(nn1,selected.covariate=1)
gwplot(nn1,selected.covariate=2)
gwplot(nn1,selected.covariate=3)
gwplot(nn1,selected.covariate=4)
gwplot(nn1,selected.covariate=5)
gwplot(nn1,selected.covariate=6)
gwplot(nn1,selected.covariate=7)
gwplot(nn1,selected.covariate=8)
gwplot(nn1,selected.covariate=9)
gwplot(nn1,selected.covariate=10)
gwplot(nn1,selected.covariate=11)


# Display inputs with model outputs
out <- cbind(nn1$covariate, nn1$net.result[[1]])
dimnames(out) <- list(NULL, c("time_in_hospital","num_medications","insulinNo","insulinUp","insulinSteady"))
head(out) 



#Confusion matrix. model accuracy for Neural Network
nn1.results <- compute(nn1, testset)
pred <- nn1.results$net.result
pred2 <- ifelse(pred>0.5, 1, 0)
ConfusionMatrixNN <- table(pred2[,1], testset$readmitted)
ConfusionMatrixNN 

