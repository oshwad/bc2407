library(data.table)
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("ggcorrplot")
library(ggcorrplot)
library(neuralnet)
library(caTools)
library(randomForest)


setwd("C:/Users/jieka/Desktop/BC2407 Analytics II/project")
DM.dt <- fread("Diabetic_data.csv",na.strings = c("NA", "missing","MISSING", "N/A", -99, "", "m", "M", "na", "."))



#identify the top 5 most used meds (least number of NOs)
#subset the meds and convert all to factor
dt.meds <- DM.dt[,c(25:47) := lapply(.SD, as.factor), .SDcols = c(25:47)]
med1 <- dt.meds[,c(25:47)]
med2 <- data.table()
#count the number of NOs for each med
for(i in colnames(med1)) {
  med2 <- rbind(med2,matrix(c(i,sum(med1[[i]] == "No")),1,2))
}
#sort in ascending order and pick the top 5
head(med2[order(as.numeric(V2)),],5)
#5 most used meds: insulin, metformin, glipizide, glyburide, pioglitazone

#select the variables we will be using
dt1 <- subset(DM.dt,select=c("patient_nbr", "gender","age","change","A1Cresult","diabetesMed","insulin", "metformin", "glipizide", "glyburide", "pioglitazone","admission_type_id","admission_source_id","time_in_hospital","num_lab_procedures","num_procedures", "num_medications","number_outpatient","number_emergency","number_inpatient","number_diagnoses","readmitted"))


length(unique(dt1$patient_nbr)) #71518 unique patients out of the 101766 rows 

#create a new variable for repeated patients as another x variable
dt1[, repeat_patient := .N > 1, by = patient_nbr]
dt1[,repeat_patient := ifelse(repeat_patient=="TRUE", 1, 0)]


#change all categorical to factor data type 
sapply(dt1, class) 
dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted', 'repeat_patient')], factor) 

#rename 'readmitted' levels: combine '>30' and 'NO' into '0', '<30' into '1'
levels(dt1$readmitted)
levels(dt1$readmitted) <- c('1', '0', '0')



#remove the rows with unknown/invalid gender
i<-1
while (i <= nrow(dt1)) { 
  if (dt1$gender[i] == 'Unknown/Invalid') { 
    dt1 <- dt1[-c(i)] 
    i <- i-1
  } 
  i<-i+1
  
} 
dt1$gender <- droplevels(dt1$gender) 
summary(dt1$gender) 

#checking for any na or missing values in the data
summary(dt1)


#for admission_type_id: remove the rows with either not available, null or not mapped
#5: not available, 6: null, 8: not mapped
dt1=subset(dt1, admission_type_id != 5 & admission_type_id != 6 & admission_type_id != 8)
dt1$admission_type_id <- droplevels(dt1$admission_type_id) 


#for admission_source_id
sum(is.na(dt1$admission_source_id))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$admission_source_id[i] < 1 | dt1$admission_source_id[i] > 25) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$admission_source_id[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  i <- i + 1
}

dt1=subset(dt1, admission_source_id != 9 & admission_source_id != 15 & admission_source_id != 17 & admission_source_id != 20 & admission_source_id != 21)
sapply(dt1, class) 


dt1[,c('admission_source_id')] <- lapply(dt1[,c('admission_source_id')], factor) 
dt1$admission_source_id <- droplevels(dt1$admission_source_id) 
class(dt1$admission_source_id)

#cleaning the continuous x variables

class(dt1$time_in_hospital)#integer
sum(is.na(dt1$time_in_hospital))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$time_in_hospital[i] < 1 | dt1$time_in_hospital[i] > 14) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$time_in_hospital[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  i <- i + 1
  
}


class(dt1$num_lab_procedures)#integer
sum(is.na(dt1$num_lab_procedures))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$num_lab_procedures[i] < 1 | dt1$num_lab_procedures[i] >132) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$num_lab_procedures[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  i <- i + 1
  
}


class(dt1$num_procedures)#integer
sum(is.na(dt1$num_procedures))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$num_procedures[i] < 0 | dt1$num_procedures[i] > 6) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$num_procedures[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1
  
}

class(dt1$num_medications)#integer
sum(is.na(dt1$num_medications))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$num_medications[i] < 1 | dt1$num_medications[i] > 81) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$num_medications[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1
  
}

class(dt1$number_outpatient)#integer
sum(is.na(dt1$number_outpatient))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$number_outpatient[i] < 0 | dt1$number_outpatient[i] > 42) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$number_outpatient[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1
  
}


class(dt1$number_emergency)#integer
sum(is.na(dt1$number_emergency))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$number_emergency[i] < 0 | dt1$number_emergency[i] > 76) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$number_emergency[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1
  
}


class(dt1$number_inpatient)#integer
sum(is.na(dt1$number_inpatient))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$number_inpatient[i] < 0 | dt1$number_inpatient[i] > 21) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$number_inpatient[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1  
}


class(dt1$number_diagnoses)#integer
sum(is.na(dt1$number_diagnoses))
i<-1
while (i <= nrow(dt1)) {
  if (dt1$number_diagnoses[i] < 1 | dt1$number_diagnoses[i] > 16) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }
  if (!is.numeric(dt1$number_diagnoses[i])) {
    dt1 <- dt1[-c(i)]
    i <- i - 1
  }    
  i <- i + 1
  
}

# replacing first column name
colnames(dt1)[11] <- "age1020"
colnames(dt1)[12] <- "age2030"
colnames(dt1)[13] <- "age3040"
colnames(dt1)[14] <- "age4050"
colnames(dt1)[15] <- "age5060"
colnames(dt1)[16] <- "age6070"
colnames(dt1)[17] <- "age7080"
colnames(dt1)[18] <- "age8090"
colnames(dt1)[19] <- "age90100"

#Resolve issue of class imbalance
install.packages("ROSE")
library("ROSE")
dt1 <- ROSE(readmitted~., data = dt1, N = nrow(dt1), seed=111)$data
table(dt1$readmitted)

#change all categorical to factor data type 
sapply(dt1, class) 
dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted', 'repeat_patient')], factor) 



#Correlation matrix for continuous independent variables

p<-dt1[,14:21]
p.mat<-cor_pmat(p)
corr<-round(cor(p),3)
ggcorrplot(corr, hc.order=TRUE, type="lower", outline.col="white",
           ggtheme=ggplot2::theme_gray, colors=c("#6D9EC1", "white", "#E46726"), 
           lab=TRUE, p.mat=p.mat, insig="blank", title="Correlation Matrix")

#time_in_hospital and num_medications are positivelr-related = 0.47
#num_procedures and num_medications are positively correlated = 0.39
#number_inpatient and number_emergency is negatively correlated = -0.07



#Data Exploration

p <- ggplot(dt1, aes(x=number_inpatient, y=num_procedures)) +
  geom_point() +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
# Add density distribution as marginal plot
library("ggExtra")
ggMarginal(p, type = "density")
# Change marginal plot type
ggMarginal(p, type = "boxplot")


## PLOT Box Plot for time_in_hospital against age##
ggplot(dt1, aes(dt1$age, dt1$time_in_hospital, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for time_in_hospital across age range", x="age", y="time_in_hospital")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))
#When age range increase the time in hospital increases regardless of whether the patient was readmitted or not.

## PLOT GeomSmooth for time_in_hospital against num_medications ##
ggplot(dt1, aes(x=time_in_hospital, y=num_medications)) +
  geom_smooth()

ggplot(dt1, aes(x=number_inpatient, y=num_procedures)) +
  geom_point()

## PLOT Box Plot for num_medications against age ##
ggplot(dt1, aes(dt1$age, dt1$num_medications, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for num_medications across age range", x="age", y="num_medication")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))
#The num_medications increases from [0-10) to [60-70) before it reduces slightly from age 70 onwards. The num_medication peaks at the age range of [60-70). This is consistent regardless whether the patient was readmitted or not.

## PLOT Box Plot for time_in_hospital against gender ##
ggplot(dt1, aes(dt1$gender, dt1$time_in_hospital, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for time_in_hospital with gender", x="gender", y="time_in_hospital")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))

## PLOT Box Plot for number_diagnoses against age ##
ggplot(dt1, aes(dt1$age, dt1$number_diagnoses, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for number_diagnoses across age range", x="age", y="number_diagnoses")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))
#the num_diagnoses increases from age 0 to age 40 and then increases at a slower rate from age 40 onwards. (only median increase from age 40 onwards). Similare trend regardless whether they were readmitted anot.


## PLOT Box Plot for number_diagnoses against diabetesMed ##
ggplot(dt1, aes(dt1$diabetesMed, dt1$number_diagnoses, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for number_diagnoses with diabetesMed", x="diabetesMed", y="number_diagnoses")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))


## PLOT Bar Graph of time_in_hospital ##
ggplot(dt1, aes(x=time_in_hospital, fill=readmitted))+
  geom_bar(position="dodge")+ 
  labs(title="Bar Graph of time_in_hospital")+
  guides(fill ="none")+
  facet_grid(rows=vars(readmitted))+ 
  geom_text(stat='count',aes(label=after_stat(count)), vjust=0.5)
#same trend in their time spent in hospital regardless of whether patient is readmitted or not.

## PLOT Bar Graph of num_lab_procedures ##
ggplot(dt1, aes(x=num_lab_procedures, fill=readmitted))+
  geom_bar(position="dodge")+ 
  labs(title="Bar Graph of num_lab_procedures")+
  guides(fill ="none")+
  facet_grid(rows=vars(readmitted))
#same trend in num_lab_procedures regardless of whether patient is readmitted or not.

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

# replacing first column name
colnames(dt2)[11] <- "age1020"
colnames(dt2)[12] <- "age2030"
colnames(dt2)[13] <- "age3040"
colnames(dt2)[14] <- "age4050"
colnames(dt2)[15] <- "age5060"
colnames(dt2)[16] <- "age6070"
colnames(dt2)[17] <- "age7080"
colnames(dt2)[18] <- "age8090"
colnames(dt2)[19] <- "age90100"

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





##Random Forest##

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

                      
                    