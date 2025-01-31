library(data.table)
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("ggcorrplot")
library(ggcorrplot)
library(neuralnet)
library(caTools)
library(randomForest)
library(caret)
install.packages("fastDummies")
library(fastDummies)

setwd("~/GitHub/bc2407")
DM.dt <- fread("data-diabetic_data.csv",na.strings = c("NA", "missing","MISSING", "N/A", -99, "", "m", "M", "na", "."))


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
dt1 <- subset(DM.dt,select=c("gender","age","change","A1Cresult","diabetesMed","insulin", "metformin", "glipizide", "glyburide", "pioglitazone","admission_type_id","admission_source_id","time_in_hospital","num_lab_procedures","num_procedures", "num_medications","number_outpatient","number_emergency","number_inpatient","number_diagnoses","readmitted"))


#change all categorical to factor data type 
sapply(dt1, class) 
dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted')] <- lapply(dt1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted')], factor) 

#rename 'readmitted' levels: combine '>30' and 'NO' into '0', '<30' into '1'
levels(dt1$readmitted)
levels(dt1$readmitted) <- c('1', '0', '0')



#remove the rows with unknown/invalid gender
dt1<-dt1[!(gender=="Unknown/Invalid")]
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

#Normalise continuous variables
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
dt1[,13:20] <- as.data.table(lapply(dt1[,13:20], min_max_norm))

#create train test split
set.seed(2022)
train = sample.split(Y = dt1$readmitted, SplitRatio = 0.9)
trainset = subset(dt1, train == T)
testset = subset(dt1, train == F)

#correct class imbalance in train set -> sampling from majority
set.seed(2022)
ds <- downSample(trainset[,1:20], trainset$readmitted, yname = 'readmitted')
trainset <- ds

#export dt1, testset, and trainset
fwrite(dt1, file = 'data-dt1-cleaned.csv')
fwrite(trainset, file = 'data-trainset.csv')
fwrite(testset, file = 'data-testset.csv')


