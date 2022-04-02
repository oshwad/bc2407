library(corrplot)
library(ggcorrplot)
library(ggplot2)

data1 = fread('data-diabetic_data.csv')

#select the variables we will be using
data1 <- subset(data1,select=c("gender","age","change","A1Cresult","diabetesMed","insulin", "metformin", "glipizide", "glyburide", "pioglitazone","admission_type_id","admission_source_id","time_in_hospital","num_lab_procedures","num_procedures", "num_medications","number_outpatient","number_emergency","number_inpatient","number_diagnoses","readmitted"))

#change all categorical to factor data type 
sapply(data1, class) 
data1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted')] <- lapply(data1[,c('gender', 'age', 'change', 'A1Cresult', 'diabetesMed', 'insulin', 'glipizide', 'metformin', "glyburide", "pioglitazone", 'admission_type_id', 'readmitted')], factor) 

colnames(data1)

#Correlation matrix for continuous independent variables
p<-data1[,13:20]
p.mat<-cor_pmat(p)
corr<-round(cor(p),3)
ggcorrplot(corr, hc.order=TRUE, type="lower", outline.col="white",
           ggtheme=ggplot2::theme_gray, colors=c("#6D9EC1", "white", "#E46726"), 
           lab=TRUE, p.mat=p.mat, insig="blank", title="Correlation Matrix")

#time_in_hospital and num_medications are positivelr-related = 0.47
#num_procedures and num_medications are positively correlated = 0.39
#number_inpatient and number_emergency is negatively correlated = -0.07

#Continuous independent variables against readmitted

#time_in_hospital against readmitted
ggplot(data=data1, aes(x=readmitted, y=time_in_hospital)) + geom_boxplot() + coord_flip() + ggtitle("Time in Hospital against Readmitted")

#number_inpatient against readmitted
ggplot(data=data1, aes(x=readmitted, y=number_inpatient)) + geom_boxplot() + coord_flip() + ggtitle("Number of Inpatient Visits against Readmitted")

#number_diagnoses against readmitted
ggplot(data=data1, aes(x=readmitted, y=number_diagnoses)) + geom_boxplot() + coord_flip() + ggtitle("Number of Diagnoses against Readmitted")

#num_medications against readmitted
ggplot(data=data1, aes(x=readmitted, y=num_medications)) + geom_boxplot() + coord_flip() + ggtitle("Number of Medications against Readmitted")
