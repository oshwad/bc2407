library(corrplot)
library(ggcorrplot)

dt1 <- fread('dt1-cleaned.csv')

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

## PLOT Box Plot for time_in_hospital against age##
ggplot(dt1, aes(dt1$age, dt1$time_in_hospital, fill=readmitted))+
  geom_boxplot()+
  labs(title="Box Plot for time_in_hospital across age range", x="age", y="time_in_hospital")+
  guides(fill="none")+
  facet_grid(rows=vars(readmitted))
#When age range increase the time in hospital increases regardless of whether the patient was readmitted or not.

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
