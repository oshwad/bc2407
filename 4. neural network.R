library(data.table)
library("ROSE")
library(neuralnet)

setwd("~/GitHub/bc2407")
dt1 <- fread('dt1-cleaned.csv', stringsAsFactors = T)
dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
dt2 <- ROSE(readmitted~., data = dt1, N = nrow(dt1), seed=111)$data
table(dt2$readmitted)
