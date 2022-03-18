library(data.table)
library(randomForest)

dt2 <- fread('dt2-cleaned.csv', stringsAsFactors = T)
sapply(dt2, class)
dt2[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')] <- lapply(dt1[,c('admission_type_id', 'admission_source_id', 'readmitted', 'repeat_patient')], factor)
sapply(dt2, class)