library(data.table)
library(dplyr)
filename <- "/Users/katiito/covid_data/CCPUKSARI_DATA_2020-06-16_1019.csv"
cocindata_all <- fread(file=filename)


# select variables of interest
cocindata <- cocindata_all %>%
  select(subjid, "onset_date" = cestdat, "adm_date" = hostdat, 
         readm_cov19, "transfer" = hooccur,
       "transfer_date" = hostdat_transfer, "cov_diag" = corna_mbcat, 
       "cov_test_outcome" = mborres, "test_date" = mbdat, 
       "outcome" = dsterm, "hosp_outcome" = dshosp, "outcome_date" = dsstdtc) %>%
  mutate(onset_date=as.numeric(as.Date(onset_date, format = "%Y-%m-%d"))) %>%
  mutate(adm_date=as.numeric(as.Date(adm_date, format = "%Y-%m-%d"))) %>%
  mutate(transfer_date=as.numeric(as.Date(transfer_date, format = "%Y-%m-%d"))) %>%
  mutate(test_date=as.numeric(as.Date(test_date, format = "%Y-%m-%d"))) %>%
  mutate(outcome_date=as.numeric(as.Date(outcome_date, format = "%Y-%m-%d"))) 
  
cocindata_collapse <- 
  cocindata %>% group_by(subjid) %>% summarise_all(funs(if(all(is.na(.))) NA else sum(., na.rm = TRUE)))

cocindata_test <- cocindata_all %>%
  select(subjid,  "transfer_name" = siteid)
  
cocin_facility <- 
  cocindata_test %>% group_by(subjid) %>% summarise_all(funs(paste(., collapse="")))
