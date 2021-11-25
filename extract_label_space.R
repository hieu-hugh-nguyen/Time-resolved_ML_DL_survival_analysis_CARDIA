rm(list=ls()) #Clear all
cat("\014")


library(haven)
library(tibble)
library(Hmisc)
library(labelled)
library(DataCombine)
library(lubridate)
library('foreign')


work_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

#Read SAS outcome files:
loading.dir="U:/CARDIA Other/CARDIACCdata/M & M/MM18/data";
outcome18 = haven::read_sas(paste0(loading.dir,'/outcomes18.sas7bdat'))
saving.dir = paste0(work_dir, "/csv_files")
write.csv(outcome18, file = paste0(saving.dir,"/all_m&m_information.csv"),row.names = F)

# extract mortality and cvd labels since y5, exclude events happened prior to y5: 
death.df = data.frame(ID =outcome18$ID, death.status = outcome18$dead18, death.days.since.y5 = outcome18$dead18ctt)
death.y5.df = death.df[which(death.df$death.days.since.y5>0),]
write.csv(death.y5.df, file = paste0(saving.dir,"/y5_mortality_outcome.csv"),row.names = F)


cvd.df = data.frame(ID =outcome18$ID, cvd.status = outcome18$cvda, cvd.days.since.y5 = outcome18$cvdafnfctt)
cvd.y5.df = cvd.df[which(cvd.df$cvd.days.since.y5>0),]
write.csv(cvd.y5.df, file = paste0(saving.dir,"/y5_cvd_outcome.csv"),row.names = F)


