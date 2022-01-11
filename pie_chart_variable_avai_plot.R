rm(list=ls()) #Clear all
cat("\014")
work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'


setwd(work_dir)

loading.dir = paste0(work_dir, '/csv_files')

data_dict = read.csv(paste0(loading.dir,'/y5_all_vars_dictionary_manually_selecting_vars (3).csv'))

data_dict$Percent_available = data_dict$Percent_available_in_y5_exam_subjects
data_dict$Percent_available = ifelse(data_dict$Percent_available >100, 100, data_dict$Percent_available)
data_dict$Percent_available = ifelse(is.na(data_dict$convert.NA.to.0), data_dict$Percent_available, 100)

# count percentage:
percent_less_50 = sum(data_dict$Percent_available<50)/nrow(data_dict)*100
percent_50_70 = sum(data_dict$Percent_available>50 & data_dict$Percent_available<=70)/nrow(data_dict)*100
percent_70_90 = sum(data_dict$Percent_available>70 & data_dict$Percent_available<=90)/nrow(data_dict)*100
percent_90_100 = sum(data_dict$Percent_available>90)/nrow(data_dict)*100
