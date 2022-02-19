# outer loop 

rm(list=ls()) #Clear all
cat("\014")

library(dplyr)

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)
vimp = read.csv(paste0(work_dir, '/csv_files/averaged_outerloop_VIMP_all_years_rerun.csv'))
vimp_var = vimp %>% dplyr::select(Variable)

#var_from_dict = read.csv(paste0(work_dir, '/csv_files/y5_all_vars_dictionary_manually_selecting_vars (3).csv'))
var_from_dict = read.csv(paste0(work_dir, '/csv_files/y5_final_vars_list.csv'))
selected_var = var_from_dict %>% dplyr::filter(Include...1..exclude...0 == 1) %>%
  rename(Variable = Variable.Name) #%>%
 # dplyr::select(Variable)

anti_var = selected_var %>% dplyr::anti_join(vimp, by = 'Variable')

anti_var2 = vimp %>% dplyr::anti_join(selected_var, by = 'Variable')