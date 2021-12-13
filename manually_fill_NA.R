# for some vars (usually the seemingly important ones, or age variables), we include in the analysis despite of high missing values. Need to convert NA to zero

rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)


require('tidyverse')
require('readxl')
require('dplyr')

loading.dir = paste0(work_dir,'/csv_files')
var.dictionary.updated = read.csv(paste0(loading.dir, '/y5_all_vars_dictionary_manually_selecting_vars (3).csv'),  stringsAsFactors = F)


feature_space = read.csv(paste0(loading.dir, '/feature_space_unimportant_vars_excluded.csv'),  stringsAsFactors = F)




# convert NA to zero for the selected variables:

na_to_zero_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 0)]
na_to_one_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 1)]
na_to_two_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 2)]
na_to_eight_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 8)]
na_to_99_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 99)]



fill_na_func <- function(data, var_list, fill_value = 0){
  columns_of_interest = which(names(data) %in% var_list)
  
  for (i in 1:length(var_list)){ 
    data[which(is.na(data[,columns_of_interest[i]])),columns_of_interest[i]] = fill_value
    data[which(data[,columns_of_interest[i]] == "NA"),columns_of_interest[i]] = fill_value
  }
  
  #filled_df = data.frame(var_list,unlist(data[1,columns_of_interest]))
  return(data)
}

filled_df_zero <- fill_na_func(feature_space, na_to_zero_vars, fill_value = 0)
filled_df_zero_one <- fill_na_func(filled_df_zero, na_to_one_vars, fill_value = 1)
filled_df_zero_one_two <- fill_na_func(filled_df_zero_one, na_to_two_vars, fill_value = 2)
filled_df_zero_one_two_eight <- fill_na_func(filled_df_zero_one_two, na_to_eight_vars, fill_value = 8)
filled_df_zero_one_two_eight_99 <- fill_na_func(filled_df_zero_one_two_eight, na_to_99_vars, fill_value = 99)



# filter out unwanted variables:
included_vars = var.dictionary.updated %>% filter(Include...1..exclude...0 == 1) %>% dplyr::select(Variable.Name) %>% 
  as.list() %>% unlist() %>% as.character()

filtered_feature_space = filled_df_zero_one_two_eight_99 %>% dplyr::select(included_vars)

saving.dir = loading.dir
write.csv(filtered_feature_space, file = paste0(saving.dir,"/y5_feature_space_manually_filled_NA.csv",sep = ""),row.names=FALSE)

