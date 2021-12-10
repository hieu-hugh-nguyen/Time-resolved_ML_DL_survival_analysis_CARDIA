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


feature_space_unimportant_vars_excluded = read.csv(paste0(loading.dir, '/feature_space_unimportant_vars_excluded.csv'),  stringsAsFactors = F)


# filter out unwanted variables:
included_vars = var.dictionary.updated %>% filter(Include...1..exclude...0 == 1) %>% dplyr::select(Variable.Name) %>% 
  as.list() %>% unlist() %>% as.character()

filtered_feature_space = feature_space_unimportant_vars_excluded %>% dplyr::select(included_vars)


# convert NA to zero for the selected variables:
na_to_zero_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 0)]
na_to_one_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$convert.NA.to.0 == 1)]
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

filled_df_zero <- fill_na_func(feature_space_unimportant_vars_excluded, na_to_zero_vars, fill_value = 0)
filled_df_zero_one <- fill_na_func(filled_df_zero, na_to_one_vars, fill_value = 1)
filled_df_zero_one_99 <- fill_na_func(filled_df_zero_one, na_to_99_vars, fill_value = 99)



saving.dir = loading.dir
write.csv(filled_df_zero_one_99, file = paste0(saving.dir,"/y5_feature_space_manually_filled_NA.csv",sep = ""),row.names=FALSE)



# # set categorical data to factor type: 
# 
# names(var.dictionary.updated)[4] = "categorical = 1"
# 
# categorical_vars = var.dictionary.updated$`Variable Name`[which(var.dictionary.updated$`categorical = 1` == 1)]
# categorical_vars_in_featurespace = intersect(categorical_vars, names(data))
# 
# columns_of_interest_cate = which(names(data) %in% categorical_vars)
# 
# 
# for (i in 1:length(categorical_vars_in_featurespace)){ 
#   data[,categorical_vars_in_featurespace[i]] = as.factor(data[,categorical_vars_in_featurespace[i]])
#   
# }
# 
# saving.dir="U:/Hieu/CARDIA_project/csv_files"
# write.csv(data, file = paste0(saving.dir,"/y5_feature_space_na_to_zero_category_as_factor.csv",sep = ""),row.names=FALSE)
