rm(list=ls()) #Clear all
cat("\014")
work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'


setwd(work_dir)

loading.dir = paste0(work_dir, '/csv_files')

label_space = read.csv(paste0(loading.dir,'/y5_cvd_outcome','.csv'))

feature_space = read.csv(paste0(loading.dir,'/feature_space_NA_manually_filled_updated_years_having_conditions','.csv'))

death_space = read.csv(paste0(loading.dir,'/y5_mortality_outcome','.csv'))

# exclude those who died prior to y5 from the analysis:
feature_space.death.exclude = inner_join(death_space, feature_space, by = 'ID')

# exclude those who had developed cvd prior to y5:
feature_space.event.exclude = inner_join(label_space,feature_space.death.exclude, by = 'ID')


# now delete the death-related and event-related columns: 
data = feature_space.event.exclude[-c(2,3,4,5)]

# colnames(data)[2] = 'event'
# colnames(data)[3] = 'time'

# remove VLDL variable:
data = data %>% dplyr::select(-one_of('CL1VLDL'))



###### IMPUTATION:



require('readxl')
loading.dir = paste0(work_dir, '/csv_files')
var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))
names(var.dictionary.updated)[4] = "categorical = 0"




# Exclude subjects with missing data >25% of variables:
filter_subjects_data = data %>% mutate(count_na = rowSums(is.na(data))/ncol(data)) %>% filter(count_na < 0.25) 
filter_subjects_data1 = filter_subjects_data %>% dplyr::select(-one_of(c('count_na')))





temp_data <- filter_subjects_data

cat_variables_to_be_reencoded = var.dictionary.updated$Variable.Name[var.dictionary.updated$convert.NA.to.0 %in% c(1,8)]
cat_variables_to_be_reencoded = cat_variables_to_be_reencoded[!cat_variables_to_be_reencoded %in% c('C03INCOM', 'C03DEGRE')]
cat_variables_to_be_reencoded = cat_variables_to_be_reencoded[cat_variables_to_be_reencoded %in% names(filter_subjects_data1)]

for (i in 1:length(cat_variables_to_be_reencoded)){
  # temp_data <- temp_data %>% 
  #   mutate(!!(paste0(age_variables_to_be_converted[i],'_YEAR')) := ifelse(get(age_variables_to_be_converted[i]) ==99, 0, (EX3_AGE - get(age_variables_to_be_converted[i])))) %>% 
  #   dplyr::select(-one_of(age_variables_to_be_converted[i]))
  temp_data[,cat_variables_to_be_reencoded[i]] = ifelse(temp_data[,cat_variables_to_be_reencoded[i]] >=8, 0, temp_data[,cat_variables_to_be_reencoded[i]])
  
  temp_data[,cat_variables_to_be_reencoded[i]] = ifelse(temp_data[,cat_variables_to_be_reencoded[i]] < 2, 0, 1)
}

data_pre_impute_reencode_cat_var_with_id <- temp_data


# save pre-impute data:
saving.dir = loading.dir
write.csv(data_pre_impute_reencode_cat_var_with_id, file = paste0(saving.dir,'/y5_feature_space_pre_impute.csv'), row.names = F)



# set categorical data to factor type: 
categorical_vars = var.dictionary.updated$`Variable.Name`[which(var.dictionary.updated$`categorical = 0` == 0)]
categorical_vars_in_featurespace = intersect(categorical_vars, names(data))

#columns_of_interest_cate = which(names(data) %in% categorical_vars)

data_cat_var_as_factor_with_id = data_pre_impute_reencode_cat_var_with_id
for (i in 1:length(categorical_vars_in_featurespace)){ 
  data_cat_var_as_factor_with_id[,categorical_vars_in_featurespace[i]] = as.factor(data_cat_var_as_factor_with_id[,categorical_vars_in_featurespace[i]])
  
}




# Impute:
library('randomForestSRC')



#imp.data.unsup = impute(data = data[-c(1,2)], nsplit = 10, nimpute = 5, nodesize = 1)
imp.data.unsup = randomForestSRC::impute(data =data_cat_var_as_factor_with_id %>% dplyr::select(-one_of(c('ID'))), nimpute = 5, nsplit = 10, nodesize = 10)

# add id column back to the imputed feature space: 
imp.data.unsup_with_id = cbind(ID = data_cat_var_as_factor_with_id$ID, imp.data.unsup)

saving.dir = loading.dir
write.csv(imp.data.unsup_with_id, file = paste0(saving.dir,'/y5_imputed_unsupervised_Dec_2021.csv'), row.names = F)
