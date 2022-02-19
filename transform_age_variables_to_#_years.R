# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

# load the dataset
loading_dir = paste0(work_dir, '/csv_files')
feature_space = read.csv(paste0(loading_dir,'/y5_feature_space_manually_filled_NA','.csv'), stringsAsFactors = FALSE)


# convert from 'age having...' to 'number of years having...' by subtracting age:
temp_feature_space <- feature_space

age_variables_to_be_converted = c('C08HBPAG','C08CHOAG','C08HRTAG','C08ANGAG',
                  'C08RHDAG','C08MVPAG','C08OTHAG','C08DIBAG',
                  'C08NEPAG','C08OTKAG','C08HEPAG','C08LUNAG',
                  'C08BRSAG','C08BLDAG','C08TESAG','C08BONAG',
                  'C08MELAG','C08SKNAG','C08BRNAG','C08STMAG',
                  'C08OCAAG','C08GALDA','C08PRGAG','C08KYSAG',
                  'C08URNAG','C09SMKAG')

for (i in 1:length(age_variables_to_be_converted)){
  temp_feature_space <- temp_feature_space %>% 
    mutate(!!(paste0(age_variables_to_be_converted[i],'_YEAR')) := ifelse(get(age_variables_to_be_converted[i]) ==99, 0, (EX3_AGE - get(age_variables_to_be_converted[i])))) %>% 
    dplyr::select(-one_of(age_variables_to_be_converted[i]))
}

feature_space_updated_years_having_conditions <- temp_feature_space
feature_space_updated_years_having_conditions[feature_space_updated_years_having_conditions < 0] <- 0

# remove exam 0 age (since already has exam 3 age)
feature_space_updated_years_having_conditions <- within(feature_space_updated_years_having_conditions, rm(EXAMAGE))

write.csv(feature_space_updated_years_having_conditions, file = paste0(work_dir,'/csv_files/feature_space_NA_manually_filled_updated_years_having_conditions.csv')
          ,  row.names=FALSE)

