# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
#work_dir= 'C:/Users/HIEU/Desktop/CARDIA project/Git'
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

# load the dataset
#loading_dir = 'C:/Users/HIEU/Desktop/CARDIA project/CARDIA_project/csv_files'
loading_dir = paste0(work_dir, '/csv_files')
feature_space = read.csv(paste0(loading_dir,'/fullfeaturespace','.csv'), stringsAsFactors = FALSE)

# fix all age variables:
# convert all age at zero (didn't have the condition so field left blank)
age_variables = c('C08HBPAG','C08CHOAG','C08HRTAG','C08ANGAG',
                  'C08RHDAG','C08MVPAG','C08OTHAG','C08DIBAG',
                  'C08NEPAG','C08OTKAG','C08HEPAG','C08LUNAG',
                  'C08BRSAG','C08BLDAG','C08TESAG','C08BONAG',
                  'C08MELAG','C08SKNAG','C08BRNAG','C08STMAG',
                  'C08OCAAG','C08GALDA','C08PRGAG','C08KYSAG','C08URNAG','C09SMKAG',
                  'C11FHAGE','C11MHAGE')


curr_feature_space <- feature_space
for (i in 1:length(age_variables)){
  curr_feature_space <- curr_feature_space %>% 
    mutate(!!age_variables[i] := ifelse(get(age_variables[i]) ==0, 99, get(age_variables[i]))) 
  
}  
feature_space_updated_age <- curr_feature_space

write.csv(feature_space_updated_age, file = paste0(work_dir,'/csv_files/feature_space_updated_age_variables.csv')
          ,  row.names=FALSE)

# convert from 'age having...' to 'number of years having...' by subtracting age:
temp_feature_space <- feature_space_updated_age

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

feature_space_updated_years_having_conditions <- within(feature_space_updated_years_having_conditions, rm(EXAMAGE))

write.csv(feature_space_updated_years_having_conditions, file = paste0(work_dir,'/csv_files/feature_space_updated_years_having_conditions.csv')
          ,  row.names=FALSE)


feature_space_fix_incorrect_cat_var <- feature_space_updated_years_having_conditions %>% 
  mutate(C08HRTAK = ifelse(C08HRTAK <2, 0, 1)) %>%
  mutate(C08NEP = ifelse(C08NEP <2, 0, 1)) %>%
  mutate(C08KYS = ifelse(C08KYS <2, 0, 1)) %>% 
  mutate(C08OTHKY = ifelse(C08OTHKY <2, 0, 1)) %>%
  mutate(C08URINE = ifelse(C08URINE <2, 0, 1))
  

write.csv(feature_space_fix_incorrect_cat_var, file = paste0(work_dir,'/csv_files/feature_space_fix_incorrect_cat_var.csv')
          ,  row.names=FALSE)
