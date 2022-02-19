# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)
parallelMap::parallelStartSocket(5)


# source snippet functions:
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/running_rsf.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/running_algo.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/createDir.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/subsetDataTopnVar.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/classif_task.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/predictSurvProb.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/eval_performance.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/eval_performance_using_different_auc_package.R'))


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')
feature_space = read.csv(paste0(loading_dir,'/y5_imputed_unsupervised_Dec_2021','.csv'), stringsAsFactors = FALSE)

label_space = read.csv(paste0(loading_dir,'/y5_cvd_outcome','.csv'))
#label_space = read.csv(paste0(loading_dir,'/y5_mortality_outcome','.csv'))
ascvd_data = read.csv(paste0(loading_dir,'/ascvd_calc_with_id','.csv'))
names(ascvd_data)[1] = 'ID'
# convert race and sex to {0,1} type:
ascvd_data$sex = ifelse(ascvd_data$sex == 'Male', 0, 1)
ascvd_data$race = ifelse(ascvd_data$race == 'White', 0, 1)

#Rename labels for name and time to event for better generalization later on:
names(label_space)[[2]] = "event"   
names(label_space)[[3]] = "time"  



feature_space = within(feature_space, rm('SEX'))
feature_space = within(feature_space, rm('RACE'))

data_full = dplyr::inner_join(label_space, ascvd_data, by = 'ID')
data_full = dplyr::inner_join(data_full, feature_space, by = 'ID')

#data_full = merge(label_space, feature_space, by = 'ID')

#rm(feature_space, label_space, ascvd_data)

# Remove (almost duplicated) correlated variables (blood pressure from echo and from anthropometry: 
data_full = within(data_full, rm('C40DBP','C40SBP', 'age', 'ascvd', 'sbp'))
data_full = data_full %>% dplyr::select(-C09SMKAG_YEAR)
# Correct for wrong entry of smoking years: 
data_full$C09SMKYR[which(data_full$C09SMKYR == 88)] = 0


#Check if there is any character column, then delete them to make sure all data is numeric:
nums = unlist(lapply(data_full, is.character))  
data_full[,nums]=NULL

#Exclude time to event <0:
positiveTimetoEvent = data_full$time>=0
data_full = data_full[positiveTimetoEvent,]


# Separate cohort into short term and long term at risk for CVD: ############################

# Convert events happening later than Y10 to censored:
data_y10 = data_full
for (i in 1:nrow(data_full)){
  if (data_full$event[i] == 1 && data_full$time[i] > 365.25*10){
    data_y10$event[i] = 0
    data_y10$time[i] = 365.25 *10.1 
  }
  if (data_full$event[i] == 0 && data_full$time[i] > 365.25*10){
    data_y10$time[i] = 365.25 *10.1
  }
}

# Select those having survived by Y10 and truncate survival origin: 
data_after_y10 = data_full %>% anti_join(data_full %>% filter(event == 1) %>% filter(time < 365.25*10), by = 'ID') 
data_after_y10_origin_at_10 = data_after_y10 %>% mutate(time = time - 365.25*10) %>% filter(time >=0)











### START THE OUTER LOOP:

# include training patients only, exclude testing patients:
loading_dir = paste0(work_dir, '/rdata_files')
trainingid_all = get(load(file = paste0(loading_dir,
                                        '/all_training_ID_outerloop_cohort_10_26.RData'
)))
start_time = Sys.time()
seed = 4495
set.seed(seed)
nfolds = 25
endpt_yr = 10


for (fold in 1:25){
  #fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'rsf_all_vars_long_term_permute'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  
  rf_permute = rfsrc(Surv(time, event)~., data = data, ntree = 1001
                               ,importance = 'permute', verbose = TRUE
  )


  saving_dir = file.path(main_dir, sub_dir)
  save(rf_permute, file = paste0(saving_dir,'/', model_name, '.RData'))

  permute_importance = rf_permute$importance
  permute_importance.df = data.frame(var.name = names(permute_importance), importance = permute_importance)

  permute_importance2.df = permute_importance.df[order(permute_importance.df[,2], decreasing = T), ]
  save(permute_importance2.df, file = paste0(saving_dir,'/', model_name, '.RData'))
  #permute_importance2.df
}
  





## Averaging VIMP over folds: ######################

fold = 1
permute_importance_df  <- get(load( file = paste0(work_dir,'/rdata_files/rsf_all_vars_long_term_permute_fold_1/',model_name,'.RData')))  %>% 
  rename(Variable = var.name, vimp_fold = importance)

for (fold in 2:25){
  loading_dir <- paste0(work_dir, '/rdata_files/rsf_all_vars_long_term_permute_fold_', fold)
  permute_fold <- get(load(paste0(loading_dir, '/rsf_all_vars_long_term_permute.RData')))
  permute_importance_df <- permute_importance_df %>% 
    inner_join(permute_fold %>% rename(Variable = var.name, vimp_fold = importance), by = 'Variable')
  
}


# Add var description:
var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))

get.var.des = function(x){
  var.des = var.dictionary.updated$`Variable Label`[which(var.dictionary.updated$`Variable Name` == x[1])]
  return(var.des)
}
vardes = unlist(apply(permute_importance_df, 1, FUN = get.var.des))
require('tibble')
permute_importance_df = add_column(permute_importance_df, VariableDescription = vardes, .after = 1)


  
permute_importance_df <- permute_importance_df %>% dplyr::select(Variable, VariableDescription, everything())  
permute_importance_df$'Avg_Vimp' = rowMeans(permute_importance_df[,3:ncol(permute_importance_df)])  

permute.ranking <- permute_importance_df %>% arrange(desc(abs(Avg_Vimp)))

normalized.permute.ranking <- permute.ranking %>% rename(importance = Avg_Vimp) %>% dplyr::select(Variable, VariableDescription, importance) %>% 
  mutate(normalized_vimp = (abs(importance)-min(abs(importance)))/(max(abs(importance))-min(abs(importance)))) %>%
  arrange(desc(normalized_vimp))
saving_dir = paste0(work_dir, '/csv_files')
write.csv(normalized.permute.ranking, file = paste0(saving_dir,'/averaged_outerloop_vimp_permute_long_term_normalized.csv'), row.names = F)


  
# # plot deph vs vimp:
# 
# mininmal_vimp_object <- mindepth.ranking %>% mutate(depth = index) %>% select(Variable, VariableDescription, depth) %>%
#   left_join(permute.ranking %>% mutate(vimp = row_number()) %>% select(Variable, vimp)
#             , by = 'Variable') %>% arrange(depth) %>% mutate(names = VariableDescription) %>% mutate(col = 'NA') %>% select(names, vimp, col, depth) 
# class(mininmal_vimp_object) <- c("gg_minimal_vimp", "data.frame") 
# attributes(mininmal_vimp_object)$modelsize <- 168
# 
# plot(mininmal_vimp_object %>% slice(1:19))
# 
