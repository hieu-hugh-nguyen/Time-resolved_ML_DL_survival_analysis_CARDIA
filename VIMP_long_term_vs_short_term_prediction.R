# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
#work_dir= 'C:/Users/HIEU/Desktop/CARDIA project/Git'
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC', 'survAUC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

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
# feature_space = read.csv(paste0(loading_dir,'/feature_space_updated_years_having_conditions','.csv'), stringsAsFactors = FALSE)
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



#feature_space = within(feature_space, rm('SEX'))
#feature_space = within(feature_space, rm('RACE'))
# data_full = dplyr::inner_join(label_space, ascvd_data, by = 'ID')
# data_full = dplyr::inner_join(data_full, feature_space, by = 'ID')

# data_full = dplyr::inner_join(label_space, ascvd_data %>% dplyr::select(c('ID','ascvd')), by = 'ID')
# data_full = dplyr::inner_join(data_full, feature_space, by = 'ID')
data_full = dplyr::inner_join(label_space, feature_space, by = 'ID')

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

# Long term prediction and VIMP: ###########################################################

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


for (fold in 1:nfolds){
  #fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'rsf_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_rsf(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # var ranking: ############################################################################
  #var_ranking_treedepth(rsf = model, saving_dir = saving_dir)
  library(randomForestSRC)
  max.subtree = max.subtree(model, conservative = F)
  #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
  
  # Get minimal depth of maximal subtree in terms of variable name, ascending order:
  allvardepth = sort(max.subtree$order[, 1])
  allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
  
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
 
  
   # load var dictionary to get var description:
  loading.dir = paste0(work_dir, '/csv_files')
  var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))
  
  get.var.des = function(x){
    var.des = var.dictionary.updated$`Variable.Label`[which(var.dictionary.updated$`Variable.Name` == x[1])]
    if(identical(var.des, character(0))){
      var.des = x[1]
    }
    return(var.des)
  }
  vardes = unlist(apply(allvardepth.df, 1, FUN = get.var.des))
  allvardepth.df = add_column(allvardepth.df, VariableDescription = vardes, .after = 1)
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank_all_var.csv', sep = ''),row.names=T)
  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)
  
  # probability of having had the disease:
  prob_risk_test = predictRisk.rsf(trained_model
                                   #  , traindata = trained_data
                                   , newdata = test_data
                                   , times = eval_times
  )
  prob_risk_test[is.na(prob_risk_test)] = 0
  performance_testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = trained_data
                                          , eval.times = eval_times
  )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset.RData'))
  
  
}



# Short term prediction and VIMP: ######################################################
trainingid_all = get(load(file = paste0(loading_dir,
                                        '/all_training_ID_outerloop_cohort_0_10.RData'
)))

for (fold in 1:nfolds){
  #fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_y10$ID)
  data = data_y10[which(data_y10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_y10[(which(!(data_y10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'rsf_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_rsf(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  # var ranking: #################################################################################
  #var_ranking_treedepth(rsf = model, saving_dir = saving_dir)
  library(randomForestSRC)
  max.subtree = max.subtree(model, conservative = F)
  #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
  
  # Get minimal depth of maximal subtree in terms of variable name, ascending order:
  allvardepth = sort(max.subtree$order[, 1])
  allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
  
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)

  
  
  # load var dictionary to get var description:
  loading.dir = paste0(work_dir, '/csv_files')
  var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))
  
  get.var.des = function(x){
    var.des = var.dictionary.updated$`Variable.Label`[which(var.dictionary.updated$`Variable.Name` == x[1])]
    if(identical(var.des, character(0))){
      var.des = x[1]
    }
    return(var.des)
  }
  vardes = unlist(apply(allvardepth.df, 1, FUN = get.var.des))
  allvardepth.df = add_column(allvardepth.df, VariableDescription = vardes, .after = 1)
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank_all_var.csv', sep = ''),row.names=T)

  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))
  
  # probability of having had the disease:
  prob_risk_test = predictRisk.rsf(trained_model
                                   #  , traindata = trained_data
                                   , newdata = test_data
                                   , times = eval_times
  )
  prob_risk_test[is.na(prob_risk_test)] = 0
  performance_testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = trained_data
                                          , eval.times = eval_times
  )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset.RData'))
  
  
}  

