# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','mboost','glmnet', 'gbm', 'mlr', 'CoxBoost', 'party')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

# # may need to install CoxBoost differerntly since the package has been archived:
# install.packages('https://cran.r-project.org/src/contrib/Archive/CoxBoost/CoxBoost_1.4.tar.gz', repos = NULL, type = 'source')

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



# write.csv(data_after_y10_origin_at_10, paste0(loading_dir,'/data_after_y10_origin_at_10.csv'),row.names = F)
# write.csv(data_y10, paste0(loading_dir,'/data_y10.csv'),row.names = F)
















### cForest:#######################################################################

# Long term prediction: ###########################################################

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
  # fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'cForest_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_cForest(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))



  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data

  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)

  # probability of having had the disease:
  tryCatch({
    prob_risk_test = predictRisk.cForest(trained_model
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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


}



# Short term prediction: ######################################################
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

  
  model_name = 'cForest_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_cForest(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))



  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data

  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))

  # probability of having had the disease:
  tryCatch({
    prob_risk_test = predictRisk.cForest(trained_model
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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  
}  














### Gradient Boosting Machine:###############################################

# Long term prediction: #####################################################

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
  
  
  model_name = 'gbm_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_gbm(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)
  
  # probability of having had the disease:
  tryCatch({
  prob_risk_test = predictRisk.gbm(trained_model
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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}



# Short term prediction: ######################################################
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
  
  
  model_name = 'gbm_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_gbm(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  

  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))
  
  # probability of having had the disease:
  tryCatch({
    prob_risk_test = predictRisk.gbm(trained_model
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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  
}  








### LASSO-Cox:#####################################################################
# Long term prediction: ###########################################################

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


for (fold in 8:nfolds){
  #fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'lasso_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_lasso(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)
  
  # probability of having had the disease:
  
  if(!is.null(model)){
    prob_risk_test = predictRisk.cox(trained_model
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
  
}



# Short term prediction: ######################################################
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
  
  
  model_name = 'lasso_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_lasso(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data
  
  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))
  
  # probability of having had the disease:
  if(!is.null(model)){
    prob_risk_test = predictRisk.cox(trained_model
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
  
}  









### CoxBoost ########################################################################

# Long term prediction: ###########################################################

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

# skip fold 2, 7,
for (fold in 8:nfolds){
  #fold = 1
  # Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))

  
  model_name = 'coxBoost_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_coxBoost2(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))



  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data

  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)

  # probability of having had the disease:
  prob_risk_test = predictRisk.coxBoost2(trained_model
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



# Short term prediction: ######################################################
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


  model_name = 'coxBoost_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model = running_coxBoost2(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))



  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data

  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))

  # probability of having had the disease:
  prob_risk_test = predictRisk.coxBoost2(trained_model
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

















### GLMBOOST ########################################################################
# Long term prediction: ###########################################################

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
  ## Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10$ID)
  data = data_after_y10_origin_at_10[which(data_after_y10_origin_at_10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_after_y10_origin_at_10[(which(!(data_after_y10_origin_at_10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))

  
  model_name = 'glmboost_all_var_long_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))



  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  #trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  trained_model = model
  trained_data = data

  endpt = 16; # year 26 after Exam 3
  eval_times = 365.25*seq(2, endpt, by = 2)

  # probability of having had the disease:
  prob_risk_test = predictRisk.glmboost(trained_model
                                        , traindata = trained_data
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



# Short term prediction: ######################################################
loading_dir = paste0(work_dir, '/rdata_files')
trainingid_all = get(load(file = paste0(loading_dir,
                                        '/all_training_ID_outerloop_cohort_0_10.RData'
)))

for (fold in 1:nfolds){
#  Training and fitting model:
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_y10$ID)
  data = data_y10[which(data_y10$ID %in% eligible_id),]
  data = within(data, rm('ID'))
  test_data = data_y10[(which(!(data_y10$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  model_name = 'glmboost_all_var_short_term'
  gc()
  main_dir = paste0(work_dir, '/rdata_files')
  sub_dir = paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_glmboost(data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))

  
  
  # Test set performance: ###################################################################
  loading.dir = paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir = loading.dir
  trained_model = get(load(paste0(loading.dir,'/', model_name, '.RData')))
  # trained_model = model
  trained_data = data

  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))

  tryCatch({

    # probability of having had the disease:
    prob_risk_test = predictRisk.glmboost(trained_model
                                          , traindata = trained_data
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

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

