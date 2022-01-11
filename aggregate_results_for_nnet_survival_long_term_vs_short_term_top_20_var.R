# outer loop 

#rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR', 'parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

source(paste0(work_dir,'/cardia_rerun_2_code/snippet/createDir.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/subsetDataTopnVar.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/predictSurvProb.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/eval_performance.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/eval_performance_using_different_auc_package.R'))


loading_dir = paste0(work_dir,'/csv_files')
data_after_y10_origin_at_10_top_var = read.csv(file = paste0(loading_dir,
                                                 '/data_after_y10_origin_at_10_top_20_var.csv'))
data_y10_top_var = read.csv(file = paste0(loading_dir,
                                          '/data_y10_top_20_var.csv'))
  



















# Long term prediction evaluation: ######################################################
loading_dir = paste0(work_dir, '/rdata_files')
trainingid_all = get(load(file = paste0(loading_dir,
                                        '/all_training_ID_outerloop_cohort_10_26.RData')))
endpt = 16; # year 26 after Exam 3
eval_times = 365.25*seq(2, endpt, by = 2)

# for (fold in 1:25){
for (fold in c(1,3:6,8:25)){  
    
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_after_y10_origin_at_10_top_var$ID)
  trained_data = data_after_y10_origin_at_10_top_var[which(data_after_y10_origin_at_10_top_var$ID %in% eligible_id),]
  trained_data = within(trained_data, rm('ID'))
  test_data = data_after_y10_origin_at_10_top_var[(which(!(data_after_y10_origin_at_10_top_var$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))


  loading.dir = paste0(work_dir, '/csv_files/nnet_survival/20_features/long_term')
  pred_prob_surv = read.csv(file = paste0(loading.dir,
                                          '/pred_prob_surv_fold_',fold,'.csv'))
  pred_prob_surv = as.matrix(pred_prob_surv)
  prob_risk_test = 1-pred_prob_surv
  #prob_risk_test = pred_prob_surv
  prob_risk_test[is.na(prob_risk_test)] = 0
  
  performance.testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                               , test.data = test_data
                                               , trained.data = trained_data
                                               , eval.times = eval_times)
  
  #saving.dir = 'U:/Hieu/CARDIA_project/CARDIA_project/Git/csv_files'
  main.dir = paste0(work_dir, '/rdata_files')
  sub.dir = paste0('nnet_survival_20_var_long_term_fold_',fold)
  if(!dir.exists(file.path(main.dir, sub.dir))){
    createDir(main.dir, sub.dir)
  }
  saving.dir = file.path(main.dir, sub.dir)
  
  save(performance.testset
       , file = paste0(saving.dir, '/performance_testset.RData'))
  
}  





# Short term prediction evaluation: ######################################################
loading_dir = paste0(work_dir, '/rdata_files')
trainingid_all = get(load(file = paste0(loading_dir,
                                        '/all_training_ID_outerloop_cohort_0_10.RData')))
endpt = 10; # year 26 after Exam 3
eval_times = 365.25*c(1, seq(2, endpt, by = 2))


for (fold in 1:25){
  
  trainingid = na.omit(trainingid_all[,fold])
  eligible_id = intersect(trainingid, data_y10_top_var$ID)
  trained_data = data_y10_top_var[which(data_y10_top_var$ID %in% eligible_id),]
  trained_data = within(trained_data, rm('ID'))
  test_data = data_y10_top_var[(which(!(data_y10_top_var$ID %in% eligible_id))),]
  test_data = within(test_data, rm('ID'))
  
  
  loading.dir = paste0(work_dir, '/csv_files/nnet_survival/20_features/short_term')
  pred_prob_surv = read.csv(file = paste0(loading.dir,
                                          '/pred_prob_surv_fold_',fold,'.csv'))
  pred_prob_surv = as.matrix(pred_prob_surv)
  prob_risk_test = 1-pred_prob_surv
  #prob_risk_test = pred_prob_surv
  prob_risk_test[is.na(prob_risk_test)] = 0
  
  performance.testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = trained_data
                                          , eval.times = eval_times)
  
  #saving.dir = 'U:/Hieu/CARDIA_project/CARDIA_project/Git/csv_files'
  main.dir = paste0(work_dir, '/rdata_files')
  sub.dir = paste0('nnet_survival_20_var_short_term_fold_',fold)
  if(!dir.exists(file.path(main.dir, sub.dir))){
    createDir(main.dir, sub.dir)
  }
  saving.dir = file.path(main.dir, sub.dir)
  
  save(performance.testset
     , file = paste0(saving.dir, '/performance_testset.RData'))

}  
