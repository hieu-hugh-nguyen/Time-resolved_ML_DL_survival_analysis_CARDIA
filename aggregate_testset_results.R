rm(list=ls()) #Clear all
cat("\014")

work_dir= 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)
y_oi = 26
nfold = 25
endpt = y_oi; # year 26

#eval_times = 365.25*c(10, seq(2,endpt, 2))

performance.all = list()


model_list = c(
 'rsf_all_var_short_term'
, 'rsf_all_var_long_term'
, 'cForest_all_var_short_term'
, 'cForest_all_var_long_term'
, 'lasso_all_var_short_term'
, 'lasso_all_var_long_term'
, 'coxBoost_all_var_short_term'
, 'coxBoost_all_var_long_term'
, 'gbm_all_var_short_term'
, 'gbm_all_var_long_term'
, 'glmboost_all_var_short_term'
, 'glmboost_all_var_long_term'
, 'nnet_survival_all_var_short_term'
, 'nnet_survival_all_var_long_term'
, 'cox_nnet_all_var_short_term'
, 'cox_nnet_all_var_long_term'
, 'deephit_all_var_long_term'
, 'deephit_all_var_short_term'
, 'deepsurv_all_var_long_term'
, 'deepsurv_all_var_short_term'

  , 'rsf_20_var_short_term'
  ,'rsf_20_var_long_term'
  , 'cForest_20_var_short_term'
  , 'cForest_20_var_long_term'
  , 'cox_20_var_short_term'
  , 'cox_20_var_long_term'
  ,'lasso_20_var_short_term'
  ,'lasso_20_var_long_term'
  ,'coxBoost_20_var_short_term'
  ,'coxBoost_20_var_long_term'
  ,'gbm_20_var_short_term'
  ,'gbm_20_var_long_term'
  ,'glmboost_20_var_short_term'
  ,'glmboost_20_var_long_term'
  ,'nnet_survival_20_var_short_term'
  ,'nnet_survival_20_var_long_term'
  ,'cox_nnet_20_var_short_term'
  ,'cox_nnet_20_var_long_term'
  ,'deephit_20_var_short_term'
  ,'deephit_20_var_long_term'
  ,'deepsurv_20_var_long_term'
  ,'deepsurv_20_var_short_term'

  ,'rsf_ascvd_var_short_term'
  ,'rsf_ascvd_var_long_term'
  , 'cForest_ascvd_var_short_term'
  , 'cForest_ascvd_var_long_term'
  , 'cox_ascvd_var_short_term'
  , 'cox_ascvd_var_long_term'
  ,'lasso_ascvd_var_short_term'
  ,'lasso_ascvd_var_long_term'
  ,'gbm_ascvd_var_short_term'
  ,'gbm_ascvd_var_long_term'
  ,'coxBoost_ascvd_var_short_term'
  ,'coxBoost_ascvd_var_long_term'
  ,'glmboost_ascvd_var_short_term'
  ,'glmboost_ascvd_var_long_term'
  ,'nnet_survival_ascvd_var_short_term'
  , 'nnet_survival_ascvd_var_long_term'
  , 'cox_nnet_ascvd_var_short_term'
  , 'cox_nnet_ascvd_var_long_term'
  , 'deephit_ascvd_var_short_term'
  , 'deephit_ascvd_var_long_term'
  , 'deepsurv_ascvd_var_short_term'
  , 'deepsurv_ascvd_var_long_term'
  
   )

agg_perf_and_ci <- function(model_name){
  for(fold in 1:nfold){
    loading.dir = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold)
    
    tryCatch({
    performance = list((get(load(paste0(loading.dir,'/performance_testset.RData')))))
    performance.all = append(performance.all, performance)
    }, error=function(e){cat("WARNING :",conditionMessage(e), "fold ", fold, "\n")})
  }
  
  # extract each metric: 
  performance.report.static = list()
  performance.report.static = within(performance.report.static,
    {last.cindex.all = c();last.brier.all = c(); uno.c.all = c(); iauc.uno.all = c(); 
    }
  )
  performance.report.dynamic= list()
  performance.report.dynamic = within(performance.report.dynamic,
                                 {dynamic.auc.all = c(); dynamic.auc.uno.all = c(); 
                                 dynamic.cindex.all = c(); 
                                 brier.all = c()
                                 }
  )
  
  for(fold in 1:length(performance.all)){
    performance.report.static = within(performance.report.static, {
      last.cindex.all = append(last.cindex.all, unlist(performance.all[[fold]]$last.cindex))
      names(last.cindex.all) = NULL                          
      last.brier.all = append(last.brier.all, unlist(performance.all[[fold]]$last.brier['matrix'])) 
      names(last.brier.all) = NULL
      uno.c.all = append(uno.c.all, unlist(performance.all[[fold]]$uno.c))
      iauc.uno.all = append(iauc.uno.all, unlist(performance.all[[fold]]$iauc.uno))
      #iauc.cd.all = append(iauc.cd.all, unlist(performance.all[[fold]]$iauc.cd))
    })
    performance.report.dynamic = within(performance.report.dynamic, {
      dynamic.auc.all = rbind(dynamic.auc.all, performance.all[[fold]]$auc)
      dynamic.auc.uno.all = rbind(dynamic.auc.uno.all, performance.all[[fold]]$uno.auc$auc)
      #dynamic.auc.cd.all = rbind(dynamic.auc.cd.all, performance.all[[fold]]$cd.auc$auc)
      dynamic.cindex.all = rbind(dynamic.cindex.all, performance.all[[fold]]$cind.obj$AppCindex$matrix)
      brier.all = rbind(brier.all, performance.all[[fold]]$pec.obj$AppErr$matrix)
    })  
  }
  
  #performance.report.dynamic$dynamic.cindex.all
  saving.dir = paste0(work_dir,'/rdata_files')
  save(performance.report.static, performance.report.dynamic
       ,file = paste0(saving.dir,'/performance_',model_name,'.RData'))
  
  
  
  
  
  
  # visualization:
  loading.dir = paste0(work_dir,'/rdata_files')
  #load(paste0(loading.dir,'/performance_AICcox_20_var.RData'))
  #
  endpt = 10; # year 26 after Exam 3
  eval_times = 365.25*c(1, seq(2, endpt, by = 2))
  
  static.summary = lapply(performance.report.static, summary)
  require('matrixStats')
  dynamic.summary = lapply(performance.report.dynamic, matrixStats::colMedians)
  
  last.brier = static.summary$last.brier.all
  
  #View(static.summary)
  # For models of which origins at Year 10:
  plot(10+eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.auc.uno.all[3:length(eval_times)], type = 'l'
       ,ylim = c(0.5, 1)
       )
  lines(10+eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.cindex.all[3:length(eval_times)], col = 'red')
  lines(10+eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.auc.all[3:length(eval_times)], col = 'blue')
  abline(h=0.8,lty=2)
  abline(h=0.75,lty=2)
  
  # For models of which origins at Year 0:
  plot(eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.auc.uno.all[3:length(eval_times)], type = 'l'
       ,ylim = c(0.5, 1)
  )
  lines(eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.cindex.all[3:length(eval_times)], col = 'red')
  lines(eval_times[3:length(eval_times)]/365.25, dynamic.summary$dynamic.auc.all[3:length(eval_times)], col = 'blue')
  abline(h=0.8,lty=2)
  abline(h=0.75,lty=2)
  
  
  
  
  # get CI of the model performance in 3 metrics: C-index, AUC, and 1-Brier Score:
  
  require(dplyr)
  work_dir= 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun'
  loading.dir = paste0(work_dir,'/rdata_files')
  
  source(paste0(work_dir,'/code/snippet/bootstrap_ci.R'))
  load(paste0(loading.dir,'/performance_',model_name, '.RData'))
  
  if(isTRUE(grepl("long_term", model_name, fixed = TRUE))){
    endpt = 16; # year 26 after Exam 3
    eval_times = 365.25*seq(2, endpt, by = 2)
    timept = 16
    
  }
  if(isTRUE(grepl("short_term", model_name, fixed = TRUE))){
    endpt = 10; # year 26 after Exam 3
    eval_times = 365.25*c(1, seq(2, endpt, by = 2))
    timept = 10
    
  }
  # lowerbound = numeric(length(eval_times))
  # upperbound = numeric(length(eval_times))
  # mean = numeric(length(eval_times))
  
  get_ci = function(type){
    
    quant = list()
    for (i in 1:length(eval_times)){
      if(type == 'cindex'){
        quant[[i]] = na.omit(performance.report.dynamic$dynamic.cindex.all[,i])
      }
      if(type == 'auc'){
        quant[[i]] = na.omit(performance.report.dynamic$dynamic.auc.all[,i])
      }
      if(type == 'auc_uno'){
        quant[[i]] = na.omit(performance.report.dynamic$dynamic.auc.uno.all[,i])
      }
      if(type == 'brier'){
        quant[[i]] = 1-na.omit(performance.report.dynamic$brier.all[,i])
      }
      quant[[i]] = bootstrap_ci(quant[[i]])
      #lowerbound[i] = append(lowerbound, quant[[i]][1])
      #mean[i] = append(mean, quant[[i]][2])
      #lowerbound[i] = append(lowerbound, quant[[i]][3])
      
    }
    return(quant)
  }
  
  model.cindex = get_ci('cindex') 
  model.auc = get_ci('auc')
  model.auc.uno = get_ci('auc_uno')
  model.brier = get_ci('brier')
  
  
  saving.dir = paste0(work_dir,'/rdata_files')
  save(model.cindex, model.auc, model.auc.uno, model.brier, 
       file = paste0(saving.dir, '/ci_performance_', model_name,'.RData'))
  
  
  # load(paste0(loading.dir, '/ci_performance_rsf_20_var_ver2_y26.RData'))
  # model.cindex = ci_perf[[1]]
  # model.auc = ci_perf[[2]]
  # model.auc.uno = ci_perf[[3]]
  # model.brier = ci_perf[[4]]
  
  #year 26, 16, and 10 after Exam 3: 
  
  print_value <- function(ci_output, digits = 2){
    print(paste0(round(ci_output[2], digits = digits),' (',
                 ci_output[1] %>% round(digits = digits),', ',
                 ci_output[3] %>% round(digits = digits),')'))
  }
  
  if(isTRUE(grepl("long_term", model_name, fixed = TRUE))){
    year_oi = 16
    print(paste0('Performance for Model ', model_name, ':'))
    print('C-index:')
    print_value(model.cindex[[which(eval_times == year_oi*365.25)]])
    print('AUC:')
    print_value(model.auc[[which(eval_times == year_oi*365.25)]])
    print('1-Brier:')
    print_value(model.brier[[which(eval_times == year_oi*365.25)]]
                , digits = 3)
    cat("\n")
    
  }
  
  if(isTRUE(grepl("short_term", model_name, fixed = TRUE))){
    year_oi = 10
    print(paste0('Performance for Model ', model_name, ':'))
    print('C-index:')
    print_value(model.cindex[[which(eval_times == year_oi*365.25)]])
    print('AUC:')
    print_value(model.auc[[which(eval_times == year_oi*365.25)]])
    print('1-Brier:')
    print_value(model.brier[[which(eval_times == year_oi*365.25)]]
                , digits = 3)
    cat("\n")
    
  }
}



for(n in 1:length(model_list)){
  agg_perf_and_ci(model_list[n])  
}








# Aggregate results for ASCVD risk score model:
# long term:
for(fold in 1:nfold){
  loading.dir = paste0(work_dir, '/rdata_files/ascvd_long_term_fold_', fold)
  performance = list((get(load(paste0(loading.dir,'/c_ascvd_long_term_testset.RData')))))
  performance.all = append(performance.all, performance)
}

endpt = 16
eval_times = 365.25*seq(2, endpt, by = 2)
#for collecting c-index of the baseline ASCVD model only:
dynamic.cindex.all = matrix(unlist(performance.all), nrow = length(performance.all), byrow = T)
model.cindex = list()
for (i in 1:length(eval_times)){
  
  model.cindex[[i]] = na.omit(dynamic.cindex.all[,i])
  model.cindex[[i]] = bootstrap_ci(model.cindex[[i]])
}

saving.dir = paste0(work_dir,'/rdata_files')
save(model.cindex, file = paste0(saving.dir, '/ci_cindex_ascvd_long_term.RData'))

# for example, 95% CI at year 26:
year = 16
model.cindex[[which(eval_times == year*365.25)]]



# short term:
for(fold in 1:nfold){
  loading.dir = paste0(work_dir, '/rdata_files/ascvd_short_term_fold_', fold)
  performance = list((get(load(paste0(loading.dir,'/c_ascvd_short_term_testset.RData')))))
  performance.all = append(performance.all, performance)
}

endpt = 10
eval_times = 365.25*c(1,seq(2, endpt, by = 2))
#for collecting c-index of the baseline ASCVD model only:
dynamic.cindex.all = matrix(unlist(performance.all), nrow = length(performance.all), byrow = T)
model.cindex = list()
for (i in 1:length(eval_times)){
  
  model.cindex[[i]] = na.omit(dynamic.cindex.all[,i])
  model.cindex[[i]] = bootstrap_ci(model.cindex[[i]])
}

saving.dir = paste0(work_dir,'/rdata_files')
save(model.cindex, file = paste0(saving.dir, '/ci_cindex_ascvd_short_term.RData'))

# for example, 95% CI at year 10:
year = 10
model.cindex[[which(eval_times == year*365.25)]]
