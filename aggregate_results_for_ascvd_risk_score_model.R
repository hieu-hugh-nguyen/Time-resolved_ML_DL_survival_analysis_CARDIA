rm(list=ls()) #Clear all
cat("\014")
performance.all = list()
nfold = 25
endpt = 26; # year 30

work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/bootstrap_ci.R'))

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

