# read all feature ranking .csv files, get Variable Importance (VIMP) (in this case is min tree depth) of each feature and output the averaged min tree depth
#rm(list=ls()) #Clear all
cat("\024")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/normalize_var_importance.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/createDir.R'))

# change work_dir for mortality outcome, comment out if not mortality:
# work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/mortality_outcome'
setwd(work_dir)



# y_oi_vec <- c('8', '12', '14', '16', '18', '20', '22', '24')
y_oi_vec <- c('10')
for (y_oi in y_oi_vec){

  ## average VIMP long term:

  # model_name = paste0('rsf_all_vars_y', y_oi ,'_sep')
  model_name = 'rsf_all_var_long_term'
  nfold = 25
  fold = 1


  #loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)
  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
  colnames(feature.ranking)[4] = paste0('MinDepth_fold_', fold)
  colnames(feature.ranking)[1] = ''
  for (fold in 2:nfold){
    loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
    feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
    colnames(feature.ranking.foldn)[4] = paste0('MinDepth_fold_', fold)
    feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(2,4)], .by = 'Variable')
  }
  feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(4,nfold+3,1)])
  feature.ranking = within(feature.ranking, rm(Var.2))
  feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
  feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)

  saving.dir = createDir(work_dir, 'csv_files')
  feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
  write.csv(feature.ranking[,c('index','Variable','VariableDescription','Avg_min_depth','normalized_depth')]
            ,file = paste0(saving.dir,'/averaged_outerloop_VIMP_long_term.csv'), row.names = F)




  ## average VIMP short term:

  # model_name = paste0('rsf_all_vars_y', y_oi ,'_sep')
  model_name = 'rsf_all_var_short_term'
  nfold = 25
  fold = 1


  #loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)
  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
  colnames(feature.ranking)[4] = paste0('MinDepth_fold_', fold)
  colnames(feature.ranking)[1] = ''
  for (fold in 2:nfold){
    loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
    feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
    colnames(feature.ranking.foldn)[4] = paste0('MinDepth_fold_', fold)
    feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(2,4)], .by = 'Variable')
  }
  feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(4,nfold+3,1)])
  feature.ranking = within(feature.ranking, rm(Var.2))
  feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
  feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)

  saving.dir = createDir(work_dir, 'csv_files')
  feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
  write.csv(feature.ranking[,c('index','Variable','VariableDescription','Avg_min_depth','normalized_depth')]
            ,file = paste0(saving.dir,'/averaged_outerloop_VIMP_short_term.csv'), row.names = F)

  
  
  ## average VIMP all years: 
  
  # model_name = paste0('rsf_all_vars_y', y_oi ,'_sep')
  model_name = 'rsf_all_var_all_years'
  nfold = 25
  fold = 1
  
  
  #loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)
  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
  colnames(feature.ranking)[4] = paste0('MinDepth_fold_', fold)
  colnames(feature.ranking)[1] = ''
  for (fold in 2:nfold){
    loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
    feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
    colnames(feature.ranking.foldn)[4] = paste0('MinDepth_fold_', fold)
    feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(2,4)], .by = 'Variable')
  }
  feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(4,nfold+3,1)])
  feature.ranking = within(feature.ranking, rm(Var.2))
  feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
  feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)
  
  saving.dir = createDir(work_dir, 'csv_files')
  feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
  write.csv(feature.ranking[,c('index','Variable','VariableDescription','Avg_min_depth','normalized_depth')]
            ,file = paste0(saving.dir,'/averaged_outerloop_VIMP_all_years.csv'), row.names = F)
  
}