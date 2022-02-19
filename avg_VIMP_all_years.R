# read all feature ranking .csv files, get Variable Importance (VIMP) (in this case is min tree depth) of each feature and output the averaged min tree depth
#rm(list=ls()) #Clear all
cat("\024")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/normalize_var_importance.R'))
source(paste0(work_dir,'/cardia_rerun_2_code/snippet/createDir.R'))

setwd(work_dir)




  ## average VIMP all years:

  # model_name = paste0('rsf_all_vars_y', y_oi ,'_sep')
  # model_name = 'rsf_all_var_all_years'
  # nfold = 25
  # fold = 1
  #
  #
  # #loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)
  # loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  # feature.ranking = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
  # colnames(feature.ranking)[4] = paste0('MinDepth_fold_', fold)
  # colnames(feature.ranking)[1] = ''
  # for (fold in 2:nfold){
  #   loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  #   feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank_all_var.csv'))
  #   colnames(feature.ranking.foldn)[4] = paste0('MinDepth_fold_', fold)
  #   feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(2,4)], .by = 'Variable')
  # }
  # feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(4,nfold+3,1)])
  # feature.ranking = within(feature.ranking, rm(Var.2))
  # feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
  # feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)
  #
  # saving.dir = createDir(work_dir, 'csv_files')
  # feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
  # write.csv(feature.ranking[,c('index','Variable','VariableDescription','Avg_min_depth','normalized_depth')]
  #           ,file = paste0(saving.dir,'/averaged_outerloop_VIMP_all_years.csv'), row.names = F)
  #



  # # model_name = paste0('rsf_all_vars_y', y_oi ,'_sep')
  # model_name = 'rsf_all_var_all_years_rerun'
  # nfold = 25
  # fold = 1
  #
  #
  # #loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)
  # loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  # feature.ranking = read.csv(paste0(loading.dir,'/depth_rank.csv'))
  #
  # # load var dictionary to get var description:
  # loading.dir = paste0(work_dir, '/csv_files')
  # var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))
  #
  # get.var.des = function(x){
  #   var.des = var.dictionary.updated$`Variable.Label`[which(var.dictionary.updated$`Variable.Name` == x[1])]
  #   if(identical(var.des, character(0))){
  #     var.des = x[1]
  #   }
  #   return(var.des)
  # }
  # vardes = unlist(apply(feature.ranking, 1, FUN = get.var.des))
  # feature.ranking = add_column(feature.ranking, VariableDescription = vardes, .after = 1)
  # colnames(feature.ranking)[3] = paste0('MinDepth_fold_', fold)
  # # colnames(feature.ranking)[1] = ''
  #
  # for (fold in 2:nfold){
  #   loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  #   feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank.csv'))
  #   colnames(feature.ranking.foldn)[2] = paste0('MinDepth_fold_', fold)
  #   feature.ranking = merge(feature.ranking, feature.ranking.foldn, .by = 'Variable')
  # }
  # feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(3,nfold+2,1)])
  # feature.ranking = within(feature.ranking, rm(Var.2))
  # feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
  # feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)
  #
  # saving.dir = createDir(work_dir, 'csv_files')
  # feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
  # write.csv(feature.ranking[,c('index','Variable','VariableDescription','Avg_min_depth','normalized_depth')]
  #           ,file = paste0(saving.dir,'/averaged_outerloop_VIMP_all_years_rerun.csv'), row.names = F)
  #
  #