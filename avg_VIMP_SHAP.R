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


model='rsf'
pred_term ='short_term'

nfold = 25
fold = 1

#loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_y24_fold_',fold)

loading.dir = paste0(work_dir,'/csv_files/',model,'/all_features/',pred_term)

feature.ranking.fold1 = read.csv(paste0(loading.dir,'/shap_kernel_explainer_fold_',fold,'.csv'))

colnames(feature.ranking.fold1)[3] = paste0('normalized_SHAP_fold_', fold)
feature.ranking = feature.ranking.fold1[c('Variable.Name', 'Variable.Label', 'normalized_SHAP_fold_1')]

# load var dictionary to get var description:
#loading.dir = paste0(work_dir, '/csv_files')
# var.dictionary.updated = read.csv(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (3).csv"))

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

for (fold in 2:nfold){
#  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking.foldn = read.csv(paste0(loading.dir,'/shap_kernel_explainer_fold_',fold,'.csv'))
  colnames(feature.ranking.foldn)[3] = paste0('normalized_SHAP_fold_', fold)
  feature.ranking = feature.ranking %>% full_join(feature.ranking.foldn %>% dplyr::select(c('Variable.Name',paste0('normalized_SHAP_fold_', fold))), .by = 'Variable.Name')
}
feature.ranking$'mean_SHAP' = rowMeans(feature.ranking[,seq(3,nfold+2,1)])
feature.ranking = feature.ranking[order(-feature.ranking$'mean_SHAP'),]
feature.ranking$norm_SHAP_imp = normalize_var_imp(feature.ranking$'mean_SHAP')

saving.dir = createDir(work_dir, 'csv_files')
feature.ranking$'index' = seq(1,nrow(feature.ranking),1)

write.csv(feature.ranking[,c('index','Variable.Name','Variable.Label','norm_SHAP_imp')] # %>% View()
          ,file = paste0(saving.dir,'/averaged_outerloop_SHAP_',model,'_', pred_term,'.csv'), row.names = F)


# # Averaging SHAP values, then normalize:
# fold = 1
# colnames(feature.ranking.fold1)[4] = paste0('SHAP_fold_', fold)
# feature.ranking = feature.ranking.fold1[c('Variable.Name', 'Variable.Label', 'SHAP_fold_1')]
# 
# 
# for (fold in 2:nfold){
#   #  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
#   feature.ranking.foldn = read.csv(paste0(loading.dir,'/shap_kernel_explainer_fold_',fold,'.csv'))
#   colnames(feature.ranking.foldn)[4] = paste0('SHAP_fold_', fold)
#   feature.ranking = feature.ranking %>% full_join(feature.ranking.foldn %>% dplyr::select(c('Variable.Name',paste0('SHAP_fold_', fold))), .by = 'Variable.Name')
# }
# feature.ranking$'Avg_norm_SHAP' = rowMeans(feature.ranking[,seq(3,nfold+2,1)])
# #feature.ranking = within(feature.ranking, rm(Var.2))
# feature.ranking = feature.ranking[order(-feature.ranking$Avg_norm_SHAP),]
# feature.ranking$normalized_SHAP = normalize_var_imp(feature.ranking$Avg_norm_SHAP)
# 
# saving.dir = createDir(work_dir, 'csv_files')
# feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
# 
# feature.ranking[,c('index','Variable.Name','Variable.Label','normalized_SHAP')] %>% View()