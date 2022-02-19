rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun'
source(paste0(work_dir,'/code/snippet/get_median.R'))
require(dplyr)
require(ggplot2)
require(reshape2)



# extract mean c-index values:
loading.dir = paste0(work_dir,'/rdata_files')


get_combined_performance_mean = function(algo_name, n_features, metric = 'cindex'){
  load(paste0(loading.dir, '/performance_', algo_name, '_', n_features, '_var_long_term.RData'))
  model_long_term_mean = get_mean(performance.report.dynamic, metric = metric, term_pred = 'long_term')
  
  load(paste0(loading.dir, '/performance_', algo_name, '_', n_features, '_var_short_term.RData'))
  model_short_term_mean = get_mean(performance.report.dynamic, metric = metric, term_pred = 'short_term')
  
  model_mean = c(model_short_term_mean[2:6], model_long_term_mean)
  
  return(model_mean)  
} 
get_combined_performance_median = function(algo_name, n_features, metric = 'cindex'){
  load(paste0(loading.dir, '/performance_', algo_name, '_', n_features, '_var_long_term.RData'))
  model_long_term_median = get_median(performance.report.dynamic, metric = metric, term_pred = 'long_term')
  
  load(paste0(loading.dir, '/performance_', algo_name, '_', n_features, '_var_short_term.RData'))
  model_short_term_median = get_median(performance.report.dynamic, metric = metric, term_pred = 'short_term')
  
  model_median = c(model_short_term_median[2:6], model_long_term_median)
  
  return(model_median)  
} 


loading.dir = paste0(work_dir,'/rdata_files')
rsf_all_var_mean <- get_combined_performance_mean('rsf', 'all', 'cindex')
cForest_all_var_mean <- get_combined_performance_mean('cForest', 'all', 'cindex')
coxBoost_all_var_mean <- get_combined_performance_mean('coxBoost', 'all', 'cindex')
nnet_survival_all_var_mean <- get_combined_performance_mean('nnet_survival', 'all', 'cindex')
cox_nnet_all_var_mean <- get_combined_performance_mean('cox_nnet', 'all', 'cindex')

rsf_20_var_mean <- get_combined_performance_mean('rsf', '20', 'cindex')
cForest_20_var_mean <- get_combined_performance_mean('cForest', '20', 'cindex')
coxBoost_20_var_mean <- get_combined_performance_mean('coxBoost', '20', 'cindex')
gbm_20_var_mean <- get_combined_performance_mean('gbm', '20', 'cindex')
nnet_survival_20_var_mean <- get_combined_performance_mean('nnet_survival', '20', 'cindex')
cox_nnet_20_var_mean <- get_combined_performance_mean('cox_nnet', '20', 'cindex')
deephit_20_var_mean <- get_combined_performance_mean('deephit', '20', 'cindex')

rsf_ascvd_var_mean <- get_combined_performance_mean('rsf', 'ascvd', 'cindex')
cox_ascvd_var_mean <- get_combined_performance_mean('cox', 'ascvd', 'cindex')
cForest_ascvd_var_mean <- get_combined_performance_mean('cForest', 'ascvd', 'cindex')
cForest_ascvd_var_mean = ifelse(cForest_ascvd_var_mean <0.5, 0.5, cForest_ascvd_var_mean)
nnet_survival_ascvd_var_mean <- get_combined_performance_mean('nnet_survival', 'ascvd', 'cindex')





rsf_all_var_median <- get_combined_performance_median('rsf', 'all', 'cindex')
cForest_all_var_median <- get_combined_performance_median('cForest', 'all', 'cindex')
coxBoost_all_var_median <- get_combined_performance_median('coxBoost', 'all', 'cindex')
nnet_survival_all_var_median <- get_combined_performance_median('nnet_survival', 'all', 'cindex')
cox_nnet_all_var_median <- get_combined_performance_median('cox_nnet', 'all', 'cindex')

rsf_20_var_median <- get_combined_performance_median('rsf', '20', 'cindex')
cForest_20_var_median <- get_combined_performance_median('cForest', '20', 'cindex')
coxBoost_20_var_median <- get_combined_performance_median('coxBoost', '20', 'cindex')
gbm_20_var_median <- get_combined_performance_median('gbm', '20', 'cindex')
nnet_survival_20_var_median <- get_combined_performance_median('nnet_survival', '20', 'cindex')
cox_nnet_20_var_median <- get_combined_performance_median('cox_nnet', '20', 'cindex')
deephit_20_var_median <- get_combined_performance_median('deephit', '20', 'cindex')

rsf_ascvd_var_median <- get_combined_performance_median('rsf', 'ascvd', 'cindex')
cox_ascvd_var_median <- get_combined_performance_median('cox', 'ascvd', 'cindex')
cForest_ascvd_var_median <- get_combined_performance_median('cForest', 'ascvd', 'cindex')
cForest_ascvd_var_median = ifelse(cForest_ascvd_var_median <0.5, 0.5, cForest_ascvd_var_median)

nnet_survival_ascvd_var_median <- get_combined_performance_median('nnet_survival', 'ascvd', 'cindex')









performance.all = list()
for(fold in 1:25){
  loading.dir = paste0(work_dir, '/rdata_files/ascvd_short_term_fold_', fold)
  performance = list((get(load(paste0(loading.dir,'/c_ascvd_short_term_testset.RData')))))
  performance.all = append(performance.all, performance)
}
performance.report.dynamic = list()
performance.report.dynamic$dynamic.cindex.all = matrix(unlist(performance.all), nrow = length(performance.all), byrow = T)
riskscore_short_term_mean = get_mean(performance.report.dynamic, term_pred = 'short_term')
riskscore_short_term_median = get_median(performance.report.dynamic, term_pred = 'short_term')

performance.all = list()
for(fold in 1:25){
  loading.dir = paste0(work_dir, '/rdata_files/ascvd_long_term_fold_', fold)
  performance = list((get(load(paste0(loading.dir,'/c_ascvd_long_term_testset.RData')))))
  performance.all = append(performance.all, performance)
}
performance.report.dynamic = list()
performance.report.dynamic$dynamic.cindex.all = matrix(unlist(performance.all), nrow = length(performance.all), byrow = T)
riskscore_long_term_mean = get_mean(performance.report.dynamic, term_pred = 'long_term')
riskscore_long_term_median = get_median(performance.report.dynamic, term_pred = 'long_term')

riskscore_mean = c(riskscore_short_term_mean[2:6], riskscore_long_term_mean)
riskscore_mean = ifelse(riskscore_mean <0.5, 0.5, riskscore_mean)

riskscore_median = c(riskscore_short_term_median[2:6], riskscore_long_term_median)
riskscore_median = ifelse(riskscore_median <0.5, 0.5, riskscore_median)




eval.times <- seq(2,26, 2)
df_for_plot = data.frame(eval.times
                         
                         ,nnet_survival_20_var_median
                         ,nnet_survival_all_var_median
                         
                         ,rsf_all_var_median
                         ,rsf_20_var_median
                         # ,cForest_all_var_median
                         #,nnet_survival_all_var_median

                         # ,cForest_20_var_mean
                         
                         ,cForest_ascvd_var_median
                         # ,cox_ascvd_var_mean

                         ,riskscore_median)


names(df_for_plot) = c('eval.times'
                       ,'Nnet-survival Top 20 Variables'
                       ,'Nnet-survival All Variables'
                       ,'RSF All Variables'
                       ,'RSF Top 20 Variables'
                       ,'cForest ASCVD Variables'
                       ,'ASCVD Risk Score (Benchmark)')
                         
                    

df_for_plot.long = melt(df_for_plot, id = 'eval.times')

names(df_for_plot.long) = c('time','Model','quant')


# only plot for Year 14 and beyond:
time_thres = 14
plot_time_short_term = seq(2,10,2)
plot_time_long_term = seq(14,26,2)
df_for_plot.long.filter = df_for_plot.long   %>% filter(time != 12) # %>% filter((time >=4))


cindex.plot = ggplot(data = df_for_plot.long.filter)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)
cindex.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
   geom_point(aes( time, quant, color = Model, group = Model), size = 2)+
   geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = 'loess', span = 0.65)+
#  geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 5))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 3") +
  ylab("C-index") +
#  xlim(time_thres, 26) + 
 # ylim(0.5, 0.95) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=21),
        axis.title=element_text(size=21,face="italic"),
        legend.title = element_text(color = "black", size = 22),
        legend.text = element_text(color = "black", size = 22)) 
 # geom_vline(xintercept=10)
