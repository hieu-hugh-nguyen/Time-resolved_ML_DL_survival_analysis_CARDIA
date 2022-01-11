# outer loop 

#rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun'
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


source(paste0(work_dir,'/code/snippet/createDir.R'))
source(paste0(work_dir,'/code/snippet/subsetDataTopnVar.R'))
source(paste0(work_dir,'/code/snippet/predictSurvProb.R'))
source(paste0(work_dir,'/code/snippet/eval_performance.R'))
source(paste0(work_dir,'/code/snippet/eval_performance_using_different_auc_package.R'))
source(paste0(work_dir,'/code/snippet/predictRisk_DeepSurv.R'))

loading_dir = paste0(work_dir,'/csv_files')
data_after_y10_origin_at_10 = read.csv(file = paste0(loading_dir,
                                                     '/data_after_y10_origin_at_10.csv'))
data_y10 = read.csv(file = paste0(loading_dir,
                                  '/data_y10.csv'))

#'C08DIAB' 'HBP05'
var = 'C08KIDNY'
table(data_y10[[var]])

