
# Nested Random Forest model:

#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())
cat("\014")
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir);


# load library
library(randomForestSRC)
library(pec)
library(riskRegression)
library(survival)
library(beepr)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(glmnet)
library(MASS)
library(doParallel)

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




# Load the var ranking:
loading_dir = paste0(work_dir,'/csv_files')
var_order_short_term_df = read.csv(file = paste0(loading_dir,
                                                 '/averaged_outerloop_VIMP_short_term.csv'))
var_order_long_term_df = read.csv(file = paste0(loading_dir,
                                                '/averaged_outerloop_VIMP_long_term.csv'))

var_order_short_term = apply(var_order_short_term_df, 2, as.character)[,2]
var_order_long_term = apply(var_order_long_term_df, 2, as.character)[,2]


## START THE NESTED RSF PROCESS: ############

var.order = var_order_short_term

trainingid_all = get(load(file = paste0(work_dir,'/rdata_files',
                                        '/all_training_ID_outerloop_cohort_0_10.RData'
)))

fold = 1
trainingid = na.omit(trainingid_all[,fold])
eligible_id = intersect(trainingid, data_y10$ID)
train_data = data_y10[which(data_y10$ID %in% eligible_id),]
train_data = within(train_data, rm('ID'))
test_data = data_y10[(which(!(data_y10$ID %in% eligible_id))),]
test_data = within(test_data, rm('ID'))
  
  

seed = 4995
set.seed(seed)

all.events=c("CVD")

BaselineYear = 5; # predict from y5
# pt = 25  # performance evaluated at year 30 (as time in the big datasheet was counted since Y5, so it would be 30 since Y5)
pt = 10
  
# Function to calculate prediction error and c-index at end time of RSF for a specific number of variables: 

RSF.performance = function(numVar){
  print(paste('number of variables  =', numVar))
  
  n.top = numVar
  train_data_top_var = train_data[, c('time', 'event',
                             var.order[1:n.top])]
  
  # RF objects
  set.seed(seed)
  rf.obj = rfsrc(Surv(time, event)~.,
                 data = train_data_top_var,
                 ntree = 1000,
                 splitrule = 'logrank')
  
  # External
  
  # Prediction error and c-index at end time
  rf.pe = pec::pec(list(rf.obj),
                   formula = Hist(time, event)~1,
                   data = test_data,
                   times = min(365.25*pt, 
                               max(test_data$time)), 
                   exact = T,
                   splitMethod = 'none',
                   cens.model = 'marginal')
  
  
  rf.c = cindex(list(rf.obj),
                formula = Hist(time, event)~1,
                data = test_data,
                eval.times = min(365.25*pt, 
                                 max(test_data$time)),
                splitMethod = 'none',
                cens.model = 'marginal')
  
  pe.external = rf.pe$AppErr$rfsrc[
    which(rf.pe$time == min(365.25*pt, 
                            max(test_data$time)))][1]
  c.external = rf.c$AppCindex
  
  return(c(n.top, 
           pe.external,
           c.external))
}



ptm = proc.time()
numVariables = length(var.order)
for (ind in 1:1){ # nUmber of events, here we only consider CVD (1 event)
  print(paste('outcome', all.events[ind]))
  
  ptmi = proc.time()
  pe.c = lapply(c(1:19, seq(20, 100, by = 10), seq(110, numVariables, by = 20), numVariables),
         RSF.performance)
    #lapply(seq(1,2,1), 
     #                  RSF.performance)
  
                # Could use mclapply if not using Windows
                         
  pe.c = data.frame(do.call(rbind, pe.c))
  
  names(pe.c) = c('n.top', 'pe.external', 'c.external')
  
  saving.dir = paste0(work_dir,'/rdata_files')
  save(pe.c, file = paste(saving.dir, '/PEC_nestedRF_short_term.Rdata', sep = ''))
  
  print(proc.time() - ptmi)
}

print(proc.time() - ptm)
beep()

save(pe.c, file = paste(saving.dir, '/PEC_nestedRF.Rdata', sep = ''))




# # NESTED FOR PART OF TRAINING DATA (SPLITTING TRAINING INTO TRAIN-TESTING SETS)#######
# 
# # load training data
# loading.dir = paste(getwd(),'/rdata_files', sep = '')
# load(paste(loading.dir, '/all_training_training.Rdata', sep = ''))
# 
# 
# # load validation data
# load(paste(loading.dir, '/all_training_validation.Rdata', sep = ''))
# 
# all.train = all.train.train
# all.valid = all.train.valid
# 
# 
# 
# RSF.performance = function(numVar){
#   print(paste('number of variables  =', numVar))
#   
#   n.top = numVar
#   dat = all.train[, c('time', 'event',
#                       var.order[1:n.top])]
#   
#   # RF objects
#   set.seed(seed)
#   rf.obj = rfsrc(Surv(time, event)~.,
#                  data = dat,
#                  ntree = 1000,
#                  splitrule = 'logrank')
#   
#   # External
#   
#   # Prediction error and c-index at end time
#   rf.pe = pec::pec(list(rf.obj),
#                    formula = Hist(time, event)~1,
#                    data = all.valid,
#                    times = min(365.25*pt, 
#                                max(all.valid$time)), 
#                    exact = T,
#                    splitMethod = 'none',
#                    cens.model = 'marginal')
#   
#   
#   rf.c = cindex(list(rf.obj),
#                 formula = Hist(time, event)~1,
#                 data = all.valid,
#                 eval.times = min(365.25*pt, 
#                                  max(all.valid$time)),
#                 splitMethod = 'none',
#                 cens.model = 'marginal')
#   
#   pe.external = rf.pe$AppErr$rfsrc[
#     which(rf.pe$time == min(365.25*pt, 
#                             max(all.valid$time)))][1]
#   c.external = rf.c$AppCindex
#   
#   return(c(n.top, 
#            pe.external,
#            c.external))
# }
# 
# 
# 
# ptm = proc.time()
# numVariables = nrow(var.order)
# for (ind in 1:1){ # nUmber of events, here we only consider CVD (1 event)
#   print(paste('outcome', all.events[ind]))
#   
#   ptmi = proc.time()
#   pe.c.train.valid = lapply(c(1:19, seq(20, 100, by = 10), seq(110, numVariables, by = 20), numVariables),
#                       RSF.performance)
#   #lapply(seq(1,2,1), 
#   #                  RSF.performance)
#   
#   # Could use mclapply if not using Windows
#   
#   pe.c.train.valid = data.frame(do.call(rbind, pe.c.train.valid))
#   
#   names(pe.c.train.valid) = c('n.top', 'pe.train.valid', 'c.train.valid')
#   
#   save(pe.c.train.valid, file = paste(saving.dir, '/PEC_nestedRF_train_valid.Rdata', sep = ''))
#   
#   print(proc.time() - ptmi)
# }
# 
# print(proc.time() - ptm)
# beep()
# 
# save(pe.c.train.valid, file = paste(saving.dir, '/PEC_nestedRF_train_valid.Rdata', sep = ''))
# 
# 
