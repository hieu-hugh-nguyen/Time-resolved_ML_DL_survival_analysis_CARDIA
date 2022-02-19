#==========================================================================
# Stratified Sampling
# to mantain class balance (equal event:nonevent ratio), sampling needs to
# be run for different diseases (cvd, afib, stroke, ect.)
# 
# For outerloop split of training-validing data:
# 5 folds (80 training : 20 validing ratio) x 5 times
# Output: returns the IDs for each training-validing split 
#==========================================================================
rm(list=ls()) #Clear all
cat("\014")

list.of.packages <- c('dplyr', 'tibble')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

# set spliting parameters:
nfold = 5 # 5-fold cross-validation
num_split_times = 5 # 5 times of 5-fold cross-validation

work_dir = "U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2"

setwd(work_dir)


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')
feature_space = read.csv(paste0(loading_dir,'/y5_imputed_unsupervised_Dec_2021','.csv'), stringsAsFactors = FALSE)

label_space = read.csv(paste0(loading_dir,'/y5_cvd_outcome','.csv'))
ascvd_data = read.csv(paste0(loading_dir,'/ascvd_calc_with_ID','.csv'))
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


#rm(feature_space, label_space, ascvd_data)

# Remove (almost duplicated) correlated variables (blood pressure from echo and from anthropometry: 
data_full = within(data_full, rm('C40DBP','C40SBP', 'age', 'ascvd', 'sbp'))
data_full = data_full %>% dplyr::select(-C09SMKAG_YEAR)
# Correct for wrong entry of smoking years: value of 88 on data collection form means 0 years  
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


# Stratified sampling: ######################################################################



stratified_event_split <- function(data){
  
  seed = seq(4995,4995+4,1)
  
  all.train = NULL
  all.valid = NULL
  
  #Rename labels for name and time to event for better generalization later on;
  names(data)[[2]] = "event";   
  names(data)[[3]] = "time";  
  
  
  #Check if there is any character column, then delete them to make sure all data is numeric:
  nums <- unlist(lapply(data, is.character))  
  data[,nums]=NULL
  
  #Exclude time to event <0:
  positiveTimetoEvent = data$time>=0
  data = data[positiveTimetoEvent,]
  
  # START SAMPLING: ###############################################
  
  ID.event = data[data$event == 1,'ID']
  ID.nonevent = data[data$event == 0,'ID']
  
  # stratified sample size
  num.event = dim(ID.event)[1]
  num.nonevent = dim(ID.nonevent)[1]
  
  train.ID.df = data.frame()  
  valid.ID.df = data.frame()
  
  for( times in 1:num_split_times ){
    set.seed(seed[times])
    #Randomly shuffle the data
    ID.event = ID.event[sample(length(ID.event))]
    ID.nonevent = ID.nonevent[sample(length(ID.nonevent))]
    #Create equally-sized folds
    folds.event = cut(seq(1,length(ID.event)),breaks=nfold,labels=FALSE)
    folds.nonevent = cut(seq(1,length(ID.nonevent)),breaks=nfold,labels=FALSE)
    
    for(fold in 1:nfold){
      #Segement your data by fold using the which() function 
      valid.event.indices = which(folds.event==fold,arr.ind=TRUE)
      valid.nonevent.indices = which(folds.nonevent==fold,arr.ind=TRUE)
      
      valid.event.ID = ID.event[valid.event.indices]
      valid.nonevent.ID =  ID.nonevent[valid.nonevent.indices]
      valid.ID = c(valid.event.ID, valid.nonevent.ID)
      
      train.event.ID = ID.event[-valid.event.indices]
      train.nonevent.ID =  ID.nonevent[-valid.nonevent.indices]
      train.ID = c(train.event.ID, train.nonevent.ID)
      
      cbind.fill <- function(...){
        nm <- list(...) 
        nm <- lapply(nm, as.matrix)
        n <- max(sapply(nm, nrow)) 
        do.call(cbind, lapply(nm, function (x) 
          rbind(x, matrix(, n-nrow(x), ncol(x))))) 
      }
      train.ID.df= cbind.fill(train.ID.df, train.ID)
      valid.ID.df= cbind.fill(valid.ID.df, valid.ID)
    }
  }
  return(list(train.ID = train.ID.df, valid.ID = valid.ID.df))
}


saving.dir = paste0(work_dir,'/rdata_files')

ID.split.y10 = stratified_event_split(data_y10)
train.ID.y10 = ID.split.y10$train.ID
valid.ID.y10 = ID.split.y10$valid.ID

save(train.ID.y10, file = paste(saving.dir, '/all_training_ID_outerloop_cohort_0_10.Rdata', sep = ''))
save(valid.ID.y10, file = paste(saving.dir, '/all_validation_ID_outerloop_cohort_0_10.Rdata', sep = ''))

write.csv(train.ID.y10, paste0(work_dir, '/csv_files','/all_training_ID_outerloop_cohort_0_10','.csv')
          ,row.names = F)

ID.split.y10.y26 = stratified_event_split(data_after_y10_origin_at_10)
train.ID.y10.y26 = ID.split.y10.y26$train.ID
valid.ID.y10.y26 = ID.split.y10.y26$valid.ID

save(train.ID.y10.y26, file = paste(saving.dir, '/all_training_ID_outerloop_cohort_10_26.Rdata', sep = ''))
save(valid.ID.y10.y26, file = paste(saving.dir, '/all_validation_ID_outerloop_cohort_10_26.Rdata', sep = ''))

write.csv(train.ID.y10.y26, paste0(work_dir, '/csv_files','/all_training_ID_outerloop_cohort_10_26','.csv')
          ,row.names = F)
