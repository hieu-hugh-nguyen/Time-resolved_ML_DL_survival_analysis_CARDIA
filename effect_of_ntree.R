#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())

# laad current directory
curr.dir = paste(getwd(), '/', sep = '')
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'

# load library
#
setwd(work_dir)

##### Load libraries:##################################################################
list.of.packages <- c('randomForestSRC', 'ggRandomForests','survival')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

#==========================================================================
# Get Survival Probability From Model Prediction
#==========================================================================

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



feature_space = within(feature_space, rm('SEX'))
feature_space = within(feature_space, rm('RACE'))

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







## START RUNNING: ###############################

data = data_y10
ntree_vector <- c(seq(1,90,10), seq(100,1000,20))
error_vector <- rep(NA,length = length(ntree_vector))

for (i in 1:length(ntree_vector)){
  set.seed(1999)
  rfsrc_object <- randomForestSRC::rfsrc(Surv(time,event)~., data, ntree =ntree_vector[i])
  error_tree <- rfsrc_object$err.rate[length(rfsrc_object$err.rate)]
  error_vector[i] = error_tree
  print(ntree_vector[i])
  
}

qplot(ntree_vector, error_vector, geom="line") + coord_cartesian(ylim = c(0.1, 0.5))


# ntree_vector2 <- c(seq(1100,10000,1000))
# error_vector2 <- error_vector
# for (i in 1:length(ntree_vector2)){
#   set.seed(1999)
#   rfsrc_object <- randomForestSRC::rfsrc(Surv(time,event)~., data %>% select(-ID), ntree =ntree_vector2[i])
#   error_tree <- rfsrc_object$err.rate[length(rfsrc_object$err.rate)]
#   error_vector2 <- append(error_vector2, error_tree)
#   print(ntree_vector2[i])
#   
# }
# 
# qplot(c(ntree_vector, ntree_vector2), error_vector2, geom="line") + coord_cartesian(ylim = c(0.0, 0.5))
# 

ntree_vector3 <- c(seq(1100,2000,100))
error_vector3 <- error_vector
for (i in 1:length(ntree_vector3)){
  set.seed(1999)
  rfsrc_object <- randomForestSRC::rfsrc(Surv(time,event)~., data, ntree =ntree_vector3[i])
  error_tree <- rfsrc_object$err.rate[length(rfsrc_object$err.rate)]
  error_vector3 <- append(error_vector3, error_tree)
  print(ntree_vector3[i])
  
}

qplot(c(ntree_vector, ntree_vector3), error_vector3, geom="line", xlab = 'Number of trees', ylab = 'OOB Error Rate') + 
  coord_cartesian(ylim = c(0.0, 0.5)) +
  geom_line(size =1.2)
