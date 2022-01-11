# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
#work_dir= 'C:/Users/HIEU/Desktop/CARDIA project/Git'
work_dir= 'U:/HIEU/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)

# load libraries:
list.of.packages <- c('survminer', 'cmprsk', "mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

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



ci_fit <- 
  cuminc(
    ftime = data_full$time/365.25, 
    fstatus = data_full$event, 
    #cencode = 2
  )


cif_plot <- ggcompetingrisks(
  fit = ci_fit, 
  multiple_panels = FALSE,
  xlab = "Years After Exam 3",
  ylab = "Cumulative Incidence",
  main = NULL,
  ylim = c(0, 0.20),
  ggtheme = theme_gray(base_size = 14),
  legend = "none",
  
) + geom_line(size =1.5)
cif_plot


# Add table:
mel_fit <- survfit(
  Surv(time/365.25, event) ~ 1, 
  data = data_full
)
num <- ggsurvplot(
  fit = mel_fit, 
  risk.table = TRUE, 
  risk.table.y.text = FALSE,
  ylab = "CIF",
  xlab = 'Years After Exam 3',
  risk.table.fontsize = 4.5,
  tables.theme = theme_survminer(font.main = 14),
) 

cowplot::plot_grid(
  cif_plot, 
  num$table, 
  nrow = 2, 
  rel_heights = c(4, 1), 
  align = "v", 
  axis = "b"
)