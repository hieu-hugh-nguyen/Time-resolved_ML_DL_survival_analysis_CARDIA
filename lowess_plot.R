#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())

# laad current directory
# curr.dir = paste(getwd(), '/', sep = '')
work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'

# load library
#
# library(randomForestSRC)
# library(pec)
# library(riskRegression)
# library(survival)
# library(beepr)
# library(glmnet)
# library(MASS)
# library(doParallel)
# setwd(work_dir)

##### Load libraries:##################################################################
list.of.packages <- c('randomForestSRC', 'ggRandomForests','survival', 'ggplot2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

#==========================================================================
# Get Survival Probability From Model Prediction
#==========================================================================

# load model objects
fold = 1
# loading.dir = paste0(work_dir,'/rdata_files/rsf_all_vars_after_y10_origin_at_10_2nd_fold_',fold)
# trained.model = get(load(paste0(loading.dir, '/rsf_all_vars_after_y10_origin_at_10_2nd.RData')))

# loading.dir = paste0(work_dir,'/rdata_files/rsf_all_var_short_term_fold_',fold)
# trained.model = get(load(paste0(loading.dir, '/rsf_all_var_short_term.RData')))

loading.dir = paste0(work_dir,'/rdata_files/rsf_all_var_long_term_fold_',fold)
trained.model = get(load(paste0(loading.dir, '/rsf_all_var_long_term.RData')))

# load the datasetloading_dir = paste0(work_dir, '/csv_files')
# work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun'
# loading_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/csv_files'
# feature_space = read.csv(paste0(loading_dir,'/feature_space_updated_years_having_conditions.csv'), stringsAsFactors = FALSE)
# label_space = read.csv(paste0(loading_dir,'/y5_cvd_outcome','.csv'))
# #label_space = read.csv(paste0(loading_dir,'/y5_mortality_outcome','.csv'))
# ascvd_data = read.csv(paste0(loading_dir,'/ascvd_calc_with_id','.csv'))
# names(ascvd_data)[1] = 'ID'
# # convert race and sex to {0,1} type:
# ascvd_data$sex = ifelse(ascvd_data$sex == 'Male', 0, 1)
# ascvd_data$race = ifelse(ascvd_data$race == 'White', 0, 1)
# 
# #Rename labels for name and time to event for better generalization later on:
# names(label_space)[[2]] = "event"   
# names(label_space)[[3]] = "time"  
# 
# 
# 
# feature_space = within(feature_space, rm('SEX'))
# feature_space = within(feature_space, rm('RACE'))
# 
# data_full = merge(label_space, ascvd_data, by = 'ID')
# data_full = merge(data_full, feature_space, by = 'ID')
# 
# rm(feature_space, label_space, ascvd_data)
# 
# # Remove (almost duplicated) correlated variables (blood pressure from echo and from anthropometry: 
# data_full = within(data_full, rm('C40DBP','C40SBP', 'age', 'ascvd', 'sbp'))
# # Correct for wrong entry of smoking years: 
# data_full$C09SMKYR[which(data_full$C09SMKYR == 88)] = 0
# 
# 
# #Check if there is any character column, then delete them to make sure all data is numeric:
# nums = unlist(lapply(data_full, is.character))  
# data_full[,nums]=NULL
# 
# #Exclude time to event <0:
# positiveTimetoEvent = data_full$time>=0
# data_full = data_full[positiveTimetoEvent,]


# # load variable description:
# loading.dir <- paste0(work_dir, '/csv_files')
# var_dict <- read.csv(paste0(loading_dir,
#                             '/y5_all_vars_dictionary_with_correct_labels','.csv'), stringsAsFactors = FALSE)
# 
# # y5_all_vars_dictionary_manually_selecting_vars (1)
# col_names <- data_full %>% names() %>% as_tibble() %>% rename(variable = value) %>% 
#   left_join(var_dict %>% rename(variable = Variable.Name) %>% dplyr::select(variable, Variable.Label)
#             , by = 'variable') 
# col_names$Variable.Label <- ifelse(is.na(col_names$Variable.Label), col_names$variable, col_names$Variable.Label)
# 
# var_top_n_var_des_df <- col_names %>% filter(variable %in% var_top_n) %>% dplyr::select(Variable.Label)
# var_top_n_var_des <- var_top_n_var_des_df$Variable.Label
# 
# 
# data_full_var_des <- data_full
# names(data_full_var_des) <- col_names$Variable.Label

# rfsrc_object <- randomForestSRC::rfsrc(Surv(time,event)~., data_full, ntree = 1001)



# Plot the marginal effect for the top variables:

marginal_object <- gg_variable(trained.model)
#marginal_object2 <- gg_variable(rfsrc_object)
# marginal_object_var_des <- gg_variable(rfsrc_object_var_des)

#rfsrc_object$xvar.names <- col_names$Variable.Label[1:length(rfsrc_object2$xvar.names)] 


# get the top var:
loading_dir = paste0(work_dir,'/csv_files')

# var_order = read.csv(file = paste0(loading_dir,
#                                    '/averaged_outerloop_VIMP_y10_origin_at_10_2nd.csv'))

# var_order = read.csv(file = paste0(loading_dir,
#                                    '/averaged_outerloop_VIMP_short_term.csv'))
var_order = read.csv(file = paste0(loading_dir,
                                   '/averaged_outerloop_VIMP_long_term.csv'))

var_order = apply(var_order, 2, as.character)
var_order = var_order[,2]

n_top = 20
var_top_n = var_order[1:n_top]
# var_top_n = var_top_n[!(var_top_n %in% c('C09SMKAG_YEAR'))]
# var_top_n = var_top_n[!(var_top_n %in% c('HBP05'))]

plot(marginal_object, xvar=var_top_n, panel=TRUE, se=TRUE
     , method = 'loess', span = 1, size =1, points = FALSE) + theme_minimal()+
  geom_rug(sides="b") + coord_cartesian(ylim = c(0.5, 1)) + labs(y = 'Survival Probability', x = 'Value') +
  theme( axis.text=element_text(size=13),
        axis.title=element_text(size=14),
        # legend.title = element_text(color = "black", size = 17),
        # legend.text = element_text(color = "black", size = 17))
  )


marginal_object_factor <- marginal_object

marginal_object_factor$C37DISTU <- as.factor(marginal_object_factor$C37DISTU)
# or
# marginal_object[,"C37DISTU"] <- factor(marginal_object[,"C37DISTU"])


# # Have to plot categorical variables separately (ggRnadomForest doesn't support combining continuous and categorical yet)
# plot(marginal_object_factor, xvar="C37DISTU", points = FALSE) 

# to remove points on categorical variables:
ggplot(marginal_object_factor, aes(C37DISTU, yhat)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0.5, 1)) + labs(y = '', x = '') + theme_minimal() +
  theme( axis.text=element_text(size=11))


