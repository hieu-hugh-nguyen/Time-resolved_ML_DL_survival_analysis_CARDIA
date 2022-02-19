#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())

# laad current directory
# curr.dir = paste(getwd(), '/', sep = '')
work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'


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

loading.dir = paste0(work_dir,'/rdata_files/rsf_all_var_long_term_fold_',fold)
trained.model = get(load(paste0(loading.dir, '/rsf_all_var_long_term.RData')))


# Plot the marginal effect for the top variables:

marginal_object <- gg_variable(trained.model)


# get the top var:
loading_dir = paste0(work_dir,'/csv_files')


# var_order = read.csv(file = paste0(loading_dir,
#                                    '/averaged_outerloop_VIMP_short_term.csv'))
var_order = read.csv(file = paste0(loading_dir,
                                   '/averaged_outerloop_VIMP_long_term.csv'))

var_order = apply(var_order, 2, as.character)
var_order = var_order[,2]

n_top = 20
var_top_n = var_order[1:n_top]

plot(marginal_object, xvar=var_top_n, panel=TRUE, se=TRUE
     , method = 'loess', span = 1, size =1, points = FALSE) + theme_minimal()+
  geom_rug(sides="b") + coord_cartesian(ylim = c(0.5, 1)) + labs(y = 'Survival Probability', x = 'Value') +
  theme( axis.text=element_text(size=13),
        axis.title=element_text(size=14),
        # legend.title = element_text(color = "black", size = 17),
        # legend.text = element_text(color = "black", size = 17))
  )


### Have to plot categorical variables separately (ggRnadomForest doesn't support combining continuous and categorical yet)

marginal_object_factor <- marginal_object
marginal_object_factor$C37DISTU <- as.factor(marginal_object_factor$C37DISTU)

# plot(marginal_object_factor, xvar="C37DISTU", points = FALSE) 

# to remove points on categorical variables:
ggplot(marginal_object_factor, aes(C37DISTU, yhat)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0.5, 1)) + labs(y = '', x = '') + theme_minimal() +
  theme( axis.text=element_text(size=11))


