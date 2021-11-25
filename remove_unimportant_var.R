rm(list=ls()) #Clear all
cat("\014")

require('readxl')

work_dir = 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
loading.dir = paste0(work_dir,'/csv_files')
feature_space = read.csv(paste0(loading.dir,'/Y5_unimputed_featurespace.csv'), stringsAsFactors = F)
var.dictionary.df = read.csv(paste0(loading.dir,'/Y5_all_vars_dictionary.csv'), stringsAsFactors = F)

#transfer all labels from var.dictionary.df to feature_space:
label = (var.dictionary.df[,2])
for (i in 1:ncol(feature_space)) feature_space[1,i] = label[i]


var.dictionary.updated = read_excel(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars.xlsx"))

unimportant_vars = var.dictionary.updated$`Variable Name`[which(var.dictionary.updated$`Include=1, exclude=0` == 0)]

feature_space_unimportant_vars_excluded = feature_space[, -which(names(feature_space) %in% unimportant_vars)]


# check if there is any NA label, fill them in:

which(feature_space_unimportant_vars_excluded[1,] == "")
# make sure labels match with variable names before moving on! 



saving.dir = "U:/Hieu/CARDIA_project/csv_files"
write.csv(feature_space_unimportant_vars_excluded, file = paste0(saving.dir, '/feature_space_unimportant_vars_excluded.csv'), row.names = F)
