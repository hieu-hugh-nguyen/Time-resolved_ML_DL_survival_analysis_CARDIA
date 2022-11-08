# Lifetime vs 10-year Cardiovascular Disease Prediction in Young Adults Using Statistical Machine Learning and Deep Learning: The CARDIA Study*

This repository is the code base for the following paper: 
Nguyen, H.T., Venkatesh, B.A., Reis, J.P., Wu, C.O., Carr, J., Nwabuo, C., Gidding, S.S., Guallar, E. and Lima, J.A., 2022. Lifetime vs 10-year Cardiovascular Disease Prediction in Young Adults Using Statistical Machine Learning and Deep Learning: The CARDIA Study. medRxiv.

Key points from this paper:
Question: Do machine learning (ML) and deep learning (DL)-based survival analysis models help differentiate 10-year versus lifetime predictors of cardiovascular (CV) outcomes in young adults? 

Findings: In this retrospective analysis of 4314 participants in the CARDIA study, ML and DL survival analysis improved CVD risk prediction over traditional Cox models and revealed the top 20 predictors among 449 variables. Top 10-year risk predictors include kidney disease, family history of CV disease, and echocardiographic parameters, where as traditional risk factors and indices of body size featured heavily as top predictors of lifetime CV risk.   

Meaning: Family history, kidney disease, and subclinical phenotyping of CVD using echocardiography play a prominent role for 10-year risk estimation, while traditional CV risk factors alone may be adequate in estimating lifetime CV risk in young adults.

Data: https://www.cardia.dopm.uab.edu/ 

## WORKFLOW CODE:

MAIN MANUSCRIPT: 

Start: extract_label_space.R
Extract and save information about CVD and mortality event and time to event from y5 exam (y5 as starting point)


--> year_5_datacompile.R
Extract, organize, and compile variables from y5 data from SAS reports for subjects in the cohort 
Filter out subjects who did not visit y5 exam (without exam 3 calculated age)
Generate a big data sheet containing n x p with n = number of observations, p = # variables (y5_feature_space_with_label).
Create the variable dictionary .csv file (y5_all_vars_dictionary).


--> remove_unimportant_var.R
Review the variable dictionary, decide which variables to exclude, if any (because of high percentage of missing values, or because they absolutely are not clinical relevant). Exclude those vars from the feature space.


--> y5_all_vars_dictionary_manually_selecting_vars (3).csv
Manually review the dictionary spreadsheet to double check each var, decide how to fill in missing data for each var


--> manually_fill_NA.R
Keep excluding Unimportant Variables 
Fill in missing (NA) values to zero, one, 99 in some selected variables as appropriate after discussion with the study physicians 


--> transform_age_variables_to_#_years.R
Convert age variables to years variables 
(e.g. from variable 'age HP diagnosed' to 'No. years having HP up to Exam Y5')


--> impute.R
Exclude subjects with death or cvd event prior to y5
Remove VLDL variable as this variable did not make sense after discussion with the study physicians
Exclude subjects with missing values in more than 25% of the total variables
Set categorical variables as factors
Encode binary variables of two levels (1 and 2) to two levels (0 and 1) - will be helpful for intepretation of cofficients in the models 
Impute missing data


-->  data_split_for_outer_loop_5_by_5_CV_short_term_n_long_term_prediction.R
Split data into training and testing sets with stratified sampling (split data into training and validation sets with equal case:non-case ratio). 5-fold x 5 times cross validation was employed. Long-term and short-term were done seperately as they were two differerent cohorts (the long-term cohort removed extra subjects whom had been censored or had a CVD event by 10 years after Exam Y5).




Start the modeling process:

--> VIMP_long_term_vs_short_term_prediction.R
Train and evaluate RSF models using all variables
Obtain ranking of variables in order of importance (VIMP) using minimal depth of maximal subtree


--> avp_VIMP.R
Average variable importance over the 25 folds. Return as csv files for Table 2 manuscript (variable importance)
output: average_outerloop_VIMP_short_term; average_outerloop_VIMP_long_term


--> Train and evaluate models:

All the following files are programs to train and evaluate models on three variable subsets (all variables, top-20 variables, and ASCVD variables (9 traditional risk factors from the ASCVD Pooled Cohort Equation).

For statistical and machine learning models: 
Train, test, and evaluate performance of models:
-running_models_ascvd_var_long_term_vs_short_term.R
-running_models_top_20_var_long_term_vs_short_term.R
-running_models_all_var_long_term_vs_short_term.R

For neural network models:
Train and test models:
-running_nnet_survival_long_term_vs_short_term.ipynb
-running_cox_nnet_long_term_vs_short_term.ipynb
-running_DeepHit_long_term_vs_short_term.ipynb
-running_DeepSurv_long_term_vs_short_term.ipynb

Evaluate performance of models:
-eval_models_nnet_survival_all_var_long_term_vs_short_term.R
-eval_models_nnet_survival_ascvd_var_long_term_vs_short_term.R
-eval_models_nnet_survival_top_20_var_short_term_vs_short_term.R
-eval_models_cox_nnet_all_var_long_term_vs_short_term.R
-eval_models_cox_nnet_ascvd_var_long_term_vs_short_term.R
-eval_models_cox_nnet_top_20_var_long_term_vs_short_term.R
-eval_models_deephit_all_var_long_term_vs_short_term.R
-eval_models_deephit_ascvd_var_long_term_vs_short_term.R
-eval_models_deephit_top_20_var_long_term_vs_short_term.R
-eval_models_deepsurv_all_var_long_term_vs_short_term.R
-eval_models_deepsurv_ascvd_var_long_term_vs_short_term.R
-eval_models_deepsurv_top_20_var_long_term_vs_short_term.R



--> aggregate_testset_results.R
Aggregate the performance of each model over 25 folds, output the summarized performance for each model (median, mean, confidence interval in AUC, C-index, and Brier Score).
The values of this file are reported in Table 2 of the manuscript and Tables S3-S5 in the Supplements


--> Plotting figures:
Main figures:
-plot_performance_over_time.R: performance over time of the best models in each variable category (figure 2 of manuscript)
-plot_VIMP_waterfall.R: waterfall plot of variable importance (figure 3 of manuscript)
-plot_lowess.R plot of the marginal effect on survival probability of the top-20 predictors for lifetimeCVD outcome prediction (figure 4 of manuscript)








MANUSCRIPT SUPPLEMENTARY MATERIALS: 

Supplementary analysis (table S6-S9):
Variable importance for all years (0-26 years after Exam Y5):
-data_split_for_outer_loop_5_by_5_CV_all_years.R: splitting data for all year prediction (instead of long/short term separation)
-VIMP_all_years_prediction.R: training and testing RSF models for all years prediction, then extract variable importance ranking from minimal depth of maximal subtree
-outer_loop_permute_ranking.R: variable importance using permutation test and averaging over the 25 folds  
-avg_VIMP_all_years.R: average variable importance from minimal deph of maximal subtree over the 25 folds

Variable importance for all years separated by race and gender (0-26 years after Exam Y5):
-data_split_for_outer_loop_5_by_5_CV_all_years_separated_by_race_n_gender.R: splitting data for all year prediction (instead of long/short term separation)
-VIMP_all_years_prediction_separated_by_race_n_gender.R: training and testing RSF models for all years prediction, then extract variable importance ranking from minimal depth of maximal subtree
-outer_loop_permute_ranking.R: variable importance using permutation test and averaging over the 25 folds  
-avg_VIMP_all_years.R: average variable importance from minimal deph of maximal subtree over the 25 folds
 

Supplementary figures:
-cif_plot.R: Cumulative Incidence Function (CIF) for CVD outcome (figure S2)
-plot_pie_chart_variable_avai.R and pie_chart_data_avai.xlsx: pie chart of data availability percentages (figure S1)
-effect_of_ntree_plot.R: investigate and plot effect of number of trees in RSF to the error rate (figure S3)
-nestedRSF.R: investigate the effect of no. of predictors on RSF model performance (figure S4 & S5)


Miscellanea: 
The '/snippet' folder: contains helper functions for the excecution of the above code files in the main folder 
The '/csv_files' folder: contains the variable dictionaries that entail all excluded and included variables
