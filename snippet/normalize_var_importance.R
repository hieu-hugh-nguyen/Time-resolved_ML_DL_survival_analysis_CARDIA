# function:
# normalize variable importance ranked by random forest  
# scale from 0 (most important) to 1 (least important)
# input: a data frame x with 1st col = var name, 2nd col = a metric
# of importance (i.e: tree depth)
# output: a vector of normalized value
normalize_var_imp = function(imp){
  normalized_imp = (imp - min(imp))/(max(imp)-min(imp))
  
}

