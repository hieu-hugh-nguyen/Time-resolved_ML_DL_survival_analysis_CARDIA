# obtain the median of a performance measure (cindex, auc, or brier) for each evaluated time point

get_median = function(performance.report.dynamic, metric = 'cindex',term_pred = 'short_term'){
  quant = list()
  if (term_pred == 'short_term'){
    pred_year = 1:6
  }
  if (term_pred == 'long_term'){
    pred_year = 1:8
  }
  
  for (i in pred_year){
    if (metric == 'cindex'){
      quant[[i]] = quantile(na.omit(performance.report.dynamic$dynamic.cindex.all[,i]))  
    }
    if (metric == 'auc'){
      quant[[i]] = quantile(na.omit(performance.report.dynamic$dynamic.auc.all[,i]))  
    }
    
  }
  #firstquantile = c()
  #thirdquantile = c()
  median = c()
  for (i in pred_year){
    #firstquantile = append(firstquantile, quant[[i]][2])
    #thirdquantile = append(thirdquantile, quant[[i]][4])
    median = append(median, quant[[i]][3])
  }
  names(median) = NULL
  return(median)
}


get_mean = function(performance.report.dynamic, metric = 'cindex', term_pred = 'short_term'){
  quant = list()
  if (term_pred == 'short_term'){
    pred_year = 1:6
  }
  if (term_pred == 'long_term'){
    pred_year = 1:8
  }
  
  for (i in pred_year){
    if (metric == 'cindex'){
      quant[[i]] = mean(na.omit(performance.report.dynamic$dynamic.cindex.all[,i]))  
    }
    if (metric == 'auc'){
      quant[[i]] = mean(na.omit(performance.report.dynamic$dynamic.auc.all[,i]))  
    }
    
  }
  #firstquantile = c()
  #thirdquantile = c()
  mean = c()
  for (i in pred_year){
    #firstquantile = append(firstquantile, quant[[i]][2])
    #thirdquantile = append(thirdquantile, quant[[i]][4])
    mean = append(mean, quant[[i]])
  }
  names(mean) = NULL
  return(mean)
}


