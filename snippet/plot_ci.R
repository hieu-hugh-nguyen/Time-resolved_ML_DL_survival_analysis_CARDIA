
plot_ci = function(performance.report.dynamic){
  quant = list()
  for (i in 1:14){
    quant[[i]] = quantile(na.omit(performance.report.dynamic$dynamic.cindex.all[,i], probs = c(0, 0.05, 0.5, 0.95, 1)))
  }
  firstquantile = c()
  thirdquantile = c()
  median = c()
  for (i in 1:14){
    firstquantile = append(firstquantile, quant[[i]][2])
    thirdquantile = append(thirdquantile, quant[[i]][4])
    median = append(median, quant[[i]][3])
  }
  endpt = 26; 
  eval.times = 365.25*c(1, seq(2, endpt, by = 2))
  plot(eval.times/365.25, firstquantile[1:14], type = 'l', col = 'blue'
       ,ylim = c(0.5, 1)
       ,xlab = 'Years after Exam 3'
       ,ylab = 'dynamic AUC'
  )
  lines(eval.times/365.25, thirdquantile[1:14], col = 'blue')
  lines(eval.times/365.25, median, col = 'red')
  abline(h=0.8,lty=2)
  #abline(h=0.7,lty=2)
}

loading.dir = paste0(getwd(),'/rdata_files')
(load(paste0(loading.dir, '/performance_cForestEnsemble_30_var.RData')))
plot_ci(performance.report.dynamic)
