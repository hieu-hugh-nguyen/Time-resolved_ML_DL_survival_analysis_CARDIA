# or calculate via pec package:
# prob.surv = pec::predictSurvProb(trained.model, newdata = test.data, times = eval.times)


eval_performance2 = function(prob.risk.test.set, test.data, trained.data, eval.times){
  
  prob.risk = prob.risk.test.set
  prob.surv = 1-prob.risk.test.set
  # metrics of evaluation:
  #c-index
  cind.obj = pec::cindex(object = prob.surv,
                         formula = Surv(time, event)~1,
                         data = test.data,
                         eval.times = eval.times, 
                         splitMethod = 'none',
                         cens.model = 'marginal')
  #brier score: 
  pec.obj = pec::pec(object = prob.surv,
                     formula = Surv(time, event)~1,
                     data = test.data,
                     times = eval.times[-length(eval.times)],
                     exact = F,
                     splitMethod = 'none',
                     cens.model = 'marginal')
  
  # c-index and Brier at a single time:
  timepoint = 26 # 26 years
  last.brier = sapply(pec.obj$AppErr,
                      function(x) x[which(pec.obj$time == min(365.25*timepoint, max(pec.obj$time)))[1]])
  last.cindex = sapply(cind.obj$AppCindex,
                       function(x) x[which(cind.obj$time == min(365.25*timepoint, max(cind.obj$time)))[1]])
  
  # cumulative dynamic AUC and roc curve:
  require(riskRegression)
  roc = list()
  auc = c()
  for (timept in 1:(length(eval.times))){
    # survivalROC.curve= survivalROC(Stime=test.data$time,
    #                                status=test.data$event,
    #                                marker = prob.risk[,timept],
    #                                predict.time = eval.times[timept], method="KM")
    dat = data.frame(event = test.data$event, time = test.data$time, absolute_risk = prob.risk[,timept])
    
    riskRegression.roc = riskRegression::Score(list("model"=dat$absolute_risk)
                                               ,formula=Surv(time,event)~1
                                               ,data=dat
                                               ,conf.int=FALSE
                                               ,times=eval_times[timept]
                                               ,plots = c('ROC','calibration'))
    
    auc = append(auc, riskRegression.roc$AUC$score$AUC)
    roc = append(roc, list(riskRegression.roc))
  }
  # uno c-index:
  require('survAUC')
  # lp = predict(trained.model, type = 'lp')$predicted
  # lp.new = predict(trained.model, newdata = test.data)$predicted
  Surv.rsp = Surv(trained.data$time, trained.data$event)
  Surv.rsp.new = Surv(test.data$time, test.data$event)
  uno_c = survAUC::UnoC(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                        ,lpnew = prob.risk[,round(ncol(prob.risk)/2)])
  # uno AUC (similar trend to pec cindex, not exact though?)
  uno_auc = survAUC::AUC.uno(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                             ,lpnew = prob.risk[,round(ncol(prob.risk)/2)]
                             ,times = eval.times)
  iauc_uno = uno_auc$iauc
  # save:
  saving.dir = loading.dir
  performance = list(prob.risk, cind.obj, pec.obj, last.cindex, last.brier, roc, auc, uno_c, uno_auc
                     , iauc_uno)
  names(performance) = c('prob.risk.test.data', 'cind.obj', 'pec.obj', 'last.cindex', 'last.brier', 'roc', 'auc'
                         , 'uno.c', 'uno.auc', 'iauc.uno')
  return(performance)
}