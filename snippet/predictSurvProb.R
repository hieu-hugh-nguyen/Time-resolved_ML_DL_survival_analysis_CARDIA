
predictRisk.rsf = function(trained.model, newdata, times, ...){
  ptemp = predict(trained.model,newdata=newdata,importance="none",...)$survival
  pos = prodlim::sindex(jump.times=trained.model$time.interest,eval.times=times)
  p = cbind(1,ptemp)[,pos+1,drop=FALSE]
  if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",NROW(newdata)," x ",length(times),"\nProvided prediction matrix: ",NROW(p)," x ",NCOL(p),"\n\n",sep=""))
  p
  return(1-p)
}

predictRisk.coxBoost = function(trained.model, newdata, times){
  #input is a mlr coxBoost object
  require(CoxBoost)
  prob.risk = predict(trained.model$learner.model, newdata = newdata 
                      ,times = times
                      ,type = 'CIF') # CoxBoost::predict(): the predicted probability of having had the event of interest.
  
  return(prob.risk)
}
predictRisk.coxBoost2 = function(trained.model, newdata, times){
  require(pec)
  prob.surv = predictSurvProb(trained.model, newdata=newdata,
                              times=times,
                              cause=1)
  return(1-prob.surv)
}

predictRisk.cForest = function(trained.model, newdata, times){
  require(pec)
  #require(mlr)
  prob.surv = pec::predictSurvProb(trained.model
                                        , newdata = newdata
                                        , times = times)
  return(1-prob.surv)
}

predictRisk.aic = function(trained.model, newdata, times){
  prob.surv = pec::predictSurvProb(object = trained.model
                                   , newdata = newdata
                                   , times = times)
  return(1-prob.surv)
}

predictRisk.cox = function(trained.model, newdata, times){
  require(riskRegression)
  prob.risk = riskRegression::predictRisk(trained.model, newdata = newdata
                                          , times = times)
  return(prob.risk)
  #probs.cox = pec::predictSurvProb(cox20, newdata = test.dat, times = cutoff)
}

predictRisk.rpart = function(trained.model, newdata, times){
  prob.surv = pec::predictSurvProb(object = trained.model
                                      , newdata = newdata
                                      , times = times)
  return(1-prob.surv)
}

predictRisk.gbm = function(object, newdata, times, n.trees = NULL, traindata = NULL) {
  require(gbm)
  #library(gbm, lib.loc = "C:/Users/hnguye78/Documents/R/R-3.5.1/library/gbm_github")

  trained.model = object
  if (is.null(traindata)) traindata =  gbm::reconstructGBMdata(object)
  if (is.null(n.trees)) n.trees = object$n.trees
  p = matrix(0, NROW(newdata), length(times))
  xb.train = predict(object ,newdata = traindata, n.trees = n.trees)
  H2 = gbm::basehaz.gbm(t = traindata[, 'time'], 
                         delta = traindata[, 'event'], 
                         f.x = xb.train, t.eval = times)
  xb.test = predict(object, newdata = newdata , n.trees = n.trees ) 
  for (i in 1:length(times)) p[,i] = exp(-H2[i] * exp(xb.test))
  p[,times==0] = 1
  
  for (i in 2:ncol(p)){
    if (is.na(p[,i])) { p[,i] = p[,i-1]} 
  } 

  if(trained.model$distribution != 'sci'){ 
    p = 1-p
    p[is.na(p)] = 0
    return(p)
  } else{ # reverse results for sci 
    p[is.na(p)] = 0
    return(p)
  }
}

predictRisk.glmboost = function(object, newdata, times, traindata, n.trees = NULL) {
  require(gbm)
  require(mboost)
  #library(gbm, lib.loc = "C:/Users/hnguye78/Documents/R/R-3.5.1/library/gbm_github")
  
  lp.glmboost = predict(object, newdata = traindata, type = "link")
  H2 = gbm::basehaz.gbm(t = traindata[, 'time'], 
                         delta = traindata[, 'event'], 
                         f.x = lp.glmboost, t.eval = times)
  xb.test = predict(object, newdata = newdata ,type = 'link' ) 
  p = matrix(0, NROW(newdata), length(times))
  for (i in 1:length(times)) p[,i] = exp(-H2[i] * exp(xb.test))
  p[,times==0] = 1
  for (i in 2:ncol(p)){
    if (is.na(p[,i])) { p[,i] = p[,i-1]} 
  } 
  
  return(1-p)
  
}

predictRisk.ranger = function(object, newdata, times, ...){
  ptemp = 1-stats::predict(object,data=newdata, ...)$survival
  pos = prodlim::sindex(jump.times=ranger::timepoints(object),eval.times=times)
  p = cbind(1,ptemp)[,pos+1,drop=F]
}

predictRisk.glmnet = function() {
  
}

predictRisk.survfit = function(object, newdata, times,...){
  p = 1-predictSurvProb(trained.model
                    , newdata=newdata
                    , times=times
                    , cause=1)
  return(p)
}  