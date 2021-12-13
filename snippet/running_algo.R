# Running different algorithms:
#
#
#

#Cox model for top 20 variables:
running_coxph = function(data){
  # load library
  # list.of.packages <- c("riskRegression",'survival','beepr', 'ggplot2', 'tibble', 'mlr','dplyr','ggfortify','survAUC')
  # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # if(length(new.packages)) install.packages(new.packages)
  # lapply(list.of.packages, require, character.only = T)
  #parallelMap::parallelStartSocket(2)
  
  # surv.task = makeSurvTask(data = data, target = c("time", "event"))
  # surv.lrn = mlr::makeLearner("surv.coxph")
  # model.coxph = mlr::train(surv.lrn, surv.task)
  model.coxph = coxph(Surv(time,event) ~., data = data, singular.ok = T, x=TRUE)
  return(model.coxph)
}

running_AICcox = function(data){
  require(MASS)
  require(survival)
  model.coxph = coxph(Surv(time, event)~., data = data, singular.ok = T, x= T)
  model.AIC = MASS::stepAIC(model.coxph,trace= T)
  return(model.AIC)
}

running_glmnet = function(data){ 
  require(mlr)
  surv.lrn.glmnet = mlr::makeLearner("surv.glmnet")
  surv.task = makeSurvTask(data = data, target = c("time", "event"))
  model.glmnet = mlr::train(surv.lrn.glmnet, surv.task)
  return(model.glmnet)
}
#lp.glmnet = predict(model.glmnet, newdata = test.dat)

running_lasso = function(data){
  require(glmnet)
  require(plyr)
  require(dplyr)
  x = model.matrix(Surv(time,event)~., data)
  y = with(data, Surv(time, event))
  # LASSO objects
  lasso.obj = glmnet(x, y, family = 'cox',
                     alpha = 1, standardize = F, 
                     nlambda = 100)
  #Cross validation, default is 10 fold:
  lasso.cv = cv.glmnet(x, y, family = 'cox',
                       alpha = 1, standardize = F,
                       nfold = 10,
                       nlambda = 100)
  lasso.cv.1 = cv.glmnet(x, y, family = 'cox',
                         alpha = 1, standardize = F,
                         nfold = 10,
                         nlambda = 100)
  lasso.cv.2 = cv.glmnet(x, y, family = 'cox',
                         alpha = 1, standardize = F,
                         nfold = 10,
                         nlambda = 100)
  ave.cvm = colMeans(plyr::rbind.fill(data.frame(t(lasso.cv$cvm)), data.frame(t(lasso.cv.1$cvm)), data.frame(t(lasso.cv.2$cvm)))
                     , na.rm = F)
  
  lambda.min = lasso.cv$lambda[which(ave.cvm == min(ave.cvm, na.rm = T))]
  lasso.coef = predict(lasso.obj, type = 'nonzero',
                       s = lambda.min) #type nonzero returns an indice list of non-zero coeffs
  #lambda.min is the lambda value that results in mininum cross validation err
  if (is.null(unlist(lasso.coef))) {
    print(paste('Lasso Cox model is null'))
    return(NULL)
  } else {
    lasso.coef = colnames(x)[lasso.coef[,1]] #return var name
    lasso.model = (coxph(as.formula(paste('Surv(time, event)~',
                                          paste(lasso.coef, collapse = '+'),
                                          sep = '')),
                         data = data,
                         singular.ok = T,
                         x=TRUE))
    return(lasso.model)
    
  }
}


running_coxBoost = function(data){
  require(mlr)
  require(CoxBoost)
  surv.lrn.coxBoost = mlr::makeLearner("surv.cv.CoxBoost")
  surv.task = makeSurvTask(data = data, target = c("time", "event"))
  model.coxBoost = mlr::train(surv.lrn.coxBoost, surv.task)
  return(model.coxBoost)
}

running_coxBoost2 = function(data){
  require(pec)
  require(CoxBoost)
  require(survival)
  matrix.covariates = as.matrix(sapply(data[,-c(1,2)], as.numeric))  
  time = data$time
  status = data$event
  # Find optimal penalty hyperparameter:
  optim.res = optimCoxBoostPenalty(time,status,x=matrix.covariates,trace=TRUE
                                   ,start.penalty=500) 
  op.penalty = optim.res$penalty 
  
  # Determine optimal boosting steps by cross validation
  cv.res = cv.CoxBoost(time=time,status=status,x=matrix.covariates, maxstepno=150,
                        K=10,type="verweij",penalty=op.penalty) 
  op.step = cv.res$optimal.step
  coxboost.model = pec::coxboost(Hist(time,event)~.
                             ,data = data
                             #,cv = T
                             #,cause = 1
                             ,stepno = op.step
                             ,penalty = op.penalty)
  return(coxboost.model)
}

running_cForest = function(data){
  require(mlr)
  require(party)
  surv.lrn.cForest = mlr::makeLearner("surv.cforest")
  surv.task = makeSurvTask(data = data, target = c("time", "event"))
  model.cForest = mlr::train(surv.lrn.cForest, surv.task)
  mlr.cforest = list(forest = model.cForest$learner.model)
  class(mlr.cforest) = 'pecCforest'
  mlr.cforest$call = match.call()
  return(mlr.cforest)
}
# mlr.to.pecCforest function:


running_rpart = function(data){
  require(mlr)
  require(rpart)
  surv.task = makeSurvTask(data = data, target = c("time", "event"))
  surv.lrn.rpart = mlr::makeLearner("surv.rpart")
  model.rpart = mlr::train(surv.lrn.rpart, surv.task)
  mlr.Rpart <- function(mlr.obj, data, formula){
    robj <- mlr.obj
    nclass <- length(unique(robj$where))
    data$rpartFactor <- factor(predict(robj,newdata=data))
    form <- update(formula,paste(".~","rpartFactor",sep=""))
    survfit <- prodlim::prodlim(form,data=data)
    out <- list(rpart=robj,survfit=survfit,levels=levels(data$rpartFactor))
    class(out) <- "pecRpart"
  }
  rpart.mlr.obj = mlr.Rpart(mlr.obj = model.rpart$learner.model
                            , data = data
                            , formula = Surv(time, event) ~.)
  return(rpart.mlr.obj)
}

#rpart.obj <- pec::pecRpart(Surv(time,event) ~., data = train.dat)


running_ranger = function(data){
  #require(mlr)
  require(ranger)
  # surv.task = makeSurvTask(data = data, target = c("time", "event"))
  # surv.lrn.ranger = mlr::makeLearner("surv.ranger_new")
  # model.ranger = trainLearner.surv.ranger_new(surv.lrn.ranger, surv.task)
  ranger.obj = ranger::ranger(Surv(time, event) ~.
                              , data = data
                              , mtry = 5
                              , nodesize = 2
                              , ntree = 2000
                              , importance = 'permutation'
                              , splitrule = 'extratrees'
                              , verbose = T
  )
}

# running_gbm = function(data){
#   require(mlr)
#   require(gbm)
#   surv.task = makeSurvTask(data = data, target = c("time", "event"))
#   surv.lrn.gbm = mlr::makeLearner("surv.gbm")
#   model.gbm = mlr::train(surv.lrn.gbm, surv.task)
#   return(model.gbm$learner.model)
# 
# }
 
running_gbm = function(data){
  #library(gbm, lib.loc = "C:/Users/hnguye78/Documents/R/R-3.5.1/library/gbm_github")
  library(gbm)
  gbm <- gbm::gbm(Surv(time, event) ~.
                     , distribution = 'coxph'
                     , data = data
                     , n.trees = 2000, shrinkage = 0.01             
                     , interaction.depth = 3, bag.fraction = 0.5, train.fraction = 1  
                     #, n.minobsinnode = 10, cv.folds = 0
                     , keep.data = T 
                     #, verbose = FALSE, n.cores = 1
  )  
  return(gbm)
}

running_gbmsci = function(data){
  library(gbm, lib.loc = "C:/Users/hnguye78/Documents/R/R-3.5.1/library/gbm_github")
  
  gbm.ci <- gbm::gbm(Surv(time, event) ~.
                     , distribution = 'sci'
                     , data = data
                     , n.trees = 500, shrinkage = 0.01             
                     , interaction.depth = 3, bag.fraction = 0.5, train.fraction = 1  
                     #, n.minobsinnode = 10, cv.folds = 0
                     , keep.data = F 
                     #, verbose = FALSE, n.cores = 1
  )  
  return(gbm.ci)
}


running_glmboost = function(data){
  require(mboost)
  
  glmboost.obj = mboost::glmboost(Surv(time, event)~., data = data
                                  , family = mboost::CoxPH()
  )
  cvm = cvrisk(glmboost.obj, grid = 1:100 * 10)
  #mstop(aic <- AIC(gamboost.obj))
  glmboost.obj2 = glmboost.obj[mstop(cvm)]
  
  return(glmboost.obj2)
}

running_gamboost = function(data){
  require(mboost)
  
  gamboost.obj = mboost::gamboost(Surv(time, event)~., data = data
                                  , family = mboost::CoxPH()
                                  , dfbase = 4 
  )
  cvm = cvrisk(gamboost.obj, grid = 1:100 * 10)
  #mstop(aic <- AIC(gamboost.obj))
  gamboost.obj2 = gamboost.obj[mstop(cvm)]
  return(gamboost.obj2)
}
 
#   require(gbm)
#   gbm1 <- gbm::gbm(Surv(time, event) ~.
#                    , distribution = 'coxph'
#                    , data = train.dat
#                    #, n.trees = 100, shrinkage = 0.1             
#                    #, interaction.depth = 1, bag.fraction = 0.5, train.fraction = 1  
#                    #, n.minobsinnode = 10, cv.folds = 0, keep.data = F 
#                    #, verbose = FALSE, n.cores = 1
#   )  
#   
#   library(gbm, lib.loc = "C:/Users/hnguye78/Documents/R/R-3.5.1/library/gbm_github")
#   surv.lrn.gbmsci = mlr::makeLearner("surv.gbmsci")
#   model.gbmsci = trainLearner.surv.gbmsci(surv.lrn.gbmsci, surv.task)
#   
#   best.iter <- gbm::gbm.perf(gbm1
#                              ,method="OOB"
#                              ,plot.it = F
#   )  # returns cv estimated best number of trees
#   
#   # variable importance from gbm:
#   summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees
#   
#   lp.gbm1 <- gbm::predict.gbm(object = gbm1
#                               ,newdata = test.dat
#                               ,n.trees = best.iter
#                               ,type = 'link'
#                               , single.tree = F
#   )
#   lp.gbmsci <- gbm::predict.gbm(object = model.gbmsci
#                                 ,newdata = test.dat
#                                 ,n.trees = best.iter
#                                 ,type = 'link'
#                                 , single.tree = F
#   )
#   
#   
#   probs.gbm = predictRisk.gbm(object=model.gbm$learner.model, newdata = test.dat, times = cutoff, n.trees = 100
#                               , traindata = train.dat)
#   
#   probs.gbmsci = predictRisk.gbm(object=model.gbmsci, newdata = test.dat, times = cutoff, n.trees = 100, traindata = train.dat)
#   
#   
#   # Gerd's cindex from pec, take predicted absolute risk as input:
#   cind.obj.gbm1 = pec::cindex(object = probs.gbm1,
#                               formula = Hist(time, event)~1,
#                               data = test.dat,
#                               #eval.times = 365.25*seq(2, max(test.dat$time)/365.25, by = 1), 
#                               eval.times = cutoff,
#                               #Starting time is 2 years from Y5 to (Y5+pt) years, calculate for every 12*2 = 24 months  
#                               splitMethod = 'none',
#                               cens.model = 'marginal')
#   cind.obj.gbm.ci = pec::cindex(object = probs.gbm.ci ,
#                                 formula = Hist(time, event)~1,
#                                 data = test.dat,
#                                 #eval.times = 365.25*seq(2, max(test.dat$time)/365.25, by = 1), 
#                                 eval.times = cutoff,
#                                 #Starting time is 2 years from Y5 to (Y5+pt) years, calculate for every 12*2 = 24 months  
#                                 splitMethod = 'none',
#                                 cens.model = 'marginal')
#   
# 
# 
# 
# 
# # train models using tuned hyper params:
# 
# # surv.lrn.rf.irace = mlr::setHyperPars(learner = mlr::makeLearner("surv.randomForestSRC"), par.vals = res.irace$x)
# # model.rf.irace = mlr::train(surv.lrn.rf.irace, surv.task)
# 
# 
# 
# surv.lrn.gamboost = mlr::makeLearner("surv.gamboost")
# model.gamboost = mlr::train(surv.lrn.gamboost, surv.task)
# 
# surv.lrn.glmboost = mlr::makeLearner("surv.glmboost")
# model.glmboost = mlr::train(surv.lrn.glmboost, surv.task)
# 
# # implement model using other packages than mlr:
# library(mboost)
# gamboost.obj = mboost::gamboost(Surv(time, event)~., data = train.dat
#                                 , family = mboost::CoxPH()
#                                 ) 
# coef(model.gamboost$learner.model)
# 
# #mstop(aic <- AIC(gamboost.obj))
# 
# lp.gamboost = predict(gamboost.obj, newdata = test.dat, type = "link")
# boost_control(mstop = 200, ## initial number of boosting iterations.
#                  + ## Default: 100
#                    + nu = 0.05, ## step length. Default: 0.1
#                  + trace = TRUE) ## print status information? Default: FALSE
# link.gamboost = mlr::predictLearner(surv.lrn.gamboost, model.gamboost, .newdata= test.dat)
#0 
#
extract_ascvd_var = function(data){
  data$sex = as.factor(data$SEX)
  data$race = as.factor(data$RACE)
  data$currentsmoker = as.factor(ifelse(data$C09SMKNW == 2, 1, 0))
  data$hbp.medication = as.factor(ifelse(data$C08HBNOW >= 1.5, 1, 0))
  data$sbp = data$C40SBP
  data$total.choles = data$CL1CHOL
  data$hdl.choles = data$CL1HDL
  data$diabetes.status = as.factor(ifelse(data$C08DIAB >= 1.5, 1, 0))
  data$age = data$EX3_AGE
  
  ascvd_var = c('time','event', 'sex','race', 'currentsmoker', 'hbp.medication','sbp','total.choles','hdl.choles','diabetes.status','age')
  dat = data[, ascvd_var]
  return(dat)
}

running_rsf = function(data){
  
  # load library
  list.of.packages <- c('randomForestSRC','survival','beepr')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = T)
  
  seed = 4495
  set.seed(seed)
  rf= rfsrc(Surv(time,event)~., data = data 
            , ntree = 1001
            , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
  num.var = ncol(rf$xvar)
  print(rf)
  #plot(rf)
  return(rf)
}

running_risk_score = function(dat){
  model = survfit(Surv(time, event)~ascvd, 
          data = dat)
  return(model)
}