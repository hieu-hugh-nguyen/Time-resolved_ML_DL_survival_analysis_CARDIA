# functions related to classif task:

classif_transform = function(data, year.oi){
  data.have.not.lost.followup.df = data[!(data$event == 0 & data$time <= year.oi*365.25), ]
  dat = data.have.not.lost.followup.df
  rm(data.have.not.lost.followup.df)
  # get all event cases in training data by year of interest:
  classi.label = ifelse( (dat$event == 1 & dat$time <= year.oi*365.25), 1, 0)
  dat = add_column(dat, classi.label, .before = 1)
  # remove time and event column:
  dat = within(dat, rm('time', 'event'))
  # convert 0 and 1 into non-event, event:
  dat$classi.label = ifelse(dat$classi.label == 0, 'no_event', 'event')
  dat$classi.label = as.factor(dat$classi.label)
  levels(dat$classi.label) = c('event', 'no_event')
  return(dat)
}


running_xgboost = function(dat){
  require(caret)
  control = caret::trainControl(method="repeatedcv", 
                              number=10, 
                              repeats=3,
                              summaryFunction = twoClassSummary,
                              classProbs = T,
                              savePredictions = T,
                              search = 'random')
# Create model weights (they sum to one)
  minority_ratio = table(dat$classi.label)[1]/(table(dat$classi.label)[1]+ table(dat$classi.label)[2])
  model_weights = ifelse(dat$classi.label == "no_event",
                         minority_ratio,
                         1-minority_ratio)
  
  # Build weighted model
  parallelMap::parallelStartSocket(2)
  modelxgboost.weighted = caret::train(classi.label ~ .,
                                       data = dat,
                                       method = "xgbTree",
                                       verbose = FALSE,
                                       weights = model_weights,
                                       metric = "ROC",
                                       trControl = control)
  return(modelxgboost.weighted)
}

running_glmnet = function(dat){
  require(caret)
  control = caret::trainControl(method="repeatedcv", 
                                number=10, 
                                repeats=3,
                                summaryFunction = twoClassSummary,
                                classProbs = T,
                                savePredictions = T,
                                search = 'random')
  # Create model weights (they sum to one)
  minority_ratio = table(dat$classi.label)[1]/(table(dat$classi.label)[1]+ table(dat$classi.label)[2])
  model_weights = ifelse(dat$classi.label == "no_event",
                         minority_ratio,
                         1-minority_ratio)
  
  # Build weighted model
  parallelMap::parallelStartSocket(2)
  modelglmnet.weighted = caret::train(classi.label ~ .,
                                       data = dat,
                                       method = "glmnet",
                                       #verbose = FALSE,
                                       weights = model_weights,
                                       metric = "ROC",
                                       trControl = control)
  return(modelglmnet.weighted)
}

running_ranger = function(dat){
  require(caret)
  control = caret::trainControl(method="repeatedcv", 
                                number=10, 
                                repeats=3,
                                summaryFunction = twoClassSummary,
                                classProbs = T,
                                savePredictions = T,
                                search = 'random')
  # Create model weights (they sum to one)
  minority_ratio = table(dat$classi.label)[1]/(table(dat$classi.label)[1]+ table(dat$classi.label)[2])
  model_weights = ifelse(dat$classi.label == "no_event",
                         minority_ratio,
                         1-minority_ratio)
  
  # Build weighted model
  parallelMap::parallelStartSocket(2)
  modelranger.weighted = caret::train(classi.label ~ .,
                                      data = dat,
                                      method = "ranger",
                                      #verbose = FALSE,
                                      weights = model_weights,
                                      metric = "ROC",
                                      trControl = control)
  return(modelranger.weighted)
}

predict_classif = function(model, newdata){
  predicted.prob = predict(model, newdata, type = 'prob')[,2]
  return(predicted.prob)
}

eval_performance.classif = function(predicted.prob, label){
  # library(pROC)
  # auc = pROC::auc(response = dat.2$classi.label, predictor = predicted.prob)
  library(ROCR)
  model.pred.obj = ROCR::prediction(predicted.prob, label)
  model.auc = ROCR::performance(model.pred.obj, measure = "auc")
  model.auc = unlist(model.auc@y.values)
  return(model.auc)
}