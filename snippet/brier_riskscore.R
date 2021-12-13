brier_riskscore = function(data, eval.times){
  t = eval.times[8]
  data = data
  new.data = within(data, {
    index1 = time < t
    index2 = event == 1
    event[index1 & index2] = 1
    event[!index1 | !index2] = 0
  })
  new.data = na.omit(new.data)
  model = survfit(Surv(time, event)~ascvd, 
                data = new.data)
  data.1 = data
  data.1 = within(na.omit(data.1), (s.prob = predictSurvProb(model, 
                                                              times = eval.times[1], 
                                                              newdata = new.data)[,2]))
  (brier = mean((1- data.1$s.prob - data.1$event)^2, na.rm = T))
}