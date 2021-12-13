predictRisk.DeepSurv <- function(pred_prob_surv, trained_time, eval_times, ...){
  pos <- prodlim::sindex(jump.times=trained_time, eval.times=eval_times)
  p <- cbind(1,pred_prob_surv)[,pos+1,drop=FALSE]
  if (NROW(p) != nrow(pred_prob_surv) || NCOL(p) != length(eval_times))
    stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",NROW(newdata)," x ",length(times),"\nProvided prediction matrix: ",NROW(p)," x ",NCOL(p),"\n\n",sep=""))
  p
  return(1-p)
}
