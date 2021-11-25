# Input: a dataframe of interest, might include time <hms> <difftime> type variables (can't be written to csv)
# Output: the dataframe with these time variables removed
remove_hms_time_type <- function(data_frame_oi){
  inx <- sapply(data_frame_oi, function(x) inherits(x, "difftime") || inherits(x, "hms"))
  data_frame_oi[,inx] <- NULL
  return(data_frame_oi)
}
