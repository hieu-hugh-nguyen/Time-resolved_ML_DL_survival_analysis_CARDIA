
stratified_event_split <- function(data, time_var = 'time',event_var = 'status', id_var = 'ID'
                                   ,nfold = 5 # 5-fold cross-validation
                                   ,num_split_times = 5 # 5 times of 5-fold cross-validation
){
  
  seed = seq(4995,4995+4,1)
  
  all.train = NULL
  all.valid = NULL
  
  
  #Check if there is any character column, then delete them to make sure all data is numeric:
  # nums <- unlist(lapply(data, is.character))  
  # data[,nums]=NULL
  # 
  
  ID.event = data[data[,event_var] == 1,id_var]
  ID.nonevent = data[data[,event_var] == 0,id_var]
  
  # stratified sample size
  num.event = dim(ID.event)[1]
  num.nonevent = dim(ID.nonevent)[1]
  
  train.ID.df = data.frame()  
  valid.ID.df = data.frame()
  test.ID.df = data.frame()
  
  for( times in 1:num_split_times ){
    set.seed(seed[times])
    #Randomly shuffle the data
    ID.event = ID.event[sample(length(ID.event))]
    ID.nonevent = ID.nonevent[sample(length(ID.nonevent))]
    #Create equally-sized folds
    folds.event = cut(seq(1,length(ID.event)),breaks=nfold,labels=FALSE)
    folds.nonevent = cut(seq(1,length(ID.nonevent)),breaks=nfold,labels=FALSE)
    
    for(fold in 1:nfold){
      # Segment your data by fold using the which() function 
      test.event.indices = which(folds.event==fold,arr.ind=TRUE)
      test.nonevent.indices = which(folds.nonevent==fold,arr.ind=TRUE)
      
      test.event.ID = ID.event[test.event.indices]
      test.nonevent.ID =  ID.nonevent[test.nonevent.indices]
      test.ID = c(test.event.ID, test.nonevent.ID)
      
      train.event.ID = ID.event[-test.event.indices]
      train.nonevent.ID =  ID.nonevent[-test.nonevent.indices]
      
      # set aside one-fourth of the training set to be used as validation set:
      valid.event.indices = sample(length(train.event.ID), size = round(length(train.event.ID)/4))
      valid.nonevent.indices = sample(length(train.nonevent.ID), size = round(length(train.nonevent.ID)/4))
      
      valid.event.ID = train.event.ID[valid.event.indices]
      valid.nonevent.ID =  train.nonevent.ID[valid.nonevent.indices]
      valid.ID = c(valid.event.ID, valid.nonevent.ID)
      
      train.event.ID = train.event.ID[-valid.event.indices]
      train.nonevent.ID =  train.nonevent.ID[-valid.nonevent.indices]
      train.ID = c(train.event.ID, train.nonevent.ID)
      
      
      
      cbind.fill <- function(...){
        nm <- list(...) 
        nm <- lapply(nm, as.matrix)
        n <- max(sapply(nm, nrow)) 
        do.call(cbind, lapply(nm, function (x) 
          rbind(x, matrix(, n-nrow(x), ncol(x))))) 
      }
      train.ID.df= cbind.fill(train.ID.df, train.ID)
      valid.ID.df= cbind.fill(valid.ID.df, valid.ID)
      test.ID.df= cbind.fill(test.ID.df, test.ID)
    }
  }
  return(list(train.ID = train.ID.df, valid.ID = valid.ID.df, test.ID = test.ID.df))
}
