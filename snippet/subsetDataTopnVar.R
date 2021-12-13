#==================================================================
# Subset data with top n variables
#==================================================================

# load predictor order ranked by minimal depth
# topvar.dir=paste0(work_dir,'/csv_files')
# var.order = read.csv(file = paste(topvar.dir, 
#                                   '/averaged_outerloop_VIMP.csv',               
#                                   sep = ''),
#                      header = T)
# var.order = apply(var.order, 2, as.character)
# 
# seed = 4995
# set.seed(seed)

# subset data function:

subsetDataTopnVar = function(n.top, data, var.order){
  
  var.list = as.character(var.order[1:n.top])
  
  # new dataset which only includes time, event status, and top n variables:
  data.featurespace = data[,var.list]
  data = cbind(data[,c('ID','time','event')],data.featurespace)
  return(data)
}

# dat10 = dat_ntopvar(n.top = 10, dat = trained.data)
# 
# dat.train = dat10
# 
# dat.valid = test.data[,which(names(test.data) %in% names(dat10))]
