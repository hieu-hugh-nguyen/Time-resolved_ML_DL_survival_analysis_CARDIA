impute = function(data){
  data = data[,-1]
  
  # impute:
  imp.data.unsup = impute.rfsrc(data = data[-c(1,2)], nsplit = 10, nimpute = 5, nodesize = 1)
  
  # add id column back to the imputed feature space: 
  imp.data.unsup.1 = cbind(ID = label_space$ID, imp.data.unsup)
  
  saving.dir = work_dir
  write.csv(imp.data.unsup.1, file = paste0(saving.dir,'/y5_imputed_unsupervised_v2.csv'), row.names = F)
}