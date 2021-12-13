# get uncensored ID for classification task at x year:

classif_id = function(year.oi){
  nfold = 25
  require('rowr')
  ID.train.classif.1.df = data.frame()
  ID.train.classif.2.df = data.frame()
  ID.test.classif.1.df = data.frame()
  ID.test.classif.2.df = data.frame()
  
  for (fold in 1:nfold){
    trainingid.1 = na.omit(trainingid.all.1[,fold])
    eligible_id.1 = intersect(trainingid.1, data.full$ID)
    data.train.1 = data.full[which(data.full$ID %in% eligible_id.1),c('ID', 'time','event')]
    ID.train.classif.1 = classif_transform(data.train.1, year.oi = year.oi)$'ID'
    ID.train.classif.1.df = rowr::cbind.fill(ID.train.classif.1.df, ID.train.classif.1, fill = NA)
    
    trainingid.2 = na.omit(trainingid.all.2[,fold])
    eligible_id.train.2 = intersect(trainingid.2, data.full$ID)
    data.train.2 = data.full[which(data.full$ID %in% eligible_id.train.2),c('ID', 'time','event')]
    ID.train.classif.2 = classif_transform(data.train.2, year.oi = year.oi)$'ID'
    ID.train.classif.2.df = rowr::cbind.fill(ID.train.classif.2.df, ID.train.classif.2, fill = NA)
    
    testingid.2 = na.omit(testingid.all.2[,fold])
    eligible_id.test.2 = intersect(testingid.2, data.full$ID)
    data.test.2 = data.full[which(data.full$ID %in% eligible_id.test.2),c('ID', 'time','event')]
    ID.test.classif.2 = classif_transform(data.test.2, year.oi = year.oi)$'ID'
    ID.test.classif.2.df = rowr::cbind.fill(ID.test.classif.2.df, ID.test.classif.2, fill = NA)
    
    testingid.1 = na.omit(testingid.all.1[,fold])
    eligible_id.test.1 = intersect(testingid.1, data.full$ID)
    data.test.1 = data.full[which(data.full$ID %in% eligible_id.test.1),c('ID', 'time','event')]
    ID.test.classif.1 = classif_transform(data.test.1, year.oi = year.oi)$'ID'
    ID.test.classif.1.df = rowr::cbind.fill(ID.test.classif.1.df, ID.test.classif.1, fill = NA)
    
  }
  
  
  ID.train.classif.1.df = ID.train.classif.1.df[,-1]
  ID.train.classif.2.df = ID.train.classif.2.df[,-1]
  ID.test.classif.1.df = ID.test.classif.1.df[,-1]
  ID.test.classif.2.df = ID.test.classif.2.df[,-1]
  
  saving.dir = 'C:/Users/HIEU/Desktop/CARDIA project/Git/rdata_files'
  save(ID.train.classif.1.df, file = paste0(saving.dir, '/all_training_id_outerloop_classif_1_year_', year.oi,'.Rdata'))
  save(ID.train.classif.2.df, file = paste0(saving.dir, '/all_training_id_outerloop_classif_2_year_', year.oi,'.Rdata'))
  save(ID.test.classif.1.df, file = paste0(saving.dir, '/all_testing_id_outerloop_classif_1_year_', year.oi,'.Rdata'))
  save(ID.test.classif.2.df, file = paste0(saving.dir, '/all_testing_id_outerloop_classif_2_year_', year.oi,'.Rdata'))
}