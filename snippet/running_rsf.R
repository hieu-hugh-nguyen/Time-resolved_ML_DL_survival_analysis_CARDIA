#Random Survival Forest Model:

running_rsf_tuned = function(data){
  
  # load library
  list.of.packages <- c('randomForestSRC','pec','riskRegression','survival','beepr','akima')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = T)
  
  # create saving dir: 
  # main.dir = paste0(work_dir, '/rdata_files')
  # sub.dir = paste0('rsf_fold_',fold)
  # if (dir.exists(file.path(main.dir, sub.dir))){
  #   saving.dir = file.path(main.dir, sub.dir)
  # } else {
  #   saving.dir = dir.create(file.path(main.dir, sub.dir), F)
  # }
  
  # tune rsf hyperparameter mtry and node size, using the tuning function from the package RandomForestSRC
  
  start.time = Sys.time()
  tuneObject <- randomForestSRC::tune(Surv(time, event)~., data = data, ntreeTry=1000
                     ,mtryStart = sqrt(ncol(data)),
                     nodesizeTry = c(1:10),
                     stepFactor = 1.25, improve = 1e-3, strikeout = 3, maxIter = 25,
                     trace = T, doBest = TRUE
  )
  #save(tuneObject, file = paste0(saving.dir,'/RFSRC_tuneObject.RData'))
  time_running_tune_object = Sys.time()-start.time
  
  #print(tuneObject$rf)
  
    ## nice little wrapper for plotting results
    plot.tune <- function(o, linear = TRUE) {
      x <- o$results[,1]
      y <- o$results[,2]
      z <- o$results[,3]
      so <- interp(x=x, y=y, z=z, linear = linear)
      idx <- which.min(z)
      x0 <- x[idx]
      y0 <- y[idx]
      filled.contour(x = so$x,
                     y = so$y,
                     z = so$z,
                     xlim = range(so$x, finite = TRUE) + c(-2, 2),
                     ylim = range(so$y, finite = TRUE) + c(-2, 2),
                     color.palette =
                       colorRampPalette(c("yellow", "red")),
                     xlab = "nodesize",
                     ylab = "mtry",
                     main = "OOB error for nodesize and mtry",
                     key.title = title(main = "OOB error", cex.main = 1),
                     plot.axes = {axis(1);axis(2);points(x0,y0,pch="x",cex=1,font=2);
                       points(x,y,pch=16,cex=.25)})
    
    ## plot the surface
    #plot.tune(tuneObject)
  }
  
  start.time = Sys.time()
  rf.optimal= rfsrc(Surv(time,event)~., data = data 
                    , ntree = 1000
                    , mtry = tuneObject$optimal['mtry']
                    , nodesize = tuneObject$optimal['nodesize']
                    , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
                    )
  #num.var = ncol(rf.optimal$xvar)
  #save(rf.optimal, file = paste0(saving.dir, '/RSF_',num.var,'_var.RData'))
  #time_running_rf = Sys.time()-start.time
  print(rf.optimal)
  #plot(rf.optimal)
  return(rf.optimal)  

}

running_rsf_no_tune = function(data){
  
  # load library
  list.of.packages <- c('randomForestSRC','pec','riskRegression','survival','beepr','akima')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = T)

  seed = 4495
  set.seed(seed)
  rf= rfsrc(Surv(time,event)~., data = data 
                    , ntree = 1000
                    , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
  num.var = ncol(rf$xvar)
  print(rf)
  plot(rf)
  return(rf)
}

running_rsf_with_tuned_param = function(data){
  
  # load library
  list.of.packages <- c('randomForestSRC','pec','riskRegression','survival','beepr','akima')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = T)
  
  # create saving dir: 
  # main.dir = paste0(work_dir, '/rdata_files')
  # sub.dir = paste0('rsf_untuned_fold_',fold)
  # if (dir.exists(file.path(main.dir, sub.dir))){
  #   saving.dir = file.path(main.dir, sub.dir)
  # } else {
  #   saving.dir = dir.create(file.path(main.dir, sub.dir), F)
  # }
  
  seed = 4495
  set.seed(seed)
  rf= rfsrc(Surv(time,event)~., data = data 
            , ntree = 1722
            , mtry = 5
            , nodesize = 2
            , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
  #num.var = ncol(rf$xvar)
  # print(rf)
  # plot(rf)
  return(rf)
}


  # print and plot the grow object
  #==================================================================
  # Minimal depth of maximal subtree
  #==================================================================
  
  minDepth = function(rsf){
    
    library(randomForestSRC)
    max.subtree = max.subtree(rsf, conservative = F)
    #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
    
    # Get minimal depth of maximal subtree in terms of variable name, ascending order:
    allvardepth = sort(max.subtree$order[, 1])
    allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
    return(allvardepth.df)
  }
  
  var_ranking_treedepth = function(rsf, saving_dir){
    
    allvardepth.df = minDepth(rsf)
    write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
    # load var dictionary to get var description:
      require('readxl')
      loading.dir = paste0(work_dir, '/csv_files')
      var.dictionary.updated = read_excel(paste0(loading.dir,"/y5_all_vars_dictionary_manually_selecting_vars (1).xlsx"))
      
      get.var.des = function(x){
        var.des = var.dictionary.updated$`Variable Label`[which(var.dictionary.updated$`Variable Name` == x[1])]
        return(var.des)
      }
      vardes = unlist(apply(allvardepth.df, 1, FUN = get.var.des))
      require('tibble')
      allvardepth.df = add_column(allvardepth.df, VariableDescription = vardes, .after = 1)
      write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank_all_var_y10.csv', sep = ''),row.names=T)
    
  }  

  # # get the top 20 vars:
  # top20var = sort(max.subtree$order[, 1])[1:20];
  # 
  # top20var.df = data.frame(Variable=names(top20var),MinDepthMaxSubtree=top20var,row.names = NULL)
  # 
  # saving.dir="U:/CARDIA Other/CARDIACCdata/Y5/ComprehensiveExcelFilesWithLABELS/ModelCSVFiles";
  # 
  # #write.csv(top20var.df, file = paste(saving.dir, '/depth_rank_top20(1).csv', sep = ''),row.names=FALSE)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # # get the top 20 vars:
  # top20var.df = allvardepth.df[1:20,]
  # write.csv(top20var.df, file = paste(saving.dir, '/depth_rank_top20.csv', sep = ''),row.names=FALSE)
  # 
  # 
  # # get the top 50 vars:
  # top50var.df = allvardepth.df[1:50,]
  # write.csv(top50var.df, file = paste(saving.dir, '/depth_rank_top50.csv', sep = ''),row.names=FALSE)
  # 
