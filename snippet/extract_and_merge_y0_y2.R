extract_and_merge_y0_y2 = function(){
  # Extract top 20 markers of y5 from y0 and y2:
  # exclude markers not available in y0 and y2:
  
  # y0:
  loading.dir = paste0(work_dir,'/Y0/csv_files')
  y0_data_full = read.csv(file = paste0(loading.dir,'/y0_feature_space_with_label.csv'))
  markers = c('A02DBP', 'A02SBP', 'AL1LDL', 'AL1NTRIG', 'A20WST', 'A02ARMCI'
              , 'AL1CHOL', 'A12FE1TM', 'A20HIP')
  y0_data = y0_data_full[-1,c('ID',markers)]
  
  # y2:
  loading.dir = paste0(work_dir,'/Y2/csv_files')
  y2_data_full = read.csv(file = paste0(loading.dir,'/BigDataSheet.csv'))
  markers = c('B02AVGDI', 'B02AVGSY', 'BL1LDL', 'BL1NTRIG', 'B20WST', 'B02ARMCI'
              , 'BL1CHOL', 'B12FE1TM', 'B20HIP')
  y2_data = y2_data_full[-1,c('ID',markers)] #remove the first row which is the label row
  
  
  # merge:
  data.longi = merge(y0_data,y2_data, by = 'ID')
  return(data.longi)
}