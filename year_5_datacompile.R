rm(list=ls()) #Clear all
cat("\014")


# Input Exam Year:
#exam_year = 'Y0'

 exam_year = 'Y5'



 


if (exam_year == 'Y0'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y0/Y0/DATA'
}
if (exam_year == 'Y2'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y2/Y2/DATA'
}
if (exam_year == 'Y5'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y5/DATA'
}
if (exam_year == 'Y7'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y7/Y7/DATA'
}
if (exam_year == 'Y10'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y10/Y10/DATA/SAS'
}
if (exam_year == 'Y15'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y15/Y15/DATA'
}
if (exam_year == 'Y20'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y20/Y20/CORE/DATA'
}
if (exam_year == 'Y25'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y25 7.26.17/DATA'
}
if (exam_year == 'Y30'){
  load_dir <- 'U:/CARDIA Other/CARDIACCdata/Y30/Y30data_v13'
}


work_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun_2'
setwd(work_dir)
 


##### Load libraries:##################################################################
list.of.packages <- c('haven', 'tibble','Hmisc','labelled','DataCombine', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


source_dir <- paste0(work_dir,'/cardia_rerun_2_code/snippet')
source(paste0(source_dir,'/remove_hms_time_type.R'))

##### Initialization:##################################################################


#Extract file names from the SAS folder:
filenames=list.files(path = load_dir, pattern = "\\.sas7bdat$");
#Extract file names without the extension:
filenamesWoExtension=tools::file_path_sans_ext(filenames);
#Create objects with the same name and assign the data in each file to its corresponding name
for(i in 1:length(filenames)){
  assign(filenamesWoExtension[i],haven::read_sas(paste0(load_dir, '/', filenames[i])))
}


# Remove unneeded studies:
rm(c1f42, c1f42b)

# remove follow-up data and only retain Y5 data:
rm(aflwup1, aflwupt1, bflwup1, bflwupt1)

# Remember to check all loaded dataframes: 
# sapply(sapply(ls(), get), glimpse)
 

#Get all studies names (data frames):
Studies <- sapply(sapply(ls(), get), is.data.frame);
Studies = names(Studies)[(Studies==TRUE)]




# load subjects who have not had CVD event by y5:
load_dir = paste0(work_dir, '/csv_files')
label_space = read.csv(paste0(load_dir,'/y5_cvd_outcome','.csv'))


ComDataset <- tibble('ID' = label_space$ID)
#Add SHORT ID column (will be necessary later):
SHORT_ID = substr(ComDataset$ID,1,5);
ComDataset = add_column(ComDataset,SHORT_ID,.after="ID")
names(ComDataset$SHORT_ID)<-"SHORT_ID"
label(ComDataset[["ID"]]) <- "PARTICIPANT ID"
label(ComDataset[["SHORT_ID"]]) <- "FIRST 5 ID DIGITS"





# keep track of which data collection form that each variable comes from:
# datadoc <- rep(Studies[1],length = ncol(SOI))
datadoc <- c('')


# Main Computational Loop to compile the big dataset:===================================================#





##### Part I. Compile existing data reported in sas files: ##############################################
for(j in 1:length(Studies)){
  SOI = get(Studies[j]);
  
  
  #Change date format to character to avoid date conversion to days since 1970:
  inx <- sapply(SOI, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
  SOI[inx] <- lapply(SOI[inx], as.character)
  
  varName=names(SOI);
  
  # convert non-numeric column to character-type (to avoid non-compatibility error later)
  # SOI_char <- apply(SOI, 2, function(x){if (class(x) == 'character'){as.character(x)}})
  
  non_overlaping_cols <- names(SOI)[!(names(SOI) %in% names(ComDataset))]
  ncol_old_ComDataset <- ncol(ComDataset)
  if(!is.null(SOI$ID)==TRUE){ #If there is a ID column
    class(SOI$ID) <- 'character'
    class(ComDataset$ID) <- 'character'
    
    ComDataset <- ComDataset %>% dplyr::left_join(SOI %>% dplyr::select(c('ID',non_overlaping_cols)), by = 'ID')
  }
  
  else{
    if(!is.null(SOI$SHORT_ID)==TRUE){ #If there is a SHORT ID column
      class(SOI$SHORT_ID) <- 'character'
      class(ComDataset$SHORT_ID) <- 'character'
      ComDataset <- ComDataset %>% dplyr::left_join(SOI %>% dplyr::select(c('SHORT_ID',non_overlaping_cols)), by = 'SHORT_ID')
    }
  }
  ComDataset<- ComDataset %>% filter(!duplicated(ID))
  datadoc <- c(datadoc, rep(Studies[j], length = ncol(ComDataset)-ncol_old_ComDataset)) #minus 1 because excluding the ID column
  
}



 

#Sort the final dataset based on full ID in ascending order: 
ComDataset = ComDataset[order(ComDataset$ID),] 








# Write the big dataset to a .CSV file:##########################################


# since <time> variables (<hms>, <difftime> class) cannot be written to csv, delete these variables: 
ComDataset2 <- remove_hms_time_type(ComDataset)


# only include participants coming to exam 3 by available data for variable EX3_AGE 'CALCULATED AGE AT EXAM 3'

ComDataset2_y5 <- ComDataset2 %>% filter(!is.na(EX3_AGE))


saving.dir = paste0(work_dir, '/csv_files')
write.csv(ComDataset2_y5, file = paste0(saving.dir,"/", exam_year, "_unimputed_featurespace.csv"),row.names=FALSE)













# Create a Variable Dictionary .CSV file##############################################

#Create a row of variable labels:
AllVarLabel = sapply(ComDataset, function(x) attr(x, 'label'))
rowofAllVarLabel = vector(mode = "character", length = length(AllVarLabel));
for(k in 1:length(AllVarLabel)){
  if(is.null(AllVarLabel[[k]])){
    rowofAllVarLabel[k]="";
  }
  else{
    rowofAllVarLabel[k]=AllVarLabel[[k]];
  }
}

rowofAllVarLabelDf = as.data.frame(t(rowofAllVarLabel));
names(rowofAllVarLabelDf) = names(ComDataset);

#Count the total of participants for each column:
#Also look for whether the variable for each column is Categorical or Continuous:
numParticipants = vector(mode = "numeric", length = length(names(ComDataset)));
CategoricalOrContinuous = rep(NA,length(names(ComDataset)));

for (y in 1:length(names(ComDataset))){
  colObject=eval(parse(text=paste("ComDataset", "$", names(ComDataset)[y],sep="")));
  numParticipants[y] = sum(sapply(colObject, function(x) (!is.na(x) && x!="")));
  
  if(length(unique(colObject)) <= 20){ #If there are fewer than 20 unique values, categorical 
    CategoricalOrContinuous[y] = 0; 
  }
  else{
    CategoricalOrContinuous[y] = 1; #else, variable is continuous  
  }
  
} 


datadoc1 <- c("",datadoc) # add empty doc for the ID variable



#Create Df of var dict: 
AllVarDf = data.frame(names(ComDataset), rowofAllVarLabel, numParticipants, CategoricalOrContinuous);
names(AllVarDf)= c("Variable Name","Variable Label","Number of Participants (total non-NA values)","Categorical=0, Continuous =1, Categorical in this context means 'there are fewer than 6 unique values', while Continuous means greater than 6 ");

#move ID row to the top:
AllVarDf = bind_rows(AllVarDf %>% filter(`Variable Name` == 'ID'), AllVarDf %>% filter(`Variable Name` != 'ID'))  

AllVarDf$Datadoc = datadoc1
AllVarDf$Percent_available_in_y5_exam_subjects = AllVarDf$`Number of Participants (total non-NA values)`/nrow(ComDataset2_y5)*100
#Write to a csv file: 
saving.dir = paste0(work_dir,'/csv_files')
write.csv(AllVarDf, file = paste0(saving.dir,"/", exam_year, "_all_vars_dictionary.csv"),row.names=FALSE)


 



 
 