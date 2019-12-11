#
# Import the for weights / not normalised
###############################################

# Note:
# The data on wieghts will be narmalised later (to add to 100% - as not all of the criteria were used in the interview)

library(rjson)
library(dplyr)

########################
#### DATA TO ADJUST ####
########################

# Folder countaining the engagments files
dir <- "/XXX/MCM/data/"

# Folder in which the outputs should be saved
dir_output <- "/XXX/MCM/csv/"

#########################
#### END DATA ADJUST ####
#########################

# set directory for data inputes
setwd(dir)

#===================================================
# Get filenames from directory to / data to import
#===================================================
Files<-list.files(dir)

#===================================
# Get all data from engagement files
#===================================
firstline<-0
for(j in Files){
  print(j)
  json_data <- fromJSON(file=j)

  # 1. Get data about criteria in df_crit dataframe
  for(i in 1:length(json_data$engagement$criteria)) {
    
    try <- as.data.frame(json_data$engagement$criteria[[i]])
    
    if ("weight" %in% colnames(try)){
      try <- try %>%
        select(label,weight,type,id)
    }
    else{
      try <- try %>%
        select(label,type,id)
        try$weight<- 100
    }
    
    try$file_name <- gsub(".engagement", "", j)
    
    if (firstline==0){
      df_crit <- data.frame(try)
      firstline<-1
    }
    else {
      df_crit<- rbind (df_crit, try)
    }
  }
  
  # 2. Get data about scores in df_score dataframe
  for(i in 1:length(json_data$engagement$scores)) {
    
    try <- as.data.frame(json_data$engagement$scores[[i]])
    
    try$file_name <- gsub(".engagement", "", j)
    
    if (firstline==1){
      df_score<- data.frame(try)
      firstline<-2
    }
    else {
      df_score<- rbind (df_score, try)
    }
  }
  
  # 3. Get data about notes in df_notes dataframe
  for(i in 1:length(json_data$engagement$notes)) {
    
    try <- as.data.frame(json_data$engagement$notes[[i]])
    
    if ("text" %in% colnames(try)){
      try <- try %>%
        select(text,id,object)
    }
    else{
      try <- try %>%
        select(id,object)
      try$text<- ""
    }
    
    try$file_name <- gsub(".engagement", "", j)
    
    if (firstline==2){
      df_notes<- data.frame(try)
      firstline<-3
    }
    else {
      df_notes<- rbind (df_notes, try)
    }
  }
  
  # 4. Get data about options in df_opt dataframe
  for(i in 1:length(json_data$engagement$options)) {
    
    try <- as.data.frame(json_data$engagement$options[[i]])
    try <- try %>%
      select(label,type,id)
    
    try$file_name <- gsub(".engagement", "", j)
    
    if (firstline==3){
      df_opt<- data.frame(try)
      firstline<-4
    }
    else {
      df_opt<- rbind (df_opt, try)
    }
  }
  
}

#========================================
# Merge all the Dataframes in master_df
#========================================

# Change relevant col name to make it easier in the merge
colnames(df_crit)[3]<-"crit_type"
colnames(df_score)[4]<-"score_type"
colnames(df_opt)[2]<-"option_type"
colnames(df_opt)[1]<-"option_label"
colnames(df_crit)[1]<-"crit_label"
colnames(df_score)[1]<-"object"

# Merge all dfs into master
master_df <- merge(df_score, df_opt, by.x=c("file_name", "option"), by.y=c("file_name", "id") , all = TRUE)
master_df <- merge(master_df, df_crit, by.x=c("file_name", "criterion"), by.y=c("file_name", "id") , all = TRUE)
master_df <- merge(master_df, df_notes, by.x=c("file_name", "object"), by.y=c("file_name", "object"), all = TRUE )

# Select the column we want to keep
master_df <- master_df %>%
  select(file_name, option_label, option_type, crit_label, crit_type, weight, score_type, score, text)%>%
  filter(!is.na(crit_label))
  
master_df <- master_df %>%
  group_by(file_name, option_label, crit_label, score_type)%>%
  mutate(comment=paste(text, collapse =" / ")) %>%
  ungroup() %>%
  select(-text)
  
master_df <- unique(master_df)
master_df<-master_df[(!is.na(master_df$option_label) & !is.na(master_df$crit_label)),]

#================
# Export data
#================

# remove any file that you do not want to keep in the analysis. 
# master_df <- master_df %>%
#   filter(file_name!="UK05")
  
# Export the master dataframe
setwd(dir_output)
write.csv(master_df, file="MCM_Overall_Rawdata.csv")

