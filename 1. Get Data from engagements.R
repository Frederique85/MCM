########################################################
# Routine to gather data from engagements              #
# By Fred Bone - for MCM                               #
# Updated by Josie Coburn - for MCM                    #
# Data needed: online engagement data and offline      #
# engagement files, weighting data, and core options   #
# Data outputs: Merged_MCM_Dataset_w_Comments.csv,     # 
# containing scores, notes, and weights for all        #
# criteria and options; and Other_MCM_data.csv,        #
# containing weighting notes and other data            #
########################################################

# Note:
# The data on weights will be normalised later (to add to 100% - 
# as not all of the criteria were used in all of the interviews)

# Remove everything from the Global Environment
rm(list=ls())

# Call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

########################
#### DATA TO ADJUST ####
########################

# Project folder containing all other folders and files
dir_MCM_project <- "INSERT YOUR PROJECT FILE PATH"

# Folder for the output data
dir_output <- file.path(dir_MCM_project, "csv")

# Folders of the engagement data files - both online and offline
dir_engagements_online <- file.path(dir_MCM_project, "online_engagements")
dir_engagements_offline <- file.path(dir_MCM_project, "offline_engagements")

# Folder of weightings data files for the online engagements
dir_weighting <- file.path(dir_MCM_project, "weighting")

# Folder of core options data file for the online engagements
dir_core_options <- file.path(dir_MCM_project, "core options")

#########################
#### END DATA ADJUST ####
#########################

#=================================================
# 1. Set the directory 
#=================================================

# Set the working directory
setwd(dir_MCM_project)

#=============================================
# 2. Get data from online engagement files
#=============================================

#=============================================
# 2.1 Get quantitative data - scores
#=============================================

# MCM Data from online engagements - quantitative
Files_online_quant<-list.files(paste0(dir_engagements_online, "/quant/"))

# Get the data from the online engagements (if there are any)
if(length(Files_online_quant) > 0){
  
  # Set a counter, i to 1
  i=1
  
  # For all of the files in the quant folder
  for(j in Files_online_quant){
    
    # Set the file name
    FName <- j
    # For testing:
    #FName <- "UK01 quant.csv"
    
    # Read in the quantitative data .csv file to a dataframe, df
    df <- read.csv(paste0(dir_engagements_online, "/quant/", FName), 
                   stringsAsFactors=F, header=T, check.names=FALSE)
    
    # Rename the first two columns
    names(df)[2]<-"score_type" 
    names(df)[1]<-"option_label" 
    
    # Convert the score type to lower case, e.g. optimistic
    df$score_type <- tolower(df$score_type)
    
    # Gather the data - "lengthen" the data, increasing the number of rows 
    # and decreasing the number of columns
    df<-gather(df, key="crit_label",value="score" , -c(option_label, score_type))
    
    # Set the file_name variable to be the first part of the file name, 
    # i.e. remove " quant.csv" from the file name
    df$file_name <- gsub(" quant.csv","", FName)
    
    # If the counter is 1, i.e the first time through the loop
    if (i==1) {
      
      # The quantitative engagement data is the df
      engagement_quant = df 
      
    }else{ # Otherwise
      
      # Add the df to the quantitative engagement data
      engagement_quant = bind_rows(engagement_quant, df) 
    }
    
    # Add 1 to the counter
    i=i+1
  } 
  
  # Strip white space from the start and end of variables
  engagement_quant <- engagement_quant %>% 
    mutate(across(where(is.character), str_trim))
}

#=============================================
# 2.2 Get qualitative data - notes
#=============================================

# MCM Data from online engagements - qualitative
Files_online_qual<-list.files(paste0(dir_engagements_online, "/qual/"))

# Get the data from the online engagements (if there are any)
if(length(Files_online_qual) > 0){
  
  # Set a counter, i to 1
  i=1
  
  # For all of the files in the qual folder
  for(j in Files_online_qual){
    
    # Set the file name
    FName <- j
    
    # For testing:
    #FName <- "UK01 qual.csv"
    
    # Read in the qualitative data .csv file
    df <- read.csv(paste0(dir_engagements_online, "/qual/", FName), stringsAsFactors=F, header=T, check.names=FALSE)
    
    # Rename the columns
    names(df)[4]<-"comments"
    names(df)[3]<-"crit_label" 
    names(df)[2]<-"score_type" 
    names(df)[1]<-"option_label" 
    
    # Convert the score type to lower case, e.g. optimistic
    df$score_type <- tolower(df$score_type)
    
    # Set the file_name variable to be the first part of the file name,
    # i.e. remove " qual.csv" from the file name
    df$file_name <- gsub(" qual.csv","", FName)
    
    # If the counter is 1, i.e. the first time through the loop
    if (i==1) {
      
      # The qualitative engagement data is the df
      engagement_qual = df 
      
    }else{ # Otherwise
      
      # Add the df to the qualitative engagement data
      engagement_qual = bind_rows(engagement_qual, df) 
    }
    
    # Add 1 to the counter
    i=i+1
  } 
  
  # Strip white space from the start and end of variables
  engagement_qual <- engagement_qual %>% 
    mutate(across(where(is.character), str_trim))
}

#=============================================
# 2.3 Get weighting data
#=============================================

# MCM Data about weights - for online engagements
Files_weighting<-list.files(dir_weighting)

# Get the weighting data from the online engagements (if there are any)
if(length(Files_weighting) > 0){
  
  # Set a counter, i to 1
  i=1
  
  # For all of the files in the weighting folder
  for(j in Files_weighting){
    
    # Set the file name
    FName <- j
    
    # For testing:
    #FName <- "UK01 weighting.csv"
    
    # Read in the weighting .csv file
    df <- read.csv(paste0(dir_weighting, "/", FName), stringsAsFactors=F, header=T, check.names=FALSE)
    
    # Rename the criterion variable
    names(df)[2]<-"crit_label"
    
    # If the counter is 1, i.e. the first time through the loop
    if (i==1) {
      
      # The weights data is the df
      weights = df 
      
    }else{ # Otherwise
      
      # Add the df to the weights data
      weights = bind_rows(weights, df) 
    }
    
    # Add 1 to the counter
    i=i+1
  } 
  
  # Strip white space from the start and end of variables
  weights <- weights %>% mutate(across(where(is.character), str_trim))
}

#=======================================================
# 2.4 Combine the data from the online engagement files
#=======================================================

# If there is weighting data
if(length(Files_weighting) > 0){
  
  # If there are quant, qual and weights files, i.e. if there are all online files
  if((exists("engagement_quant")) & (exists("engagement_qual")) & (exists("weights"))){
    
    # Print a message
    print("All online data")
    
    # Combine the quant and qual engagement data
    engagement_online <- left_join(engagement_quant, engagement_qual, 
                                   by=c('option_label', 'score_type', 'crit_label', 'file_name')) 
    
    # Combine the quant and qual engagement data with the weights
    engagement_online <- left_join(engagement_online, weights, 
                                   by=c('crit_label', 'file_name')) 
    
    # Select the file_name variable and then everything else
    engagement_online <- engagement_online %>% 
      select(file_name, option_label, crit_label, score_type, score, comments, weight)
    
  } else if ((exists("engagement_quant")) & (!exists("engagement_qual")) & (exists("weights"))) {
    
    # If there are no notes, print a message
    print("Online data no notes")
    
    # Combine the engagement data with the weights
    engagement_online <- left_join(engagement_quant, weights, 
                                   by=c('crit_label', 'file_name')) 
    
    # There are no comments, so set the comments variable to NA
    engagement_online$comments <- NA
    
  } else { # Otherwise
    
    # There is either incomplete online data or no online data
    print("Incomplete online data / No online data")
    
  }
}

#===============================================
# 2.5 Get core options data
#===============================================

# Get MCM core options - for online engagements

# Set the file name for the core options
FName <- "core options.csv"

# Create core options if there is a core options file
core_options <- tryCatch({
  core_options <- read.csv(file=paste0(dir_core_options, "/", FName), 
                           stringsAsFactors	= F, header = T, encoding = "UTF-8")
}, warning = function(war) {
  # Warning handler picks up where warning was generated
  print(paste("MY_WARNING:  ",war))
  return(NA)
}, error = function(err) {
  # Error handler picks up where error was generated
  print(paste("MY_ERROR:  ",err))
  return(NA)
}, finally = {
  print("Core options code completed")
}) # END tryCatch

# If there are core options
if(is.data.frame(core_options)){
  
  # Set the engagement option type (core or additional)
  engagement_online$option_type <-
    
    # If the engagement option is in the list of core options
    ifelse((engagement_online$option_label %in% core_options$option_label),
           "core", # then the type is core
           "additional") # otherwise, the type is additional
  
} else{ # Otherwise
  
  #Print a message saying there's no core options data
  print("No core options data")
}

#===============================================
# 3. Get all data from offline engagement files
#===============================================

# MCM Data from offline engagements 
Files_offline<-list.files(dir_engagements_offline)

# Get the data from the offline engagements (if there are any)
if(length(Files_offline) > 0){
  
  # Set a counter, i to 1
  i=1
  
  # For each offline file
  for(j in Files_offline){
    
    # Set the file name and print it to the screen
    FName <- j
    print(FName)
    
    # For testing
    #FName <- "NL_7.engagement"
    
    # Create a dataframe from the engagement file
    df <- jsonlite::fromJSON(paste0(dir_engagements_offline, "/", FName))
    
    # Get the options
    options <- df$engagement$options %>% 
      select(id, label, type) %>% rename(option_type = type)
    
    # If we want to inspect the changes
    changes <- df$engagement$changes
    
    # If there are criteria weights
    if(!is.null(df$engagement$criteria$weight)){
      
      # Get the criteria including weights
      criteria <- df$engagement$criteria %>% select(id, label, weight)
      
    } else { #Otherwise
      
      # Get the (singular) criterion label
      criteria <- df$engagement$criteria %>% select(id, label)
      
      # If there is only 1 criterion
      if(nrow(criteria)==1){
        
        # Set the weight to 100
        criteria$weight <- 100
        
      } else { # Otherwise
        
        # Print warning messages
        print("There are multiple criteria with no weights.")
        print("Look carefully at engagement:")
        print(df$engagement$participant_name)
        print("And consider using online data from a ranks chart for this engagement.")
        print("Instead of the data from the offline engagement.")
        
        # Set the weight(s) to 100
        criteria$weight <- 100
      }
    }
    
    # Get the scores
    scores <- df$engagement$scores
    
    # Get the notes
    notes <- df$engagement$notes %>% 
      select(id, text, object, created)
    
    # Merge the scores and notes
    scores_notes <- merge(scores, notes, by.x = "id", by.y = "object", all = TRUE) %>%
      rename(id_notes = id.y) %>% 
      merge(criteria, by.x = "criterion", by.y = "id", all = TRUE) %>%
      merge(options, by.x = "option", by.y = "id", all = TRUE) %>%
      rename(crit_label = label.x, option_label = label.y, score_type = type)
    
    # If there is a principle
    if("decision" %in% colnames(scores_notes)){
      
      # Concatenate comments including decision
      scores_notes <- scores_notes %>%
        arrange(option_label, option_type, crit_label, weight, score_type, decision, score, created) #%>%
        #group_by(option_label, option_type, crit_label, weight, score_type, decision, score) %>%
        #summarise(comments = paste(text, collapse="// "), .groups = 'drop')
      
    } else { # Otherwise
      
      # Concatenate comments without decision
      scores_notes <- scores_notes %>%
        arrange(option_label, option_type, crit_label, weight, score_type, score, created) #%>%
        #group_by(option_label, option_type, crit_label, weight, score_type, score) %>%
        #summarise(comments = paste(text, collapse="// "), .groups = 'drop')
      
      # Set the decision to NA
      scores_notes$decision <- NA
    }
    
    # Add the file name
    scores_notes$file_name <- df$engagement$participant_name
    
    # If the counter is 1
    if (i==1) {
      engagement_offline = scores_notes # The engagement is the scores and notes
      
    } else { # Otherwise
      engagement_offline = bind_rows(engagement_offline, scores_notes) # Add the scores and notes
    }
    
    # Add 1 to the counter
    i=i+1
  }  
}

# If there are offline engagements
if(exists("engagement_offline")){
  
  # Select the variables 
  engagement_offline <- engagement_offline %>% 
    select(file_name, option_label, option_type, crit_label, score_type, decision, score, text, weight, created)
  
  # Select other data, i.e. if crit_label or score is NA, e.g. weighting notes and principle notes
  other_data <- engagement_offline %>% filter(is.na(score) | is.na(crit_label))
  
  # Filter out other data, e.g. weighting comments, i.e. if crit_label and score are both not NA
  # And remove the decision column because we are only interested in scores
  engagement_offline <- engagement_offline %>% filter(!is.na(score) & !is.na(crit_label)) %>%
    select(-decision) %>%
    group_by(file_name, option_label, option_type, crit_label, score_type, score, weight) %>%
    summarise(comments = paste(text, collapse="// "), .groups = 'drop')
  
  # Convert offline scores to numbers
  engagement_offline$score <- as.numeric(engagement_offline$score)
}

#================================================
# 4. Merge data from different sources together
#================================================

# If there are weighting files, offline engagements and online files
if((length(Files_weighting) > 0) & (exists("engagement_offline")) & 
   (exists("engagement_online"))){
  
  # Need to add core/additional to online engagements
  
  # Add an option_type of 'additional'
  engagement_online$option_type <- "additional"
  
  # Get the core options from the offline engagements
  core_options <- engagement_offline %>% select(option_label, option_type) %>% 
    unique() %>% filter(option_type=='core') %>% select(option_label)
  
  # Filter the core options
  engagement_online_core <- engagement_online %>%
    filter(option_label %in% core_options$option_label)
  
  # Label them as core
  engagement_online_core$option_type <- "core"
  
  # Filter the additional options
  engagement_online_additional <- engagement_online %>%
    filter(!(option_label %in% core_options$option_label))
  
  # Add the core and additional together
  engagement_online_with_type <- bind_rows(engagement_online_core, engagement_online_additional) %>%
    select(file_name, option_label, option_type, crit_label, score_type, score, comments, weight)
  
} else if ((length(Files_weighting) > 0) & (!exists("engagement_offline")) & 
           (exists("engagement_online"))) { 
  
  # Else if there are online engagements but not offline engagements
  
  # Select the file_name variable and then everything else
  engagement_online_with_type <- engagement_online %>% 
    select(file_name, option_label, option_type, crit_label, score_type, score, comments, weight)
  
} else { # Otherwise
  
  print("No online engagements")
}

#If there are weighting files and both online and offline engagements
if((length(Files_weighting) > 0) & (exists("engagement_offline")) & (exists("engagement_online_with_type"))){
  
  # Add the online and offline together
  engagement_all <- bind_rows(engagement_offline, engagement_online_with_type)
  
} else if (exists("engagement_offline")) { # Otherwise if there are offline engagements
  
  # The engagements are the offline engagements
  engagement_all <- engagement_offline
  
} else { # Otherwise
  
  print("No offline engagements")
  
  # The engagements are the online engagements
  engagement_all <- engagement_online_with_type
}

# Convert long decimals to characters for printing
engagement_all_char <- engagement_all %>% mutate_if(is.numeric, as.character )

# Write the dataset of scores and comments
write_excel_csv(engagement_all_char, file=paste0(dir_output, 
                                                 "/Merged_MCM_Dataset_w_Comments.csv"))

# If there are offline engagements
if(exists("engagement_offline")){
  
  # Convert long decimals to characters for printing
  other_data_char <- other_data %>% mutate_if(is.numeric, as.character )
  
  # Write the dataset of other data - weighting notes, other notes
  write_excel_csv(other_data_char, file=paste0(dir_output, "/MCM_Offline_Engagement_Other_Data.csv"))
}