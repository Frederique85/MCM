# Routine to convert MCM csv into one dataframe
# By Fred Bone - for MCM 
##############################################

library(dplyr)
library(tidyr)

########################
#### DATA TO ADJUST ####
########################

# Folder countaining the engagments
dir_input <- "/Users/fl49/Desktop/MCM/csv/"

# Folder in which the outputs should be saved
dir_output <- "/Users/fl49/Desktop/MCM/csv/"

# Folder of the offline engagement
dir_engagements <- "/Users/fl49/Desktop/MCM/online engagements/"

# Folder of weighting for the offline engagement
dir_weighting <- "/Users/fl49/Desktop/MCM/weighting/"
#########################
#### END DATA ADJUST ####
#########################


#=================================================
# 1. Set the directory 
#=================================================

# Mac laptop
setwd(dir_input)

#=================================================
# 2. Read and import background files 
#=================================================

# MCM Data from Offline engagements
FName <- "MCM_Overall_Rawdata.csv"
master_df <- read.csv(FName, stringsAsFactors	= F, header= T, encoding = "UTF-8")

# MCM for stakeholder look-up
# FName <- "MCM_Stakeholder2.csv"
# Stakeholders <- read.csv(FName, stringsAsFactors	= F, header= T, encoding = "UTF-8")

# Prepare country look-up
# Code      <- c("GR", "NL", "SP", "UK", "IT", "DE")
# Country   <- c("Greece", "Netherlands", "Spain", "United Kingdom", "Italy", "Germany")
# Country_key <- data.frame(Code, Country)

# MCM Data from Offline engagements 
Files<-list.files(dir_engagements)
firstline<-0
setwd(dir_engagements)
i=1
for(j in Files){
FName <- j
UK01 <- read.csv(FName, stringsAsFactors	= F, header= T, check.names=FALSE)
names(UK01)[2]<-"score_type" 
names(UK01)[1]<-"option_label" 
UK01$score_type <- tolower(UK01$score_type)
UK01<-gather(UK01, key="criteria",value="criteria_score" , -c(option_label, score_type))
UK01$file_name <- gsub(".csv","", FName)
UK01 <- data.frame(UK01)%>%
  mutate(option_label=gsub("^\\s+|\\s+$", "", option_label))%>%
  mutate(criteria=gsub("^\\s+|\\s+$", "", criteria))

if (i==1) {
  engagement =UK01
}else{
  engagement = bind_rows(engagement, UK01)
}
i=i+1
} 

# Data about weights (for UK01)
Files<-list.files(dir_weighting)
firstline<-0
setwd(dir_weighting)
i=1
for(j in Files){
  FName <- j
  UK01 <- read.csv(FName, stringsAsFactors	= F, header= T, check.names=FALSE)
  UK01 <- data.frame(UK01) %>%
    mutate(file_name=gsub("^\\s+|\\s+$", "", file_name))%>%
    mutate(criteria=gsub("^\\s+|\\s+$", "", criteria))
  
  if (i==1) {
    Weights = UK01
  }else{
    Weights = bind_rows(engagement, UK01)
  }
  i=i+1
} 

# Combine the engagements with the weighting
engagement <- left_join(engagement, Weights) 
engagement <- engagement %>% 
  select(file_name, everything())

# Filter only core options
core_options <- master_df %>%
  select(option_label, option_type)%>%
  unique()%>%
  filter(option_type=="core")


engagement <- left_join(engagement, core_options)
engagement<- engagement %>%
  filter(option_type=="core")

setwd(dir_input)
#========================================================================================
# 3. Prepare and integrate the data in a single dataframe (keep only quanti data)
#========================================================================================

# 3.1. Only keep quanti data for the master (change name from master to df)
df <- master_df %>%
  select(file_name, option_label, option_type, crit_label, weight, score_type, score)

# 3.2. Merge with UK01
df <- merge(df, engagement, by.x=c("file_name", "option_label", "option_type", "crit_label", "weight", "score_type", "score"), by.y=c("file_name", "option_label", "option_type", "criteria", "weight", "score_type", "criteria_score") , all = TRUE )

# 3.3. Add the stakeholder groups
# df <- merge(df, Stakeholders, by.x = c("file_name"), by.y=c("Id"), all.x = TRUE )
# colnames(df)[colnames(df) == "Group"] <- "stakeholder_group"

# 3.4. Add country codes to the master
# df$Code <- substring(df$file_name, 1, 2)
# df <- merge(df, Country_key, key=Code)
# df$Code<-NULL

# 3.5. Keep core only and remove the core column
df <- df %>%
  filter(option_type=="core")%>%
  select(-option_type)

# 3.6. Optimistic and pessimistic scores as variables
df <- df %>%
  tbl_df() %>%
  group_by(score_type)%>%
  mutate(i1 = row_number()) %>% 
  spread(score_type, score) %>%
  select(-i1)



# 3.6. Remove all the unnecessary files
# rm(Country_key)
# rm(Stakeholders)
rm(UK01)
rm(Weights)

setwd(dir_output)
write.csv(df, file="Merged_MCM_Dataset.csv")

#==========================================================================================
# 4. Prepapre a separate dataframe for the summary files (Final picture for stakeholders)
#==========================================================================================

# 4.1. Compute normalised weight (make sure the weights add up to one)
#              _________________

working_df <- df %>%
  group_by(file_name, option_label)%>%
  mutate(sum_weight = sum(weight))%>%
  ungroup()

working_df <- working_df %>%
  mutate(normalised_weight = weight/sum_weight)

# 4.2. Compute the weighted scores for each criteria
#                  _______________
working_df <- working_df %>%
  group_by(file_name) %>%
  group_by(crit_label) %>%
  mutate(Max_Op_Crit=max(optimistic)) %>%
  mutate(Min_Op_Crit=min(optimistic)) %>%
  mutate(Max_Pe_Crit=max(pessimistic)) %>%
  mutate(Min_Pe_Crit=min(pessimistic))  %>%
  ungroup()

working_df <- working_df %>%  
  mutate(optimistic_normalised = (optimistic-Min_Pe_Crit) / (Max_Op_Crit-Min_Pe_Crit)) %>%
  mutate(pessimistic_normalised = (pessimistic-Min_Pe_Crit) / (Max_Op_Crit-Min_Pe_Crit)) 

working_df <- working_df %>%
  mutate(optimistic_weighted = normalised_weight*optimistic_normalised) %>%
  mutate(pessimistic_weighted = normalised_weight*pessimistic_normalised)

# 4.3. Compute the overall option score  and create a new df for final scores
#                  ____________________
df_finalscore <- working_df %>%
  select(file_name, option_label, crit_label, optimistic_weighted, pessimistic_weighted ) %>%
  group_by(file_name, option_label) %>%
  mutate(optimistic_overall=sum(optimistic_weighted)) %>%
  mutate(pessimistic_overall=sum(pessimistic_weighted)) %>%
  select(-optimistic_weighted, -pessimistic_weighted, -crit_label) %>%
  ungroup()%>%
  unique()

# 4.5. Remove working_df
rm(working_df)

# 4.6. Export the final picture
setwd(dir_output)
write.csv(df_finalscore, file="MCM_finalscore.csv")

