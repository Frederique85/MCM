########################################################
# Routine to convert MCM csv into final ranks          #
# By Fred Bone - for MCM                               #
# Updated by Josie Coburn - for MCM                    #
# Data needed: Merged_MCM_Dataset_w_Comments.csv       #
# (from 1. Get data from engagements.R),               #
# containing scores, notes, and weights for all        #
# You must also create an "csv" folder                 #
# Data outputs: Merged_MCM_Dataset.csv,                # 
# containing scores and weights for all engagements,   #
# and MCM_Final_Ranks.csv, containing final ranks for  #
# all engagements.                                     #
########################################################

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

# Folder containing the output data from the previous R script - "1. Get data from engagements.R"
# and in which the outputs from this script should be saved
dir_output <- paste0(dir_MCM_project, "/csv")

#########################
#### END DATA ADJUST ####
#########################

#=================================================
# 1. Set the directory 
#=================================================

# Set the working directory
setwd(dir_output)

#=================================================
# 2. Read in data from files 
#=================================================

# MCM data from engagements from 1. Get data from offline engagements
FName <- "Merged_MCM_Dataset_w_Comments.csv"
df_all <- read.csv(FName, stringsAsFactors	= F, header = T, encoding = "UTF-8")

#========================================================================================
# 3. Prepare and integrate the data in a single dataframe (keep only quanti data)
#========================================================================================

# Filter only core options
core_options <- df_all %>% filter(option_type=="core")

# Only keep quanti data for the core options 
df <- core_options %>%
  select(file_name, option_label, option_type, crit_label, weight, score_type, score)

# Keep core only and remove the core column
df <- df %>% filter(option_type=="core") %>% select(-option_type)

# Optimistic and pessimistic scores as variables
df <- df %>%
  as_tibble() %>%
  group_by(score_type)%>%
  mutate(i1 = row_number()) %>% 
  spread(score_type, score) %>%
  select(-i1)

# Convert long decimals to characters for printing
df_char <- df %>% mutate_if(is.numeric, as.character )

# Write the dataset of scores and comments
write_excel_csv(df_char, file=paste0(dir_output, "/Merged_MCM_Dataset.csv"))

#==========================================================================================
# 4. Prepare a separate dataframe for the summary files (Final picture for stakeholders)
#==========================================================================================

# 4.1. Compute normalised weight (make sure the weights add up to one)

df_working <- df %>%
  group_by(file_name, option_label)%>%
  mutate(sum_weight = sum(weight))%>%
  ungroup()

df_working <- df_working %>%
  mutate(normalised_weight = weight/sum_weight)

# 4.2. Compute the weighted scores for each criteria

# NB. Added .add=TRUE here so that optimistic and pessimistic scores are
# gathered for only crit_label, option and participant, and not for all participants.
df_working <- df_working %>%
  group_by(file_name) %>%
  group_by(crit_label, .add=TRUE) %>%
  mutate(Max_Op_Crit = max(optimistic)) %>%
  mutate(Min_Op_Crit=min(optimistic)) %>%
  mutate(Max_Pe_Crit=max(pessimistic)) %>%
  mutate(Min_Pe_Crit=min(pessimistic)) %>%
  ungroup()

df_working <- df_working %>%  
  mutate(optimistic_normalised = (optimistic-Min_Pe_Crit) / (Max_Op_Crit-Min_Pe_Crit)) %>%
  mutate(pessimistic_normalised = (pessimistic-Min_Pe_Crit) / (Max_Op_Crit-Min_Pe_Crit)) 

df_working <- df_working %>%
  mutate(optimistic_weighted = normalised_weight*optimistic_normalised) %>%
  mutate(pessimistic_weighted = normalised_weight*pessimistic_normalised)

# 4.3. Compute the overall option score and create a new df for final scores

df_final_rank <- df_working %>%
  select(file_name, option_label, crit_label, optimistic_weighted, pessimistic_weighted ) %>%
  group_by(file_name, option_label) %>%
  mutate(optimistic_overall=sum(optimistic_weighted)) %>%
  mutate(pessimistic_overall=sum(pessimistic_weighted)) %>%
  select(-optimistic_weighted, -pessimistic_weighted, -crit_label) %>%
  ungroup()%>%
  unique()

# 4.6. Export the final picture

# Convert long decimals to characters for printing
df_final_rank_char <- df_final_rank %>% mutate_if(is.numeric, as.character )

# Write the dataset of scores and comments
write_excel_csv(df_final_rank_char, file=paste0(dir_output, "/MCM_Final_Ranks.csv"))

