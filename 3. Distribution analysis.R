########################################################
# Distribution / Robustness Analysis                   #
# BONE II charts                                       #
# Do pairwise comparisons between policies             #
# By Fred Bone for MCM                                 #
# Updated by Josie Coburn for MCM                      #
# Data needed:  MCM_Final_Ranks.csv                    #
# (from 2. Transform data.R)                           #
# You must also create an "analysis" folder            #
# Data outputs: pairwise inclination data in the       # 
# folder "analysis/pairwise data", pairwise graphs in  #
# the folder "analysis/pairwise graphs", and merit     #
# order data and graphs in "analysis/merit order"      #
########################################################

# Remove everything from the Global Environment
rm(list=ls())

# Call libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggrepel)
library(forcats)

########################
#### DATA TO ADJUST ####
########################

# Project folder containing all other folders and files
dir_MCM_project <- "INSERT YOUR PROJECT FILE PATH"

# Folder containing the data from the previous R script - "2. Transform data.R"
dir_input <- paste0(dir_MCM_project, "/csv")

# Folder in which the outputs should be saved
dir_output <- paste0(dir_MCM_project, "/analysis")

#########################
#### END DATA ADJUST ####
#########################

#=================================================
# 1. Set the directory 
#=================================================

# Set the directory
setwd(dir_input)

#=================================================
# 2. Read and import background files 
#=================================================

# MCM data from offline engagements
FName <- "MCM_Final_Ranks.csv"
df_final_ranks <- read.csv(FName, stringsAsFactors	= F, header= T, encoding = "UTF-8")

# Replace "/" with "-" in option labels because "/" causes problems with the code
df_final_ranks$option_label <- str_replace_all(df_final_ranks$option_label, '/', '-')

################################################################################
####            THE FOLLOWING CODE IS PROJECT SPECIFIC                     #####
#### EDIT THIS IF YOU HAVE GROUPINGS LIKE COUNTRIES AND STAKEHOLDER GROUPS #####
################################################################################

# The following code must be edited as it is project specific!

# Prepare stakeholder group look-up
# A stakeholder group folder
#folder_stakeholder <- "stakeholder groups - AMR example"
#dir_stakeholder <- file.path(dir_MCM_project, folder_stakeholder)

# Get the stakeholder groups from the stakeholder groups file
#FName <- "/stakeholder groups.csv"
#Stakeholders <- read.csv(paste0(dir_stakeholder, FName), stringsAsFactors = F, header = T, encoding = "UTF-8")

# Prepare country look-up
#code      <- c("DE", "NL", "UK", "SP", "GR", "IT")
#country   <- c("Germany", "Netherlands", "United Kingdom", "Spain", "Greece", "Italy")
#code      <- c("D ", "NL")
#country   <- c("Germany", "Netherlands")
#country_key <- data.frame(code, country)

# Add country codes to the df
#df_final_ranks$code <- substring(df_final_ranks$file_name, 1, 2)
#df_final_ranks <- merge(df_final_ranks, country_key, key=code)
#df_final_ranks$code<-NULL

# Add the stakeholder groups
#df_final_ranks <- merge(df_final_ranks, Stakeholders, by.x = c("file_name"), by.y=c("Id"), all.x = TRUE )
#colnames(df_final_ranks)[colnames(df_final_ranks) == "Group"] <- "stakeholder_group"

# Set up levels - overall, country, and stakeholder_group (and others if needed,
# if there is additional data on stakeholder groups or country or other relevant variables)
levels <- c("Overall")
#levels <- c("Overall", "country")
#levels <- c("Overall", "country", "stakeholder_group") 

#######################################
#### END OF PROJECT SPECIFIC CODE #####
#######################################

#=================================================================
# 3. Function to compute the vector for Distribution analysis 
#       Inputs: Policy 1, Policy 2, Type (overall, variable name)
#       Outputs: two columns vector
#=================================================================

# Get policies from the option labels
Policies <- unique(df_final_ranks$option_label)

# Function to get direction and separation values for pairs of policies
# and add them to a dataframe, df
MCM_Distrib <- function(df, Policy1, Policy2) {
  
  #For testing, create Policy1 and Policy2 variables
  #df_final_ranks$option_label <- str_trim(df_final_ranks$option_label)
  #Policy1 <- "Low Energy bulbs"
  #Policy2 <- "Power down"
  
  # Print the two policies to compare
  print(Policy1)
  print(Policy2)
  
  #-------------------------------------------
  # 3.1. Transform dataframe for two policies
  #-------------------------------------------
  
  # Filter the dataframe to only the two policies
  df <- df_final_ranks %>%
    filter(option_label %in% c(Policy1,Policy2)) 
  
  #--------------------------------------------------------------------------
  # 3.2. Flip dataframe to have each optimistic and pessimistic as variables
  #--------------------------------------------------------------------------
  df <- df %>%
    as_tibble() %>%
    unite(level, optimistic_overall, pessimistic_overall, sep=" / ") %>%
    group_by(option_label) %>%
    mutate(i1 = row_number()) %>%
    spread(option_label, level) %>%
    select(-i1) %>%
    separate(Policy1, into=c("Optimistic1", "Pessimistic1"), sep=" / ", convert=TRUE) %>%
    separate(Policy2, into=c("Optimistic2", "Pessimistic2"), sep=" / ",convert=TRUE)
  
  #-----------------------
  # 3.3. Compute direction
  #-----------------------
  # Direction consists of three scale points
  df <- df %>%
    mutate(mean_diff = (((Optimistic1+Pessimistic1)/2))-((Optimistic2+Pessimistic2)/2)) %>%
    mutate(direction_outline = ifelse(mean_diff>0,1,ifelse(mean_diff<0,-1,0))) %>%
    mutate(direction_binary = ifelse(Optimistic1<=Optimistic2 & Pessimistic1>=Pessimistic2, 0,
                        ifelse(Optimistic1>=Optimistic2 & Pessimistic1<=Pessimistic2, 0,1))) %>%
    select(-mean_diff)
  
  #-------------------------
  # 3.4. Compute separation
  #-------------------------
  df <- df %>%
    rowwise() %>%
    mutate(maxlocal= max(Optimistic1,Optimistic2))%>%
    mutate(minlocal= min(Pessimistic1,Pessimistic2))
  
  df <- df %>%
    #mutate(separation=ifelse(maxlocal==minlocal,0,((abs((Optimistic1-Optimistic2)+(Pessimistic1-Pessimistic2))/(maxlocal-minlocal))/2))) %>%
    mutate(separation=ifelse(maxlocal==minlocal,0,((abs((Pessimistic1-Pessimistic2)+(Optimistic1-Optimistic2))/(maxlocal-minlocal))/2))) %>%
    select (-Optimistic1, -Optimistic2, -Pessimistic1, -Pessimistic2, -maxlocal, -minlocal) %>%
    mutate(direction_fill=direction_outline*abs(direction_binary)) %>%
    arrange(ifelse(direction_outline<0,separation,-separation)) %>%
    arrange(-direction_fill)
  
}

#=================================================================
# 4. Function to compute the visual / using only the right data
#       Inputs: Policy 1, Policy 2, Type (overall, variable name)
#       Outputs: two columns vector
#=================================================================

# Design all the plots
MCM_robust_plot <- function(result, level, Policy1, Policy2) {
  
  #-------------------------
  # 4.1. Compute separation
  #-------------------------
  # Filter for level
  if(level=="stakeholder_group") {
    item <- unique(result$stakeholder_group)

  } else if (level=="country") {
    item <- unique(result$country)

  } else{
    item <- "Overall"
    dir.create(file.path(paste0(dir_output, "/pairwise graphs"), "Overall"), showWarnings = FALSE)
    setwd(file.path(paste0(dir_output, "/pairwise graphs"), "Overall"))
  }
  
  # Go through each item within levels
  for (i in 1:length(item)){

    if(level=="stakeholder_group") {
      result2 <- result %>% 
        filter(stakeholder_group==item[i])
    } else if (level=="country") {
      result2  <- result %>% 
        filter(country==item[i])
    } else{
      result2  <- result
    }
    
    result2 <- result2 %>%
      select (direction_outline, direction_fill, separation)
    
    result2$x1=1:nrow(result2)
    
    result2$direction_outline <- as.character(result2$direction_outline)
    
  #-------------------------
  # 4.2. Create the plot
  #-------------------------  
    cols <- c("-1" = "tomato2", "1" = "royalblue2", "0" = "grey93", "2"="white")
    name <- item[i]
    print(name)
    
    dir.create(file.path(paste0(dir_output, "/pairwise graphs"), name), showWarnings = FALSE)
    setwd(file.path(paste0(dir_output, "/pairwise graphs"), name))
    
    Plot <- ggplot()+
      geom_rect(data=result2, mapping=aes(xmin=x1, xmax=x1+1, ymin=0, ymax=1, 
                                          fill=as.character(direction_outline), alpha=separation) )+
      geom_rect(data=result2, mapping=aes(xmin=x1+0.05, xmax=x1+0.95, ymin=0.05, ymax=0.95, fill="2", alpha=1) )+
      geom_rect(data=result2, mapping=aes(xmin=ifelse(direction_outline==0, x1,x1+0.05), 
                                          xmax=ifelse(direction_outline==0, x1+1,x1+0.95), 
                                          ymin=ifelse(direction_outline==0, 0, 0.05), 
                                          ymax=ifelse(direction_outline==0, 1,0.95), 
                                          fill=as.character(direction_fill), 
                                          alpha=ifelse(direction_fill==0, 1,separation)) )+
      scale_fill_manual(values = cols)+
      scale_y_continuous(limits = c(0, 1))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank(),
            axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank(),
            plot.margin=unit(c(0,0,0,0),"mm")) +
      theme(legend.position="none") 
        
    f_name <-paste(Policy1,"vs", Policy2, level, item[i], sep = "_")
    ggsave(plot = Plot, filename = paste(f_name,"png", sep="."), units = "in", 
           dpi=150, height=1, width=10, limitsize = FALSE)
    print("done")
    
  }
}

# Rename the dataframe
df<-df_final_ranks

# Create the graphs folder
dir.create(file.path(dir_output, "pairwise data"), showWarnings = FALSE)
dir.create(file.path(dir_output, "pairwise graphs"), showWarnings = FALSE)

# Save all the pairwise data and charts
for (j in 1:length(Policies)) {
  
  for (k in 1:length(Policies)) {
    
    if (Policies[j] != Policies[k]){
      
      print("k")
      print(k)
      
      result <- MCM_Distrib(df_finalscore, Policies[j], Policies[k])
      
      f_name <-paste(Policies[j],"vs", Policies[k], sep = "_")
      setwd(file.path(dir_output, "pairwise data"))
      write.csv(result, file=paste(f_name,".csv"))
      
      print("level1")
      
      for (m in 1:length(levels)) {
        
        print("level2")
        
        plot<-MCM_robust_plot(result,levels[m], Policies[j], Policies[k])
      }
    }
  }
}

# Make a dataframe with all pairs of policies and then compute the merit scores
for (j in 1:length(Policies)) {
  
  for (k in 1:length(Policies)) {
    
    if (Policies[j] != Policies[k]){
      
      print("k")
      print(k)
      
      if (j==1 & k==2) {
        
        Merit_df <- MCM_Distrib(df_final_ranks, Policies[j], Policies[k])
        Merit_df$Policy1 <- Policies[j]
        Merit_df$Policy2 <- Policies[k]
      }
      
      else {
        
        temp <- MCM_Distrib(df_final_ranks, Policies[j], Policies[k])
        temp$Policy1 <- Policies[j]
        temp$Policy2 <- Policies[k]
        Merit_df <-rbind(Merit_df, temp)
      }
    }
  }
}

# Create a folder for the Merit scores
dir.create(file.path(dir_output, "merit order"), showWarnings = FALSE)

Merit_df_overall <- Merit_df %>%
  select("Policy1", "Policy2", "direction_outline", "direction_fill", "separation")%>%
#  select("Policy1", "Policy2", "stakeholder_group", "country", "direction_outline", "direction_fill", "separation")%>%
  group_by(Policy1, .add=TRUE)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))

f_name <- paste0("Merit_", "Overall")
setwd(file.path(dir_output, "merit order"))
write.csv(Merit_df_overall, file=paste0(f_name,".csv"))

Merit_df_overall <- read.csv("Merit_Overall.csv")

# Create related plot
# plot_Merit_df_overall  <- Merit_df_overall  %>%
#   ggplot(aes(x = "Overall", y = Merit_withgrey)) + 
#   scale_fill_viridis(discrete= TRUE)+
#   geom_label(aes(label= Policy1, fill = factor(Policy1)), colour="white", show.legend = F, fontface = "bold") +
#   theme_minimal(base_size = 15) +
#   labs(y = "Merit order")

# Set the number of rows in the legend according to the number of core options
temp <- df$option_label %>% unique() %>% length() / 2 
legend_rows <- temp %>% ceiling()

# You can choose Merit with No Gry or Merit with Grey
plot_Merit_df_overall  <- Merit_df_overall  %>%
  ggplot(aes(x = "Overall", y = Merit_withgrey)) + 
  scale_fill_viridis(discrete= TRUE)+
  scale_colour_viridis_d()+
  #theme(plot.background = element_rect(fill = 'white', colour = 'grey'))+
  geom_point(aes(color=factor(Policy1)), size=5, shape = 18)+
  #scale_shape_manual(values = c(23))+
  geom_text_repel(aes(label= Policy1), 
                  colour="Black", show.legend = F,
                  direction = "y",
                  nudge_x = -0.5,
                  min.segment.length = 0,
                  #point.padding = 0.25,
                  hjust        = 0.5,
                  segment.size = 0.2,
                  segment.color = "Black") +
  theme_minimal(base_size = 15) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(#legend.position="none", 
        legend.position='bottom', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.text = element_text(size = 10),
        axis.title.x = element_blank())+
  theme(text = element_text(size = 10))+
  guides(colour = guide_legend(nrow = legend_rows))+
  labs(y = "Merit order")

setwd(file.path(dir_output, "merit order"))
f_name <- paste0("Merit_Overall")
ggsave(plot = plot_Merit_df_overall, filename = paste(f_name,"png", sep="."), width = 5, height = 7)

######################################################################
############               !!! STOP HERE !!!                ##########
############           Unless you have countries            ##########
######################################################################

# country and stakeholder group
#Merit_df_country <- Merit_df %>%
#  select("Policy1", "Policy2", "stakeholder_group", "country", "direction_outline", "direction_fill", "separation")%>%
#  group_by(country, Policy1)%>%
#  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
#  arrange(desc(Merit_nogrey))%>%
#  arrange(country) %>%
#  ungroup()

# Make country dataframe
Merit_df_country <- Merit_df %>%
  select("Policy1", "Policy2", "country", "direction_outline", "direction_fill", "separation")%>%
  group_by(country, Policy1)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))%>%
  arrange(country) %>%
  ungroup()

# Write the dataframe to a file
f_name <- paste0("Merit_", "country")
setwd(file.path(dir_output, "merit order"))
write.csv(Merit_df_country, file=paste0(f_name,".csv"))

# Read in the dataframe
Merit_df_country <- read.csv("Merit_Country.csv")

# Create country related merit plot with Grey
plot_Merit_country <- Merit_df_country %>%
  ggplot(aes(x = country, y = Merit_withgrey)) + 
  scale_fill_viridis(discrete= TRUE)+
  scale_colour_viridis_d()+
  geom_point(aes(color=factor(Policy1)), size=5, shape = 18)+
  geom_text_repel(aes(label= Policy1), 
                  colour="Black", show.legend = F,
                  direction = "y",
                  nudge_x = -0.5,
                  min.segment.length = 0,
                  #point.padding = 0.25,
                  hjust        = 0.5,
                  segment.size = 0.2,
                  segment.color = "Black") +
  theme_minimal(base_size = 15) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(#legend.position="none", 
    legend.position='bottom', 
    legend.justification='left',
    legend.direction='horizontal',
    legend.text = element_text(size = 10),
    axis.title.x = element_blank())+
  theme(text = element_text(size = 10))+
  guides(colour = guide_legend(nrow = legend_rows))+
  labs(y = "Merit order")

# Print the graph to a file
setwd(file.path(dir_output, "merit order"))
f_name <- paste0("Country_Merit")
ggsave(plot = plot_Merit_country, filename = paste(f_name,"png", sep="."), width = 8, height = 8)

######################################################################
############               !!! STOP HERE !!!                ##########
############      Unless you have stakeholder groups        ##########
######################################################################

# Create a stakeholder dataframe
Merit_df_stakeholder <- Merit_df %>%
  select("Policy1", "Policy2", "stakeholder_group", "country", "direction_outline", "direction_fill", "separation")%>%
  group_by(stakeholder_group, Policy1)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))%>%
  arrange(stakeholder_group)

# Write the stakeholder data to a file
f_name <- paste0("Merit_stakeholder")
setwd(file.path(dir_output, "merit order"))
write.csv(Merit_df_stakeholder, file=paste0(f_name, ".csv"))

# Read in the stakeholder data from a file
Merit_df_stakeholder <- read.csv("Merit_stakeholder.csv")

# Create stakeholder related merit plot with Grey
plot_Merit_stakeholder <- Merit_df_stakeholder %>%
  ggplot(aes(x = stakeholder_group, y = Merit_withgrey)) + 
  scale_fill_viridis(discrete= TRUE)+
  scale_colour_viridis_d()+
  geom_point(aes(color=factor(Policy1)), size=5, shape = 18)+
  geom_text_repel(aes(label= Policy1), 
                  colour="Black", show.legend = F,
                  direction = "y",
                  nudge_x = -0.5,
                  min.segment.length = 0,
                  #point.padding = 0.25,
                  hjust        = 0.5,
                  segment.size = 0.2,
                  segment.color = "Black") +
  theme_minimal(base_size = 15) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(#legend.position="none", 
    legend.position='bottom', 
    legend.justification='left',
    legend.direction='horizontal',
    legend.text = element_text(size = 10),
    axis.title.x = element_blank())+
  theme(text = element_text(size = 10))+
  guides(colour = guide_legend(nrow = legend_rows))+
  labs(y = "Merit order")

# Write the stakeholder merit plot to a file
setwd(file.path(dir_output, "merit order"))
f_name <- paste0("Stakeholder_Merit")
ggsave(plot = plot_Merit_stakeholder, filename = paste(f_name,"png", sep="."), width = 13, height = 8)


