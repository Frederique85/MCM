# Distribution / Robustness Analysis
#     BONE II charts
#     Do pairwise comparisons between policies
#     By Fred Bone for MCM
#     Data needed:  MCM_finalscore.csv (from 2.TransformDataOnly)
#                   
#################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggrepel)
library(forcats)

#Remove everything from the Global Environment
rm(list=ls())

########################
#### DATA TO ADJUST ####
########################

# Folder countaining the engagments
dir_input <- "/XXX/MCM/csv/"

# Folder in whihc the outputs should be saved
dir_output <- "/XXX/MCM/analysis/"

#########################
#### END DATA ADJUST ####
#########################

#=================================================
# 1. Set the directory 
#=================================================
setwd(dir_input)

#=================================================
# 2. Read and import background files 
#=================================================
# MCM Data from Offline engagements
FName <- "MCM_finalscore.csv"
df_finalscore <- read.csv(FName, stringsAsFactors	= F, header= T, encoding = "UTF-8")
df_finalscore <- df_finalscore %>%
  select(-X)

#=================================================================
# 3. Function to compute the vector for Distribution analysis 
#       Inputs: Policy 1, Policy 2, Type (overall, variable name)
#       Outputs: two columns vector
#=================================================================

# Temp befor the function is fully built
Policies <- unique(df_finalscore$option_label)
levels <- c("Overall")
# levels <- c("Overall", "Country", "stakeholder_group") # this is only relevant if there is additional data on stakeholdergroups or country or other relevant variables
# Function to get the right scores
MCM_Distrib <- function(df, Policy1, Policy2) {
  print(Policy1)
  print(Policy2)
  
  #------------------------------------
  # 3.1. Transform df for two policies
  #------------------------------------
  df <- df_finalscore %>%
    filter(option_label %in% c(Policy1,Policy2)) 
  
  #---------------------------------------------------------------
  # 3.2. Flip to have each optimistic and pessimistic as variables
  #---------------------------------------------------------------
  df <- df %>%
    tbl_df() %>%
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
  # direction three scale points
  df <- df %>%
    mutate(mean_diff=(((Optimistic1+Pessimistic1)/2))-((Optimistic2+Pessimistic2)/2)) %>%
    mutate(direction_outline=ifelse(mean_diff>0,1,ifelse(mean_diff<0,-1,0))) %>%
    mutate(dir2= ifelse(Optimistic1<=Optimistic2 & Pessimistic1>=Pessimistic2, 0,
                        ifelse(Optimistic1>=Optimistic2 & Pessimistic1<=Pessimistic2, 0,1))) %>%
    select(-mean_diff)
  
  #-------------------------
  # 3.4. Compute separation
  #-------------------------
  df<- df %>%
    rowwise() %>%
    mutate(maxlocal= max(Optimistic1,Optimistic2))%>%
    mutate(minlocal= min(Pessimistic1,Pessimistic2))
  df <- df %>%
    mutate(separation=ifelse(maxlocal==minlocal,0,((abs((Optimistic1-Optimistic2)+(Pessimistic1-Pessimistic2))/(maxlocal-minlocal))/2))) %>%
    select (-Optimistic1, -Optimistic2, -Pessimistic1, -Pessimistic2, -maxlocal, -minlocal) %>%
    mutate(direction_fill=direction_outline*abs(dir2)) %>%
    arrange(ifelse(direction_outline<0,separation,-separation)) %>%
    arrange(-direction_fill)
  
}

#=================================================================
# 4. Function to compute the visual / using only the right data
#       Inputs: Policy 1, Policy 2, Type (overall, variable name)
#       Outputs: two columns vector
#=================================================================

# Design all the plots
MCM_robust_plot <- function(result,level, Policy1, Policy2) {
  
  #-------------------------
  # 4.1. Compute separation
  #-------------------------
  # filter for level
  if(level=="stakeholder_group") {
    item <- unique(result$stakeholder_group)

  } else if (level=="Country") {
    item <- unique(result$Country)

  } else{
    item <- "Overall"
    # dir.create(file.path(paste0(dir_output, "Graphs"), "Overall"))
    # setwd(file.path(paste0(dir_output, "Graphs"), "Overall"))
  }
  
  #go through each item within levels
  for (i in 1:length(item)){

    if(level=="stakeholder_group") {
      result2 <- result %>% 
        filter(stakeholder_group==item[i])
    } else if (level=="Country") {
      result2  <- result %>% 
        filter(Country==item[i])
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
    dir.create(file.path(paste0(dir_output, "Graphs"), name))
    setwd(file.path(paste0(dir_output, "Graphs"), name))
    
  Plot <- ggplot()+
      geom_rect(data=result2, mapping=aes(xmin=x1, xmax=x1+1, ymin=0, ymax=1, fill=as.character(direction_outline), alpha=separation) )+
      geom_rect(data=result2, mapping=aes(xmin=x1+0.05, xmax=x1+0.95, ymin=0.05, ymax=0.95, fill="2", alpha=1) )+
      geom_rect(data=result2, mapping=aes(xmin=ifelse(direction_outline==0, x1,x1+0.05), xmax=ifelse(direction_outline==0, x1+1,x1+0.95), ymin=ifelse(direction_outline==0, 0, 0.05), ymax=ifelse(direction_outline==0, 1,0.95), fill=as.character(direction_fill), alpha=ifelse(direction_fill==0, 1,separation)) )+
      scale_fill_manual(values = cols)+
      scale_y_continuous(limits = c(0, 1))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks.y=element_blank(),
          plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="none") 
        
    f_name <-paste(Policy1,"vs", Policy2, level, item[i], sep = "_")
    ggsave(plot = Plot, filename = paste(f_name,"png", sep="."), units = "in", dpi=150, height=1, width=10, limitsize = FALSE)
    print("done")
    }
}

df<-df_finalscore

# Create the graphs folder
dir.create(file.path(dir_output, "Data"))
dir.create(file.path(dir_output, "Graphs"))

# save all the charts
for (j in 1:length(Policies)) {

  for (k in 1:length(Policies)) {
    if (Policies[j] != Policies[k]){
      print("k")
      print(k)
      result <- MCM_Distrib(df_finalscore, Policies[j], Policies[k])
      f_name <-paste(Policies[j],"vs", Policies[k], sep = "_")

      setwd(file.path(dir_output, "Data"))
      write.csv(result, file=paste(f_name,".csv"))
      print("level1")
      for (m in 1:length(levels)) {
        print("level2")
        plot<-MCM_robust_plot(result,levels[m], Policies[j], Policies[k])
      }
   }
    
  }
  
}

# make a dataframe with all pairs of policies and then compute the MERIT Scores
for (j in 1:length(Policies)) {
  for (k in 1:length(Policies)) {
    if (Policies[j] != Policies[k]){
      print("k")
      print(k)
      
      if (j==1 & k==2) {
        Merit_df <- MCM_Distrib(df_finalscore, Policies[j], Policies[k])
        Merit_df$Policy1 <- Policies[j]
        Merit_df$Policy2 <- Policies[k]
      }
      else {
        temp <- MCM_Distrib(df_finalscore, Policies[j], Policies[k])
        temp$Policy1 <- Policies[j]
        temp$Policy2 <- Policies[k]
        Merit_df <-rbind(Merit_df, temp)
      }

    }
    
  }
  
}
# compute the Merit scores
dir.create(file.path(dir_output, "Metrics"))

Merit_df2 <- Merit_df %>%
  select("Policy1", "Policy2", "direction_outline", "direction_fill", "separation")%>%
#  select("Policy1", "Policy2", "stakeholder_group", "Country", "direction_outline", "direction_fill", "separation")%>%
  group_by(Policy1, add=TRUE)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))

f_name <- paste0("Merit_", "Overall")
setwd(file.path(dir_output, "Metrics"))
write.csv(Merit_df2, file=paste0(f_name,".csv"))

Merit_df2 <- read.csv("Merit_Overall.csv")

# Create related plot
# plot_Merit_df2  <- Merit_df2  %>%
#   ggplot(aes(x = "Overall", y = Merit_withgrey)) + 
#   scale_fill_viridis(discrete= TRUE)+
#   geom_label(aes(label= Policy1, fill = factor(Policy1)), colour="white", show.legend = F, fontface = "bold") +
#   theme_minimal(base_size = 15) +
#   labs(y = "Merit order")


# You can choose Merit with No Gry or Merit with Grey
plot_Merit_df2  <- Merit_df2  %>%
  ggplot(aes(x = "Overall", y = Merit_withgrey)) + 
scale_fill_viridis(discrete= TRUE)+
  scale_colour_viridis_d()+
  geom_point(aes(color=factor(Policy1)), size=3)+
  geom_text_repel(aes(label= Policy1), 
                  colour="Black", show.legend = F,
                  direction = "y",
                  nudge_x = -0.25,
                  min.segment.length = 0,
                  point.padding = 0.25,
                  hjust        = 0.5,
                  segment.size = 0.2,
                  segment.color = 'Black') +
  theme_minimal(base_size = 15) +
  theme(legend.position="none",     
        axis.title.x = element_blank())+
  labs(y = "Merit order")

setwd(dir_output)
f_name <- paste0("Merit_Overall")
ggsave(plot = plot_Merit_df2, filename = paste(f_name,"png", sep="."), width = 4, height = 7)

######################################################################
############               !!! STOP HERE !!!                ##########
############ Unless you have countries / stakeholder groups ##########
######################################################################

# Country
Merit_df_country <- Merit_df %>%
  select("Policy1", "Policy2", "stakeholder_group", "Country", "direction_outline", "direction_fill", "separation")%>%
  group_by(Country, Policy1)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))%>%
  arrange(Country) %>%
  ungroup()

f_name <- paste0("Merit_", "Country")
setwd(file.path(dir_output, "Metrics"))
write.csv(Merit_df_country, file=paste0(f_name,".csv"))

Merit_df_country <- read.csv("Merit_Country .csv")

# Create related plot with Grey
plot_Merit_Country <- Merit_df_country %>%
  ggplot(aes(x = Country, y = Merit_withgrey)) + 
  scale_fill_viridis(discrete= TRUE)+
  scale_colour_viridis_d()+
  geom_point(aes(color=factor(Policy1)), size=3)+
  geom_text_repel(aes(label= Policy1), 
                  colour="Black", show.legend = F,
                  direction = "y",
                  nudge_x = -0.25,
                  min.segment.length = 0,
                  point.padding = 0.25,
                  hjust        = 0.5,
                  segment.size = 0.2,
                  segment.color = 'Black') +
  theme_minimal(base_size = 15) +
  theme(legend.position="none",     
        axis.title.x = element_blank())+
  labs(y = "Merit order")

# plot_Merit_Country <- Merit_df_country %>%
#   ggplot(aes(x = Country, y = Merit_withgrey)) + 
#   scale_fill_viridis(discrete= TRUE)+
#   scale_colour_viridis_d()+
#   geom_point(aes(color=factor(Policy1)), size=3)+
#   geom_label_repel(aes(label= Policy1,
#                        fill = factor(Policy1)), 
#                    colour="white", show.legend = F,
#                    nudge_x = -0.25,
#                    fontface = "bold",
#                    direction = "y",
#                    hjust        = 0.5,
#                    segment.size = 0.12,
#                    segment.color = 'grey80') +
#   theme_minimal(base_size = 15) +
#   theme(legend.position="none",     
#         axis.title.x = element_blank())+
#   labs(y = "Merit order")

setwd(dir_output)
f_name <- paste0("Country_Merit")
ggsave(plot = plot_Merit_Country, filename = paste(f_name,"png", sep="."), width = 12, height = 8)




# Stakeholder
Merit_df_stak <- Merit_df %>%
  select("Policy1", "Policy2", "stakeholder_group", "Country", "direction_outline", "direction_fill", "separation")%>%
  group_by(stakeholder_group, Policy1)%>%
  summarise(Merit_nogrey = sum(direction_fill*separation), Merit_withgrey = sum(direction_outline*separation))%>%
  arrange(desc(Merit_nogrey))%>%
  arrange(stakeholder_group)

f_name <- paste0("Merit_", "stakeH")
setwd(file.path(dir_output, "Metrics"))
write.csv(Merit_df_stak, file=paste0(Merit_df_stak,".csv"))

Merit_df_stak <- read.csv("Merit_stakeholder_group2.csv")

# Create related plot with Grey
# plot_Merit_stak <- Merit_df_stak %>%
#   ggplot(aes(x = fct_reorder(stakeholder_group, Merit_withgrey), y = Merit_withgrey)) + 
#   scale_fill_viridis(discrete= TRUE)+
#   scale_colour_viridis_d()+
#   geom_point(aes(color=factor(Policy1)), size=3)+
#   geom_label_repel(aes(label= Policy1,
#                        fill = factor(Policy1)), 
#                    colour="white", show.legend = F,
#                    nudge_x = -0.25,
#                    fontface = "bold",
#                    direction = "y",
#                    hjust        = 0.5,
#                    segment.size = 0.12,
#                    segment.color = 'grey80') +
#   theme_minimal(base_size = 15) +
#   theme(legend.position="none",     
#         axis.title.x = element_blank())+
#   labs(y = "Merit order")

plot_Merit_stak <- Merit_df_stak %>%
  ggplot(aes(x = fct_reorder(stakeholder_group, Merit_withgrey), y = Merit_withgrey)) + 
  scale_fill_viridis(discrete= TRUE)+
    scale_colour_viridis_d()+
    geom_point(aes(color=factor(Policy1)), size=3)+
    geom_text_repel(aes(label= Policy1), 
                    colour="Black", show.legend = F,
                    direction = "y",
                    nudge_x = -0.25,
                    min.segment.length = 0,
                    point.padding = 0.25,
                    hjust        = 0.5,
                    segment.size = 0.2,
                    segment.color = 'Black') +
    theme_minimal(base_size = 15) +
    theme(legend.position="none",     
          axis.title.x = element_blank())+
    labs(y = "Merit order")

setwd(dir_output)
f_name <- paste0("Stakeholder_Merit")
ggsave(plot = plot_Merit_stak, filename = paste(f_name,"2","png", sep="."), width = 13, height = 8)


