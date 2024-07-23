################################################################################
# Author: Ana Paula Lula Costa
# July 7th 2024
# Analysis for the paper: Challenges Faced in Ecological Sciences Careers: A Case Study of Gender Perceptions in Brazil 

#loading the packages
library(tidyverse)# Para manipulação e visualização dos dados
library(ggplot2)#
library(dplyr)#
library(viridis)# Para cores
library(scales)#
library(ggthemes)# Para themes
library(ggrepel)
library(cowplot)
library(ggpubr)
library(DescTools)


# Load data
Join_Count_Q <- read.csv("./Results/Join_Count_Q.csv")  
Join_Sum_Q <- read.csv("./Results/Join_Sum_Q.csv")
Q_45_55_unite2 <- read.csv("./Results/Q_45_55_unite2.csv")

# Organize data - remove non representative variables and with less than 5 answers
Join_Count_Qf <- Join_Count_Q %>% 
  filter(!Category %in% c("Didnt   answer",  "Didnt \n answer"))

Join_Sum_Qf <- Join_Sum_Q %>% 
filter(!Category %in% c("Retired", "Didnt answer", "End of temporary contract", "Fired from my last job", "Less focus on research", "Not applicable", "Work security", "Children didnt move", "Children moved but suffered   significant adverse effects", "Partner or significant other hasnt moved in with me,   but with significant negative effects on the relationship"))

########## Analyse "Q33" "Q34" "Q39" "Q41" "Q57" "Q58" "Q60" "Q61" #############
# by gender - count
questions <- unique(Join_Count_Qf$Question)
combined_c_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Join_Count_Qf %>%
    filter(Question == q) %>% 
  group_by(Group_gender, Category) %>%
    summarize(Count = sum(count)) %>%
    spread(key = Category, value = Count, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(Group_gender)))
  
  rownames(result_matrix) <- reshaped_df$Group_gender

  #make chi-test
  Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
  Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
  p_value20F <- Xsq_fisher$p.value 
  p_value20C <- Xsq$p.value
  chi_square <- Xsq$statistic
  
  #Make dataframe
  rs <- rowSums(result_matrix)
  
  df <- result_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "Group_gender") %>%
    gather(key = "Category", value = "Frequency", -Group_gender) %>% 
    mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
    Group_gender == "Men" ~ Frequency/rs["Men"]),
    p_value_F = p_value20F,
    chi_stat = chi_square,
    p_valueChi=p_value20C,
    Question = q)
    
  combined_c_gender <- rbind(combined_c_gender, df)                      
    
}


write.csv(combined_c_gender, "./Results/combined_c_gender.csv")

#by status - count

Status<-unique(Join_Count_Qf$Group_time2)

questions <- unique(Join_Count_Qf$Question)

combined_c_status <- data.frame(NULL)

for (q in questions) {
for (i in Status) {
  
  reshaped_df <- Join_Count_Qf %>% 
  filter(Question == q) %>% 
  filter(Group_time2 == i) %>% 
  group_by(Group_gender, Category) %>%
  summarize(Count = sum(count)) %>%
  spread(key = Category, value = Count, fill = 0) %>% 
  ungroup()
  
  result_matrix <- as.matrix(reshaped_df %>%
                         select(-c(Group_gender)))
  
  rownames(result_matrix) <- reshaped_df$Group_gender
  
  rs <- rowSums(result_matrix)
  
  Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
  Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
  
  p_valueswF <- Xsq_fisher$p.value
  p_valueswC <- Xsq$p.value
  chi_square <- Xsq$statistic
  
  df <- result_matrix %>%
  as.data.frame() %>% 
  rownames_to_column(var = "Group_gender") %>% 
  gather(key = "Category",value = "Frequency", -Group_gender) %>% 
  mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
                                Group_gender == "Men" ~ Frequency/rs["Men"]),
         p_value_F = p_valueswF,
         chi_stat = chi_square,
         p_valueChi= p_valueswC,
         Group_time2 = i,
         Question = q)
  
  combined_c_status <- rbind(combined_c_status, df)
}
  
}

write.csv(combined_c_status, "./Results/combined_c_status.csv")
################ Analyse  "Q19" "Q20" "Q36" "Q38" "Q40" "Q44" ##################
# by gender - sum
questions <- unique(Join_Sum_Qf$Question)
combined_s_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Join_Sum_Qf %>%
    filter(Question == q) %>% 
    group_by(Group_gender, Category) %>%
    summarize(Sum = sum(Sum)) %>%
    spread(key = Category, value = Sum, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(Group_gender)))
  
  rownames(result_matrix) <- reshaped_df$Group_gender
  
  #make chi-test
  Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
  Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
  p_value20F <- Xsq_fisher$p.value 
  p_value20C <- Xsq$p.value
  chi_square <- Xsq$statistic
  
  #Make dataframe
  rs <- rowSums(result_matrix)
  
  df <- result_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "Group_gender") %>%
    gather(key = "Category", value = "Frequency", -Group_gender) %>% 
    mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
                                  Group_gender == "Men" ~ Frequency/rs["Men"]),
           p_value_F = p_value20F,
           chi_stat = chi_square,
           p_valueChi=p_value20C,
           Question = q)
  
  combined_s_gender <- rbind(combined_s_gender, df)                      
  
}


write.csv(combined_s_gender, "./Results/combined_s_gender.csv")

# by status - sum

Status<-unique(Join_Sum_Qf$Group_time2)

questions <- unique(Join_Sum_Qf$Question)

combined_s_status <- data.frame(NULL)

for (q in questions) {
  for (i in Status) {
    
    reshaped_df <- Join_Sum_Qf %>% 
      filter(Question == q) %>% 
      filter(Group_time2 == i) %>% 
      group_by(Group_gender, Category) %>%
      summarize(Sum = sum(Sum)) %>%
      spread(key = Category, value = Sum, fill = 0) %>% 
      ungroup()
    
    result_matrix <- as.matrix(reshaped_df %>%
                                 select(-c(Group_gender)))
    
    rownames(result_matrix) <- reshaped_df$Group_gender
    
    rs <- rowSums(result_matrix)
    
    Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
    Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
    
    p_valueswF <- Xsq_fisher$p.value
    p_valueswC <- Xsq$p.value
    chi_square <- Xsq$statistic
    
    
    df <- result_matrix %>%
      as.data.frame() %>% 
      rownames_to_column(var = "Group_gender") %>% 
      gather(key = "Category",value = "Frequency", -Group_gender) %>% 
      mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
                                                 Group_gender == "Men" ~ Frequency/rs["Men"]),
             p_value_F = p_valueswF,
             chi_stat = chi_square,
             p_valueChi= p_valueswC,
             Group_time2 = i,
             Question = q)
    
    combined_s_status <- rbind(combined_s_status, df)
  }
  
}

write.csv(combined_s_status, "./Results/combined_s_status.csv")

############################ Analyse  Q45-55 ###################################
# by gender - Q45-55 
questions <- unique(Q_45_55_unite2$Influence)
combined_Influence_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Q_45_55_unite2 %>%
    filter(Influence == q) %>% 
    group_by(Group_gender, Type) %>%
    summarize(Count = sum(Count)) %>%
    spread(key = Type, value = Count, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(Group_gender)))
  
  rownames(result_matrix) <- reshaped_df$Group_gender
  
  #make chi-test
  Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
  Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
  p_value20F <- Xsq_fisher$p.value 
  p_value20C <- Xsq$p.value
  chi_square <- Xsq$statistic
  
  #Make dataframe
  rs <- rowSums(result_matrix)
  
  df <- result_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "Group_gender") %>%
    gather(key = "Type", value = "Frequency", -Group_gender) %>% 
    mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
                                  Group_gender == "Men" ~ Frequency/rs["Men"]),
           p_value_F = p_value20F,
           chi_stat = chi_square,
           p_valueChi=p_value20C,
           Influence = q)
  
  combined_Influence_gender <- rbind(combined_Influence_gender, df)                      
  
}


write.csv(combined_Influence_gender, "./Results/combined_Influence_gender.csv")

# by status - Q45-55

Status<-unique(Q_45_55_unite2$Group_time2)

questions <- unique(Q_45_55_unite2$Influence)

combined_Influence_status <- data.frame(NULL)

for (q in questions) {
  for (i in Status) {
    
    reshaped_df <- Q_45_55_unite2 %>% 
      filter(Influence == q) %>% 
      filter(Group_time2 == i) %>% 
      group_by(Group_gender, Type) %>%
      summarize(Count = sum(Count)) %>%
      spread(key = Type, value = Count, fill = 0) %>% 
      ungroup()
    
    result_matrix <- as.matrix(reshaped_df %>%
                                 select(-c(Group_gender)))
    
    rownames(result_matrix) <- reshaped_df$Group_gender
    
    rs <- rowSums(result_matrix)
    
    Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
    Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
    
    p_valueswF <- Xsq_fisher$p.value
    p_valueswC <- Xsq$p.value
    chi_square <- Xsq$statistic
    
    
    df <- result_matrix %>%
      as.data.frame() %>% 
      rownames_to_column(var = "Group_gender") %>% 
      gather(key = "Type",value = "Frequency", -Group_gender) %>% 
      mutate(Proportion = case_when(Group_gender == "Women" ~ Frequency/rs["Women"],
                                    Group_gender == "Men" ~ Frequency/rs["Men"]),
             p_value_F = p_valueswF,
             chi_stat = chi_square,
             p_valueChi= p_valueswC,
             Group_time2 = i,
             Influence = q)
    
    combined_Influence_status <- rbind(combined_Influence_status, df)
  }
  
}

write.csv(combined_Influence_status, "./Results/combined_Influence_status.csv")

# Make plot by gender 
combined_Influence_gender$Influence <- factor(combined_Influence_gender$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

gender_g5<- combined_Influence_gender %>% 
  ggplot(aes(y=Type, x=Frequency, fill= Influence)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9) +
  theme_classic() +
  scale_fill_manual(values = c("Big Accelerator" = "blue4","Accelerator"= "cornflowerblue","Neutral/No impact" = "cornsilk2", "Impediment" = "coral", "Big impediment" ="brown3")) +
  facet_wrap(vars(Group_gender), scales = "free_x")+
  ylab("Factors - Women") + 
  xlab("") +
  guides(fill = guide_legend(title="Influence",
                             
                             override.aes = aes(label = "", size=3)))+
  theme(panel.background = element_rect(fill="transparent"), #transparent panel bg
        plot.background = element_rect(fill="transparent", color=NA),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        legend.background =element_rect(fill="transparent"),
        legend.position = "bottom")
gender_g5

# Make plot by status
unique(combined_Influence_status$Influence)
combined_Influence_status$Influence <- factor(combined_Influence_status$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

status_g5<- combined_Influence_status %>% 
  filter(Group_time2 == "Senior") %>% 
  ggplot(aes(y=Type, x=Frequency, fill= Influence)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9) +
  theme_classic() +
  scale_fill_manual(values = c("Big Accelerator" = "blue4","Accelerator"= "cornflowerblue","Neutral/No impact" = "cornsilk2", "Impediment" = "coral", "Big impediment" ="brown3")) +
  facet_wrap(vars(Group_gender), scales = "free_x")+
  ylab("Factors - Women") + 
  xlab("") +
  guides(fill = guide_legend(title="Influence",
                             
                             override.aes = aes(label = "", size=3)))+
  theme(panel.background = element_rect(fill="transparent"), #transparent panel bg
        plot.background = element_rect(fill="transparent", color=NA),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        legend.background =element_rect(fill="transparent"),
        legend.position = "bottom")
status_g5

################# Data visualization by gender ##########################

#Chi-square values - sum questions
Q<-combined_s_gender %>% 
  filter(Question == 'Q63')

unique(Q$chi_stat)
unique(Q$p_valueChi)

# Plot the data sum
P <- combined_s_gender %>% 
  filter(Question == 'Q44') %>%
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = Group_gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Group_gender, scales = "free") +
  scale_fill_manual(values=c(Women = "#9800B7", Men = "#3C9B1C"))+
  labs(title = "Team work preference",
       x = " ",
       y = "Proportion") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.text.x = element_text(family = "serif", size = 12),
        axis.title.x = element_text(family = "serif", size = 12),
        title = element_text(family = "serif", size = 14, face = "bold"))+
  geom_text(aes(label = paste0(round(100 * Proportion, 1), "%")), size = 3,
            position = position_stack(vjust = 0.5),
            inherit.aes = TRUE)+
  coord_flip()
P

#Chi-square values - count questions
Q<-combined_c_gender %>% 
  filter(Question == 'Q61')
unique(Q$chi_stat)
unique(Q$p_valueChi)

# Plot data - count
P <- combined_c_gender %>% 
  filter(Question == 'Q61') %>%
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = Group_gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Group_gender, scales = "free") +
  scale_fill_manual(values=c(Women = "#9800B7", Men = "#3C9B1C"))+
  labs(title = "Team work preference",
       x = " ",
       y = "Proportion") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.text.x = element_text(family = "serif", size = 12),
        axis.title.x = element_text(family = "serif", size = 12),
        title = element_text(family = "serif", size = 14, face = "bold"))+
  geom_text(aes(label = paste0(round(100 * Proportion, 1), "%")), size = 3,
            position = position_stack(vjust = 0.5),
            inherit.aes = TRUE)+
  coord_flip()
P

################# Data visualization by status ##########################
# Chi-square values - junior

Q<-combined_c_status %>% 
  filter(Group_time2 =="Junior") %>% 
  filter(Question == 'Q61')
unique(Q$chi_stat)
unique(Q$p_valueChi)

# Chi-square values - senior
Q<-combined_c_status %>% 
  filter(Group_time2 =="Senior") %>% 
  filter(Question == 'Q61')
unique(Q$chi_stat)
unique(Q$p_valueChi)

# Plot data

M <- combined_c_status %>% 
  filter(Question == "Q57") %>% 
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = Group_gender)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Group_time2, scales = "free") +
  labs(title = "",
       x = " ",
       y = "Proportion") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.text.x = element_text(family = "serif", size = 12),
        axis.title.x = element_text(family = "serif", size = 12),
        title = element_text(family = "serif", size = 14, face = "bold"))+
  geom_text(aes(label = paste0(round(100 * Proportion, 1), "%")), size = 3,
            position = position_stack(vjust = 0.5),
            inherit.aes = TRUE)+
  coord_flip()
M
