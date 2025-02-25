################################################################################
# Author: Ana Paula Lula Costa
# July 7th 2024
# Analysis for the paper: Challenges Faced in Ecological Sciences Careers: A Case Study of Gender Perceptions in Brazil 

# Load packages ----
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

# Load data ----
Join_Count_Q <- read.csv("./Data/Join_Count_Q.csv")  
Join_Sum_Q <- read.csv("./Data/Join_Sum_Q.csv")
Q_38_Influence <- read.csv("./Data/Q_38_Influence.csv")
data_sum_gender <- read.csv("./Data/data_sum_gender.csv")
data_sum_status <- read.csv("./Data/ data_sum_status.csv")

# Organize data ----
#remove non representative variables and with less than 5 answers

Join_Sum_Qf <- Join_Sum_Q %>% 
filter(!Category %in% c("Retired", "Didnt answer", "End of temporary contract", "Fired from my last job", "Less focus on research", "Not applicable", "Work security", "Children didnt move", "Children moved but suffered   significant adverse effects", "Partner or significant other hasnt moved in with me,   but with significant negative effects on the relationship")) %>% 
  ungroup()

# Analyse "Q26" "Q27" "Q32" "Q34" "Q39" "Q40" "Q42" "Q43" ----
### By gender - count ----
questions <- unique(Join_Count_Q$Question)
combined_c_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Join_Count_Q %>%
    filter(Question == q) %>% 
  group_by(GENDER_CAT , Category) %>%
    summarize(Count = sum(count)) %>%
    spread(key = Category, value = Count, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(GENDER_CAT )))
  
  rownames(result_matrix) <- reshaped_df$GENDER_CAT 

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
    rownames_to_column(var = "GENDER_CAT") %>%
    gather(key = "Category", value = "Frequency", -GENDER_CAT ) %>% 
    mutate(Proportion = case_when(GENDER_CAT  == "Women" ~ Frequency/rs["Women"],
    GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
    p_value_F = p_value20F,
    chi_stat = chi_square,
    p_valueChi=p_value20C,
    Question = q)
    
  combined_c_gender <- rbind(combined_c_gender, df)                      
    
}


write.csv(combined_c_gender, "./Results/combined_c_gender.csv")

### By status - count ----

Status<-unique(Join_Count_Q$C_STAGE_CAT)

questions <- unique(Join_Count_Q$Question)

combined_c_status <- data.frame(NULL)

for (q in questions) {
for (i in Status) {
  
  reshaped_df <- Join_Count_Q %>% 
  filter(Question == q) %>% 
  filter(C_STAGE_CAT == i) %>% 
  group_by(GENDER_CAT , Category) %>%
  summarize(Count = sum(count)) %>%
  spread(key = Category, value = Count, fill = 0) %>% 
  ungroup()
  
  result_matrix <- as.matrix(reshaped_df %>%
                         select(-c(GENDER_CAT )))
  
  rownames(result_matrix) <- reshaped_df$GENDER_CAT 
  
  rs <- rowSums(result_matrix)
  
  Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
  Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
  
  p_valueswF <- Xsq_fisher$p.value
  p_valueswC <- Xsq$p.value
  chi_square <- Xsq$statistic
  
  df <- result_matrix %>%
  as.data.frame() %>% 
  rownames_to_column(var = "GENDER_CAT") %>% 
  gather(key = "Category",value = "Frequency", -GENDER_CAT ) %>% 
  mutate(Proportion = case_when(GENDER_CAT  == "Women" ~ Frequency/rs["Women"],
                                GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
         p_value_F = p_valueswF,
         chi_stat = chi_square,
         p_valueChi= p_valueswC,
         C_STAGE_CAT = i,
         Question = q)
  
  combined_c_status <- rbind(combined_c_status, df)
}
  
}

write.csv(combined_c_status, "./Results/combined_c_status.csv")

# Analyse  "Q14" "Q15" "Q29" "Q31" "Q33" "Q37" ----
#"Q14" "Q15" "Q16" "Q17" "Q19" "Q28" "Q29" "Q31" "Q33" "Q36" "Q37" "Q44"

### By gender - sum ----
questions <- c("Q14", "Q15","Q28", "Q29", "Q31", "Q33", "Q37")
combined_s_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Join_Sum_Qf %>%
    filter(Question == q) %>% 
    group_by(GENDER_CAT, Category) %>%
    summarize(Sum = sum(Sum)) %>%
    spread(key = Category, value = Sum, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(GENDER_CAT)))
  
  rownames(result_matrix) <- reshaped_df$GENDER_CAT 
  
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
    rownames_to_column(var = "GENDER_CAT") %>%
    gather(key = "Category", value = "Frequency", -GENDER_CAT ) %>% 
    mutate(Proportion = case_when(GENDER_CAT  == "Women" ~ Frequency/rs["Women"],
                                  GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
           p_value_F = p_value20F,
           chi_stat = chi_square,
           p_valueChi=p_value20C,
           Question = q)
  
  combined_s_gender <- rbind(combined_s_gender, df)                      
  
}


write.csv(combined_s_gender, "./Results/combined_s_gender.csv")

### By status - sum ----

Status<-unique(Join_Sum_Qf$C_STAGE_CAT)

questions <- c("Q14", "Q15","Q28", "Q29", "Q31", "Q33", "Q37")

combined_s_status <- data.frame(NULL)

for (q in questions) {
  for (i in Status) {
    
    reshaped_df <- Join_Sum_Qf %>% 
      filter(Question == q) %>% 
      filter(C_STAGE_CAT == i) %>% 
      group_by(GENDER_CAT , Category) %>%
      summarize(Sum = sum(Sum)) %>%
      spread(key = Category, value = Sum, fill = 0) %>% 
      ungroup()
    
    result_matrix <- as.matrix(reshaped_df %>%
                                 select(-c(GENDER_CAT )))
    
    rownames(result_matrix) <- reshaped_df$GENDER_CAT 
    
    #Make dataframe
    rs <- rowSums(result_matrix)
    
    Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
    Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
    
    p_valueswF <- Xsq_fisher$p.value
    p_valueswC <- Xsq$p.value
    chi_square <- Xsq$statistic
    
    
    df <- result_matrix %>%
      as.data.frame() %>% 
      rownames_to_column(var = "GENDER_CAT") %>% 
      gather(key = "Category",value = "Frequency", -GENDER_CAT ) %>% 
      mutate(Proportion = case_when(GENDER_CAT  == "Women" ~ Frequency/rs["Women"],
                                                 GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
             p_value_F = p_valueswF,
             chi_stat = chi_square,
             p_valueChi= p_valueswC,
             C_STAGE_CAT = i,
             Question = q)
    
    combined_s_status <- rbind(combined_s_status, df)
  }
  
}

write.csv(combined_s_status, "./Results/combined_s_status.csv")

# Analyse  Q38 ----
### By gender - Q38 ---- 

questions <- unique(Q_38_Influence$Influence)

combined_Influence_gender <- data.frame(NULL)

for (q in questions) {
  
  reshaped_df <- Q_38_Influence %>%
    filter(Influence == q) %>% 
    group_by(GENDER_CAT , Type) %>%
    summarize(Count = sum(Count)) %>%
    #mutate(Proportion = case_when(GENDER_CAT  == "Women" ~ (Count/215)*100,
     #                             GENDER_CAT  == "Men" ~ (Count/68)*100)) %>% 
    select(GENDER_CAT , Type, Count) %>% 
    spread(key = Type, value = Count, fill = 0) %>% 
    ungroup()
  
  # Convert to matrix 
  result_matrix <- as.matrix(reshaped_df %>%
                               select(-c(GENDER_CAT )))
  
  rownames(result_matrix) <- reshaped_df$GENDER_CAT 
  
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
    rownames_to_column(var = "GENDER_CAT") %>%
    gather(key = "Type", value = "Frequency", -GENDER_CAT ) %>% 
    mutate(Percentage = case_when(
      GENDER_CAT  == "Women" ~ Frequency/rs["Women"], 
      GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
      p_value_F = p_value20F,
           chi_stat = chi_square,
           p_valueChi=p_value20C,
      Influence = q)
  
  combined_Influence_gender <- rbind(combined_Influence_gender, df)                      
  
}

write.csv(combined_Influence_gender, "./Results/combined_Type_gender.csv")

### By status - Q38 ----

Status<-unique(Q_38_Influence$C_STAGE_CAT)

questions <- unique(Q_38_Influence$Influence)

combined_Influence_status <- data.frame(NULL)

for (q in questions) {
  for (i in Status) {
    
    reshaped_df <- Q_38_Influence %>% 
      filter(Influence == q) %>% 
      filter(C_STAGE_CAT == i) %>% 
      group_by(GENDER_CAT , Type) %>%
      summarize(Count = sum(Count)) %>%
      # mutate(Proportion = case_when(
      #   GENDER_CAT  == "Women" ~ (Count/215)*100,
      #   GENDER_CAT  == "Men" ~ (Count/68)*100)) %>% 
      select(GENDER_CAT , Type, Count) %>%
      spread(key = Type, value = Count, fill = 0) %>% 
      ungroup()
    
    result_matrix <- as.matrix(reshaped_df %>%
                                 select(-c(GENDER_CAT )))
    
    rownames(result_matrix) <- reshaped_df$GENDER_CAT 
    
    rs <- rowSums(result_matrix)
    
    Xsq <- chisq.test(result_matrix, simulate.p.value = TRUE)
    Xsq_fisher <- fisher.test(result_matrix, simulate.p.value = TRUE)
    
    p_valueswF <- Xsq_fisher$p.value
    p_valueswC <- Xsq$p.value
    chi_square <- Xsq$statistic
    
    df <- result_matrix %>%
      as.data.frame() %>% 
      rownames_to_column(var = "GENDER_CAT") %>% 
      gather(key = "Type",value = "Frequency", -GENDER_CAT ) %>% 
      mutate(Percentage = case_when(
        GENDER_CAT  == "Women" ~ Frequency/rs["Women"], 
        GENDER_CAT  == "Men" ~ Frequency/rs["Men"]),
        p_value_F = p_valueswF,
             chi_stat = chi_square,
             p_valueChi= p_valueswC,
             C_STAGE_CAT = i,
             Influence = q)
    
    combined_Influence_status <- rbind(combined_Influence_status, df)
  }
  
}

write.csv(combined_Influence_status, "./Results/combined_Type_status.csv")

### Plot results Q38 ----
#### Make plot by gender ----

combined_Influence_gender$Influence <- factor(combined_Influence_gender$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

gender_g5<- combined_Influence_gender %>% 
  ggplot(aes(y=Type, x=Frequency, fill= Influence)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9) +
  theme_classic() +
  scale_fill_manual(values = c("Big Accelerator" = "blue4","Accelerator"= "cornflowerblue","Neutral/No impact" = "cornsilk2", "Impediment" = "coral", "Big impediment" ="brown3")) +
  facet_wrap(vars(GENDER_CAT ), scales = "free_x")+
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

#### Make plot by status ----

unique(combined_Influence_status$Influence)

combined_Influence_status$Influence <- factor(combined_Influence_status$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

status_g5<- combined_Influence_status %>% 
  filter(C_STAGE_CAT == "Senior") %>% 
  ggplot(aes(y=Type, x=Frequency, fill= Influence)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.9) +
  theme_classic() +
  scale_fill_manual(values = c("Big Accelerator" = "blue4","Accelerator"= "cornflowerblue","Neutral/No impact" = "cornsilk2", "Impediment" = "coral", "Big impediment" ="brown3")) +
  facet_wrap(vars(GENDER_CAT ), scales = "free_x")+
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

# Data visualization by gender ----

## Chi-square values - sum questions ----

Q<-combined_s_gender %>% 
  filter(Question == 'Q14')

unique(Q$chi_stat)
unique(Q$p_valueChi)
str(combined_s_gender)

### Plot the data sum ----

P <- combined_s_gender %>% 
  filter(Question == 'Q14') %>%
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = GENDER_CAT )) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ GENDER_CAT , scales = "free") +
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


## Chi-square values - count questions ----

Q<-combined_c_gender %>% 
  filter(Question == 'Q60')
unique(Q$chi_stat)
unique(Q$p_valueChi)

### Plot data - count ----

P <- combined_c_gender %>% 
  filter(Question == 'Q61') %>%
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = GENDER_CAT )) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ GENDER_CAT , scales = "free") +
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

# Data visualization by status ----
## Chi-square values - junior ----

Q<-combined_c_status %>% 
  filter(C_STAGE_CAT =="Junior") %>% 
  filter(Question == 'Q57')
unique(Q$chi_stat)
unique(Q$p_valueChi)

## Chi-square values - senior ----
Q<-combined_c_status %>% 
  filter(C_STAGE_CAT =="Junior") %>% 
  filter(Question == 'Q57')
unique(Q$chi_stat)
unique(Q$p_valueChi)

### Plot data ----

M <- combined_s_status %>% 
  filter(Question == "Q14") %>% 
  filter(C_STAGE_CAT == "Junior") %>% 
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = GENDER_CAT )) +
  geom_bar(stat = "identity", position = "stack") +
  #facet_wrap(~ C_STAGE_CAT, scales = "free") +
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

M <- combined_c_status %>% 
  filter(Question == "Q57") %>% 
  ggplot(aes(x = reorder(Category, Proportion), y = Proportion, fill = GENDER_CAT )) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ C_STAGE_CAT, scales = "free") +
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

