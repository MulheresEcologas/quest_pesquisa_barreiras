#
#Department of Ecology and Evolutionary Biology
#University of California, Los Angeles
#Elvira D'Bastiani
#

#clean the desktop
rm(list=ls())

#Garbage Collection
gc()

# files in directory
dir()

###########
#exemplo analysis ANACOR 
############Script to Simple correspondence analysis (ANACOR)################
# Load the ca package
library(ca)
library(FactoMineR)
library(tidyverse)
library(factoextra)
library(ggplot2)
library(ggrepel)

# Read the entire sheet into a data frame
data_geral_af <- read.csv2("./Data/data_geral_af.csv", sep=",")

#Question Q39_GP: What is your scientific productivity?
Q39_GP <- data_geral_af %>% select(starts_with("Q57"))

#Question Q17_DC: Who does the weekly housework
Q17_DC <- data_geral_af %>% select(starts_with("Question_22"))

#Question Q16_DC: How/with whom do you live?
Q16_DC <- data_geral_af %>% select(starts_with("Question_21"))

#Question Q4_PS: gender
Q4_PS <- data_geral_af %>% select(starts_with("Group_gender"))

#Question C_STAGE_CAT: Stage of career
C_STAGE_CAT<-data_geral_af %>% select(starts_with("Group_time2"))

#Question Q40_P: Parenthood
Q40_P <- data_geral_af %>% select(starts_with("Q58"))

#Question Q41_P: Amount of children
Q41_P <- data_geral_af %>% select(starts_with("Q59"))

#Question Q22_EI: What is your income?
Q22_EI <- data_geral_af %>% select(starts_with("Q28"))

#Question 38: Moral harassment
Q31_WE <- data_geral_af %>% select(starts_with("Q38"))

#Question Q32_WE: Have you ever been sexually harassed by a coworker?
Q32_WE <- data_geral_af %>% select(starts_with("Question_39"))

#Question Q34_GP: If you were/are in a postgraduate program, the person who supervised you last was:
Q34_GP <- data_geral_af %>% select(starts_with("Q41"))

##########################
#ANACOR 1 # fazer só com mulheres p/ ver se fica mais claro a relação
##########################
# Response Variable: 
  #Moral and sexual harassment situations

#Question 38: Moral harassment
Q31_WE

#Question 39: Have you ever been sexually harassed by a coworker?
Q32_WE

# Explanatory Variables:
  # Supervisor's gender
  # Gender (of employees or involved individuals)
  # Career stages

#Question 41: If you were/are in a postgraduate program, the person who supervised you last was:
Q34_GP

#Question 3: gender # Gender (of employees or involved individuals)
Q4_PS

#Question C_STAGE_CAT: Stage of career
C_STAGE_CAT

data_anacor_1<-data.frame(Q31_WE,
                        Q32_WE,
                        Q34_GP,
                        Q4_PS,
                        C_STAGE_CAT)

str(data_anacor_1)

data_anacor_1 <- data_anacor_1 %>%
  mutate(across(c(
    "Q38_Algum.homem.já.levou.o.crédito.pelo.trabalho.feito.por.você",                                                                  
    "Q38_Você.já.perdeu.alguma.promoção.benefício..bolsa.produtividade..para.algum.homem..mesmo.que.tenha.se.empenhado.igualmente",     
    "Q38_Você.já.foi.vista.como.agressiva.ou.desagradável.por.exercer.sua.autoridade.ou.mostrar.sua.opinião",                           
    "Q38_Você.já.sentiu.que.o.seu.gênero.foi.decisivo.ao.não.ser.chamada.para.algum.trabalho.de.campo",                                 
    "Q38_Você.já.sentiu.que.o.seu.gênero.foi.decisivo.ao.não.conseguir.algum.trabalho.de.liderança",                                    
    "Q38_Você.já.sentiu.que.seu.gênero.foi.decisivo.para.ter.uma.opinião.aceita",                                                       
    "Q38_Você.já.passou.por.situações.constrangedoras.que.envolveram.piadas.relacionadas.ao.seu.gênero..ex..piadas.sobre.estar.de.TPM.",
    "Q38_Você.já.sofreu.alguma.discriminação.por.estar.grávida.ou.por.ser.mulher.e.poder.engravidar",                                   
    "Q38_Não.passei.por.nenhuma.das.situações.descritas.nas.opções.anteriores",                                                         
    "Q38_Não.se.aplica",                                                                                                                
    "Q38_Prefiro.não.responder"
    ), 
    ~ recode(., `1` = "yes", `0` = "no")))

# Replace dots with spaces in all column names
colnames(data_anacor_1) <- gsub("\\.", " ", colnames(data_anacor_1))
ncol(data_anacor_1)

data_anacor_1 <- data_anacor_1[, !colnames(data_anacor_1) %in% 
                                 c("Q38_Não se aplica","Q38_Prefiro não responder")]

colnames(data_anacor_1) <- c(
  "Cred_M",
  "Lost_P",
  "S_Agg",
  "F_Bias",
  "L_Bias",
  "O_Bias",
  "G_Jokes",
  "P_Disc",
  "No_Sit",
  "S_Har",
  "Supervisor",
  "Gender",
  "Stage"
)


# change levels for 'Supervisor'
data_anacor_1$Supervisor <- recode(data_anacor_1$Gender_Identity,
                                        "Women" = "Woman",
                                        "Men" = "Man"
                                   )

# change levels for 'Gender_Identity'
data_anacor_1$Gender_Identity <- recode(data_anacor_1$Gender_Identity,
                                        "Women" = "Woman",
                                        "Men" = "Man")


# Ensure all columns in data_anacor_2 are factors
data_anacor_1[] <- lapply(data_anacor_1, function(x) as.factor(x))

# Perform Multiple Correspondence Analysis (MCA)
str(data_anacor_1)
mca_1_result <- MCA(data_anacor_1, graph = FALSE)

# Summarize the results
summary(mca_1_result)

# Eigenvalues and variance explained
eigenvalues_mca_1<- mca_1_result$eig
eigenvalues_mca_1

# Contributions of categories to the dimensions
mca_1_result$var$contrib

# Color individuals by groups, add concentration ellipses
# Remove labels: label = "none".
grp_1 <- as.factor(data_anacor_1[, "Gender_Identity"])

my_theme <- theme_bw(base_size = 14,
                     base_family = "",
                     base_line_size = 1,
                     base_rect_size = 1) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 13, color = "gray30", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(size = 0.2, color = "gray85"),
    axis.ticks = element_blank()
  )


# Create the MCA plot
p <- fviz_mca_biplot(mca_1_result, 
                     label="var",
                     habillage = grp_1,
                     addEllipses = TRUE, 
                     ellipse.level = 0.99,
                     repel=T,
                     col.var ="black") +
  scale_color_manual(values = c("#9cd6d5", "#feac6b"), 
                     name = "Gender Identity",  # Adding a legend title
                     labels = c("Man", "Woman")) +
  scale_fill_manual(values = c("#9cd6d5", "#feac6b"),
                    name = "Gender Identity",  # Adding a legend title
                    labels = c("Man", "Woman")) +  
  labs(x = "Dimension 1(31.9%)", y = "Dimension 2 (10.3%)", 
       title = "ANACOR 1") +
  my_theme

# Print the plot
print(p)

ggsave("Anacor_1.png", width = 26, height = 22, units = "cm",  dpi = 600)

##########################
#ANACOR 3 # talvez juntar variaveis da anacor 2 com a 3
##########################
# Response Variable: 
  #Productivity

#Question Q39_GP: What is your scientific productivity?
Q39_GP

#Question Q4_PS: gender
Q4_PS
#Question: Stage of career
C_STAGE_CAT
#Question 58: Parenthood
Q40_P

data_anacor_3<-data.frame(Q39_GP,
                          Q4_PS,
                          C_STAGE_CAT,
                          Q40_P
                          )

colnames(data_anacor_3) <- c(
  # "Avg_Annual_Production",
  # "Gender_Identity",
  # "Group_Time",
  # "Has_Children",
  # "Amount_of_Children",
  # "Monthly_income"
  "AAP",
  "GI",
  "GT",
  "HC"
  )

# Ensure all columns in data_anacor_3 are factors
data_anacor_3[] <- lapply(data_anacor_3, function(x) as.factor(x))

# Perform Multiple Correspondence Analysis (MCA)
mca_3_result <- MCA(data_anacor_3, graph = FALSE)

# Summarize the results
summary(mca_3_result)

# Eigenvalues and variance explained
eigenvalues_mca_3 <- mca_3_result$eig
eigenvalues_mca_3 

# Contributions of categories to the dimensions
mca_3_result$var$contrib

# Color individuals by groups, add concentration ellipses
# Remove labels: label = "none".
grp_3 <- as.factor(data_anacor_3[, "GI"])

# Create the MCA plot
p <- fviz_mca_biplot(mca_3_result, 
                     label="var",
                     habillage = grp_3,
                     addEllipses = TRUE, 
                     ellipse.level = 0.79,
                     repel=T,
                      col.var = 'black') +
  # # Customizing the colors for different levels of the grouping variable
  scale_color_manual(values = c("#9cd6d5", "#feac6b"),
                     name = "Gender Identity",  # Adding a legend title
                     labels = c("Man", "Women")) +  # Customize labels if needed
  scale_fill_manual(values = c("#9cd6d5", "#feac6b"),
                    name = "Gender Identity",  # Adding a legend title
                    labels = c("Man", "Women")) +  # Customize labels if needed Customizing axes labels for clarity
   labs(x = "Dimension 1(23.3%)", y = "Dimension 2 (12.5%)", 
        title = "ANACOR 2") +
my_theme

# Print the plot
print(p)

ggsave("Anacor_3.png", width = 26, height = 22, units = "cm",  dpi = 600)


