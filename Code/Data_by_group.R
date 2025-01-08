
#!!!############################################################################
#Steps to clean the data - Elvira precisa descrever todos os passos
#Step 1. remove the 1, 2, 3 column - email information and indication
#Step 2. substitute the space by _ underline for questions
#Step 4. 
#Step 5. 
#Step 6. 
#!!!############################################################################

#loading the packages
library(tidyverse)# Para manipulação e visualização dos dados
library(ggplot2)#
library(dplyr)#
library(viridis)# Para cores
library(scales)#
library(ggthemes)# Para themes
library(ggrepel)

# Importando os dados do arquivo CSV gerado pelo Google Forms
data_geral <- readxl::read_xlsx("./Data/respostas_cleaned_final.xlsx")
data_matrix <- read.csv("C:/Users/anapa/OneDrive/Desktop/Nuvem/Projetos/Mulheres na Ecologia/Análises_Artigo_MEco/data_matrix.csv")
colnames(data_geral)
colnames(data_matrix)

data_matrix = data_matrix %>% 
  dplyr::rename("Carimbo de data/hora"= "Carimbo.de.data.hora.x",
                "Carimbo de data" = "Carimbo.de.data.x")

# Grupo 1: por gênero
#unique(data_geral$`Q3_Qual a sua identidade de gênero?`)

data_geral_i= data_geral %>% 
  mutate(Group_gender = case_when(
      `Q3_Qual a sua identidade de gênero?`== "Homem cisgênero" ~ "Men",
      `Q3_Qual a sua identidade de gênero?` == "Homem Pansexual"~ "Men", 
      `Q3_Qual a sua identidade de gênero?` == 'Mulher cisgênera' ~ "Women",
      .default = "other"))

data_matrix = data_matrix %>% 
  dplyr::rename("Carimbo de data/hora"= "Carimbo.de.data.hora.x",
                "Carimbo de data" = "Carimbo.de.data.x")

#selecione somente pessoas que nasceram e que trabalham no brasil e que ainda estão no ambiente academico

data_geral_a<-data_geral_i %>% 
  filter(`Q7_Qualéoseupaísdeorigem?`== 'Brasil')%>% 
  filter(`Q30_Qual é a melhor descrição da instituição onde você trabalha atualmente?` %in%
           c("Universidade pública", "Universidade Comunitária", "Universidade comunitária", "Universidade privada", "Instituição/Fundação de pesquisa privada", "Instituição de pesquisa pública", "Instituto Federal de Educação, Ciência e Tecnologia"))

# Grupo 2: Por ocupação. Qual o cargo atual das pessoas que estão na academia
#unique(data_geral_a$`Q25_Qual categoria melhor descreve a sua situação atual?`)

data_geral_a= data_geral_a %>% 
  mutate(Group_occupation = case_when(
      `Q25_Qual categoria melhor descreve a sua situação atual?`== "Pesquisador(a) de mestrado/doutorado" ~ "Master_PhD_Candidate",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) associada"  ~ "Professor",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor Colaborador Adjunto (temporário)"~ "Professor", 
      `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) titular"  ~ "Professor",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) adjunta" ~ "Professor",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisador(a) pós-doutorado' ~ "Researcher",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisador(a) em instituição de pesquisa' ~ "Researcher",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisadora voluntária' ~ "Researcher",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Professor visitante' ~ "Professor",
      `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) assistente" ~ "Professor",
      .default = "other"))

# Grupo 3 por tempo de carreira. Aqui usamos duas variaveis, primeiro categorizamos como profissionais seniores aqueles que estão no cargo atual a mais de 6 anos e Juniores os que estão a menos de 6 anos no seu cargo atual
#unique(data_geral_a$`Q26_Há quanto tempo você está na situação acima?`)

data_geral_a= data_geral_a %>% 
  mutate(Group_time = case_when(
      `Q26_Há quanto tempo você está na situação acima?`== "1 a 3 anos" ~ "Junior",
      `Q26_Há quanto tempo você está na situação acima?` == "Menos de 1 ano" ~ "Junior", 
      `Q26_Há quanto tempo você está na situação acima?` == "3 a 6 anos" ~ "Junior",
      `Q26_Há quanto tempo você está na situação acima?` == "6 a 9 anos" ~ "Senior",
      `Q26_Há quanto tempo você está na situação acima?` == "Mais que 10 anos" ~ "Senior",
      `Q26_Há quanto tempo você está na situação acima?` == "Mais que 20 anos" ~ "Senior",
      .default = "other")) %>%  
  mutate(Group_time = case_when(
    Group_occupation == "Master_PhD_Candidate" ~ "Junior",
                            .default = as.character(Group_time)))

# Em um segundo momento colocamos como Seniores aqueles que possuem 40 anos ou mais, além daqueles que já estão a mais de 6 anos no cargo atual. Pessoas que estão no mestrado ou doutorado, independente da idade, foram tidos como estando no inicio da carreira academica.

data_geral_a= data_geral_a %>% 
  mutate(Group_time2 = case_when(
    `Q5_Qual é a sua idade?`== 'Acima de 60 anos' ~ "Senior",
    `Q5_Qual é a sua idade?`== 'Entre 20 a 29 anos' ~ "Junior",
    `Q5_Qual é a sua idade?`== 'Entre 30 a 39 anos' ~ "Junior",
    `Q5_Qual é a sua idade?`== 'Entre 40 a 49 anos' ~ "Senior",
    `Q5_Qual é a sua idade?`== 'Entre 50 a 59 anos' ~ "Senior",
    .default = "other")) %>%  
  mutate(Group_time2 = case_when(
    Group_time == "Senior" ~ "Senior",
    Group_occupation == "Master_PhD_Candidate" ~ "Junior",
    .default = as.character(Group_time2)))

data_geral_af<-data_geral_a %>% 
  filter(Group_occupation != 'other') %>% 
  filter(Group_gender  != 'other') %>% 
  select(- c(`Question_62_Descreva o por quê da sua resposta anterior.`,`Q64_Tem alguma outra coisa que você gostaria de adicionar - em relação ao questionário em geral, algum comentário específico, impressões gerais, experiências que gostaria de compartilhar?`, `Q64_Descreva o por quê da sua resposta anterior.`))

data_geral_count <- data_geral_af %>% group_by(Group_gender, Group_occupation, Group_time2)  %>% summarise(Count = n())

data_geral_count = data_geral_count %>% 
  mutate(Group_occupation= recode(Group_occupation, Master_PhD_Candidate = "Mst/PhD")) %>% 
  glimpse()

data_geral_count$Group_gender <- factor(data_geral_count$Group_gender, levels = c('Women', 'Men'))

write.csv(data_geral_af, "data_geral_af.csv")
write.csv(data_geral_a, "data_geral_a.csv")

groups_af = data_geral_af %>% 
  select(ID, Group_gender, Group_occupation, Group_time2)
colnames(groups_af)

data_matrix = groups_af %>%
  left_join(data_matrix)

########## Gather answers from multiple-choice questions #######################
# O código com # são as questões que ainda precisam ser trabalhadas na planilha
colnames(data_matrix)
colnames(data_geral_af)

data_geral_19 <- data_matrix %>% gather('Q_19_Var', 'Q_19_Answer', 9:24) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_19_Var', 'Q_19_Answer')
# 
data_geral_19sum <- data_geral_19 %>% group_by(Q_19_Var, Group_gender, Group_time2)  %>% summarise(Sum = sum(Q_19_Answer))
# 
data_geral_20 <- data_matrix %>% gather('Q_20_Var', 'Q_20_Answer', 26:36) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_20_Var', 'Q_20_Answer') 
# 
data_geral_21 <- data_matrix %>%gather('Q_21_Var', 'Q_21_Answer', 38:44) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_21_Var', 'Q_21_Answer')

# 
data_geral_22 <- data_matrix %>%gather('Q_22_Var', 'Q_22_Answer', 46:52) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_22_Var', 'Q_22_Answer')

# 
data_geral_24 <- data_matrix %>%gather('Q_24_Var', 'Q_24_Answer', 54:68) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_24_Var', 'Q_24_Answer') 
# 
data_geral_35 <- data_matrix %>% gather('Q_35_Var', 'Q_35_Answer', 70:76) %>% select('ID', 'Group_gender', 'Group_time2', 'Q_35_Var', 'Q_35_Answer')

#
data_geral_36 <- data_geral_af %>% gather('Q_36_Var', 'Q_36_Answer', 37:44)  %>% select('Carimbo de data/hora','Carimbo de data', 'ID', 'Group_gender', 'Group_time2', 'Group_time','Group_time2','Q_36_Var', 'Q_36_Answer')

#
data_geral_38 <- data_geral_af %>% gather('Q_38_Var', 'Q_38_Answer', 47:57)  %>% select('Carimbo de data/hora','Carimbo de data', 'ID', 'Group_gender', 'Group_time2','Group_time2', 'Q_38_Var', 'Q_38_Answer')

#
data_geral_40 <- data_geral_af %>% gather('Q_40_Var', 'Q_40_Answer', 60:68) %>% select('Carimbo de data/hora','Carimbo de data', 'ID', 'Group_gender', 'Group_time2', 'Q_40_Var', 'Q_40_Answer')

#
data_geral_43 <- data_geral_af %>% gather('Q_43_Var', 'Q_43_Answer', 72:85) %>% select('Carimbo de data/hora','Carimbo de data', 'ID', 'Group_gender','Group_time2', 'Q_43_Var', 'Q_43_Answer')

#
data_geral_44 <- data_matrix %>% gather('Q_44_Var', 'Q_44_Answer', 78:85) %>% select('ID', 'Group_gender', 'Group_occupation', 'Q_44_Var', 'Q_44_Answer')

#
data_geral_63 <- data_matrix %>% gather('Q_63_Var', 'Q_63_Answer', 87:94) %>% select('ID', 'Group_gender', 'Group_occupation', 'Q_63_Var', 'Q_63_Answer')



########################### Q4 identity ################################
data_geral_af = data_geral_af %>% 
  mutate(`Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Amarela' = "Asian"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Branca' = "White"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Parda' = "Mixed"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Preta' = "Black"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Indígena' = "Indigenous"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Prefiro não me classificar' = "Not identify"),
         `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Prefiro não responder' = "Not identify"))%>% 
  glimpse()

################################# Q5 Age #######################################

data_geral_af = data_geral_af %>% 
  mutate(`Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Acima de 60 anos' = "> 60"),
         `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 20 a 29 anos' = "20-29"),
         `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 30 a 39 anos' = "30-39"),
         `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 40 a 49 anos' = "40-49"),
         `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 50 a 59 anos' = "50-59"))


############################## Q 9 state of birth ##############################
data_geral_af<- data_geral_af %>% 
  mutate(`Q9_Qualéoseuestadodeorigem?` = case_when(`Q9_Qualéoseuestadodeorigem?` == "Minasgerais" ~ "Minas Gerais",
                                                   `Q9_Qualéoseuestadodeorigem?` == "Sãopaulo" ~ "São Paulo",
                                                   `Q9_Qualéoseuestadodeorigem?` == "Distritofederal" ~ "Distrito Federal",
                                                   `Q9_Qualéoseuestadodeorigem?` == "Riodejaneiro" ~ "Rio De Janeiro",
                                                   `Q9_Qualéoseuestadodeorigem?` == "Riograndedosul" ~ "Rio Grande Do Sul",
                                                   `Q9_Qualéoseuestadodeorigem?` == "Santacatarina" ~ "Santa Catarina",
         .default = as.character(`Q9_Qualéoseuestadodeorigem?`)))

unique(data_geral_af$`Q9_Qualéoseuestadodeorigem?`)


data_geral_af<- data_geral_af %>% 
  mutate(`Q13_Emqualestadovocêresideatualmente?` = case_when(`Q13_Emqualestadovocêresideatualmente?` == "Minasgerais" ~ "Minas Gerais",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Sãopaulo" ~ "São Paulo",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Distritofederal" ~ "Distrito Federal",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Riodejaneiro" ~ "Rio De Janeiro",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Riograndedosul" ~ "Rio Grande Do Sul",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Santacatarina" ~ "Santa Catarina",
                                                   `Q13_Emqualestadovocêresideatualmente?` == "Matogrosso" ~ "Mato Grosso",
                                                   .default = as.character(`Q13_Emqualestadovocêresideatualmente?`)))

unique(data_geral_af$`Q13_Emqualestadovocêresideatualmente?`)


################################# Q29 income #######################################
unique(data_geral_af$`Q28_Qual é a sua renda mensal?`)

data_geral_af<- data_geral_af %>% 
  mutate(`Q28_Qual é a sua renda mensal?` = case_when(`Q28_Qual é a sua renda mensal?` == "1 a 2 salários mínimos"  ~ "1-2 minimum salary",
                                                             `Q28_Qual é a sua renda mensal?` == "2 a 3 salários mínimos"  ~ "2-3 minimum salary",
                                                             `Q28_Qual é a sua renda mensal?` == "Mais que 10 salários mínimos" ~ ">10 minimum salary",
                                                             `Q28_Qual é a sua renda mensal?` == "Mais que 5 salários mínimos" ~ ">5 minimum salary",
                                                             `Q28_Qual é a sua renda mensal?` ==  "3 a 5 salários mínimos"  ~ "3-5 minimum salary",
                                                             `Q28_Qual é a sua renda mensal?` ==  "Prefiro não responder" ~ "Didnt answer",
                                                             .default = as.character(`Q28_Qual é a sua renda mensal?`)))

################################# Q31 more than one job #######################################
data_geral_af = data_geral_af %>% 
  mutate(`Q31_Você possui mais de uma fonte de renda?`= recode(`Q31_Você possui mais de uma fonte de renda?`, 'Sim' = "Yes"),
         `Q31_Você possui mais de uma fonte de renda?`= recode(`Q31_Você possui mais de uma fonte de renda?`, 'Não' = "No"))


################################# Q33 team preference #######################################

data_geral_af = data_geral_af %>% 
  mutate(case_when(`Q33_Você prefere trabalhar em uma equipe:`==  'Primordialmente feminina' ~ "Primordially female",
         `Q33_Você prefere trabalhar em uma equipe:`== 'Primordialmente masculina' ~ "Primordially male",
         `Q33_Você prefere trabalhar em uma equipe:`== 'Indiferente' ~ "Indifferent",
                                                             `Q33_Você prefere trabalhar em uma equipe:` ==  "Prefiro não responder" ~ "Didnt answer",
                                                             .default = as.character(`Q33_Você prefere trabalhar em uma equipe:`)))

################################# Q34 collegues #######################################

data_geral_af = data_geral_af %>% 
  mutate(case_when(`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`== 'Mulheres' ~ "Women",
         `Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`== 'Homens' ~ "Men",
         `Q34_Colegas que mais auxiliam em sua posição atual são principalmente:` == 'Ambos' ~ "Both",
         `Q34_Colegas que mais auxiliam em sua posição atual são principalmente:` ==  "Prefiro não responder" ~ "Didnt answer",
         .default = as.character(`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`))) 


#################### Q36 % gender in departament ###############################
data_36sum <- data_geral_36 %>% 
  group_by(Q_36_Var, Group_gender, Group_time2)  %>% 
  summarise(Sum = sum(Q_36_Answer))

data_36sum = data_36sum %>% mutate(
  Q_36_Var= recode(Q_36_Var, "Q36_Homens são facilmente selecionados para cargos de coordenação"  = "Men are easily selected for leadership positions"),
  Q_36_Var= recode(Q_36_Var, "Q36_Há mais homens que mulheres em meu departamento" = 'Men outnumber women in my department'), 
  Q_36_Var= recode(Q_36_Var, "Q36_Há mais mulheres que homens no meu departamento" = 'Women outnumber men in my department'), 
  Q_36_Var= recode(Q_36_Var,"Q36_Mulheres são facilmente selecionadas para cargos de coordenação" = 'Women are easily selected for leadership positions'),
  Q_36_Var= recode(Q_36_Var,"Q36_Não há distinção entre homens e mulheres para cargos de coordenação" = 'No gender-based leadership'),
  Q_36_Var= recode(Q_36_Var,"Q36_Não há distinção entre proporção de homens e mulheres" = 'No gender disparity'),
  Q_36_Var= recode(Q_36_Var,"Q36_Não se aplica" = 'Not applicable'),
  Q_36_Var= recode(Q_36_Var,"Q36_Prefiro não responder" = 'Didnt answer')) %>% 
  glimpse()

#################### Q38 % moral harassment ###############################

data_38sum <- data_geral_38 %>% 
  group_by(Q_38_Var, Group_gender, Group_time2)  %>% 
  summarise(Sum = sum(Q_38_Answer))

data_38sum = data_38sum %>% mutate(
  Q_38_Var= recode(Q_38_Var, "Q38_Algum homem já levou o crédito pelo trabalho feito por você"  = "A man ever took credit for your work"),
  Q_38_Var= recode(Q_38_Var, "Q38_Não passei por nenhuma das situações descritas nas opções anteriores" = 'Havent experienced any of the situations'), 
  Q_38_Var= recode(Q_38_Var, "Q38_Não se aplica"  = 'Not applicable'),
  Q_38_Var= recode(Q_38_Var,"Q38_Prefiro não responder" = 'Didnt answer'),
  Q_38_Var= recode(Q_38_Var, "Q38_Você já foi vista como agressiva ou desagradável por exercer sua autoridade ou mostrar sua opinião" = 'Perceived as aggressive or unpleasant \n for exercising authority'),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já passou por situações constrangedoras que envolveram piadas relacionadas ao seu gênero (ex: piadas sobre estar de TPM)" = 'Being subjected to gender-based jokes'),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já perdeu alguma promoção/benefício (bolsa produtividade) para algum homem, mesmo que tenha se empenhado igualmente" = 'Loss of promotion/grant for a men'),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já sentiu que o seu gênero foi decisivo ao não conseguir algum trabalho de liderança" = 'Career advancement influenced \n by your gender'),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já sentiu que o seu gênero foi decisivo ao não ser chamada para algum trabalho de campo"   = 'Field recruitment influenced by your Gender'),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já sentiu que seu gênero foi decisivo para ter uma opinião aceita" = "Opinion acceptance influenced by gender"),
  Q_38_Var= recode(Q_38_Var,"Q38_Você já sofreu alguma discriminação por estar grávida ou por ser mulher e poder engravidar" = 'Discrimination on the basis of pregnancy'))

#################### Q39 % sexual harassment ###############################
output_Q39 = data_geral_af %>% 
  group_by(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`, Group_gender, Group_time2) %>% 
  summarize(count = n()) %>% 
  mutate(percent = case_when(
    Group_gender == "Women" ~ count/214,
    .default = count/68)) #%>% 

unique(output_Q39$`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`)

output_Q39 = output_Q39 %>% 
  mutate(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`= recode(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`, 'Sim' = "Yes"),
         `Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`= recode(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`, 'Não' = "No"),
          `Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`= recode(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`, 'Prefiro não responder' = "Didnt \n answer"))

output_Q39$Group_gender <- factor(output_Q39$Group_gender, levels = c('Women', 'Men'))

########################## Q40 sexual harassment ###############################

data_40sum <- data_geral_40 %>% 
  group_by(Q_40_Var, Group_gender, Group_time2)  %>% 
  summarise(Sum = sum(Q_40_Answer)) %>% 
  mutate(Percent = case_when(
    Group_gender == "Women" ~ Sum/214,
    .default = Sum/68))

unique(data_40sum$Q_40_Var)

data_40sum = data_40sum %>% mutate(
  Q_40_Var= recode(Q_40_Var, "Q40_A situação foi exposta e a pessoa sofreu as devidas consequências"  = "The harasser has been exposed \n with consequences"),
  Q_40_Var= recode(Q_40_Var, "Q40_A situação foi exposta mas nada aconteceu ao assediador" = 'The harasser has been exposed \n with no consequences'), 
  Q_40_Var= recode(Q_40_Var, "Q40_A situação levou a minha demissão ou saída da instituição/empresa"  = 'The situation led to my resignation'),
  Q_40_Var= recode(Q_40_Var,"Q40_A situação levou à demissão ou saída do assediador da instituição/empresa" = 'The situation led to the harasser resignation'),
  Q_40_Var= recode(Q_40_Var, "Q40_A situação não foi exposta" = 'The situation was not exposed'),
  Q_40_Var= recode(Q_40_Var,"Q40_Não se aplica" = 'Not applicable'),
  Q_40_Var= recode(Q_40_Var,"Q40_O assediador era meu chefe/orientador" = 'The harasser was my supervisor'),
  Q_40_Var= recode(Q_40_Var,"Q40_Prefiro não responder" = 'Didnt answer'),
  Q_40_Var= recode(Q_40_Var,"Q40_Você foi coagida a não expor o assédio"   = 'Coerced not to disclose the situation'))%>% 
  glimpse()


data_40sum$Group_gender <- factor(data_40sum$Group_gender, levels = c('Women', 'Men'))

data_40sum$Q_40_Var <- factor(data_40sum$Q_40_Var, levels = c('Didnt answer', 'Not applicable', 'The situation led to the harasser resignation', "The harasser has been exposed \n with consequences",'The situation led to my resignation', 'Coerced not to disclose the situation', 'The harasser has been exposed \n with no consequences', 'The harasser was my supervisor','The situation was not exposed'))

########################## Q43 leaving academia ###############################

data_43sum <- data_geral_43 %>% 
  group_by(Q_43_Var, Group_gender)  %>% 
  summarise(Sum = sum(Q_43_Answer)) %>% 
  mutate(Percent =Sum/66)

unique(data_43sum$Q_43_Var)

data_43sum = data_43sum %>% mutate(
  Q_43_Var= recode(Q_43_Var, "Q43_Aposentada"  = "Retired"),
  Q_43_Var= recode(Q_43_Var, "Q43_Falta apoio financeiro (Concursos e/ou editais de pós-doutorado)" = 'Financial support is lacking'), 
  Q_43_Var= recode(Q_43_Var, "Q43_Melhor renda em outro lugar"  = 'Better income elsewhere'),
  Q_43_Var= recode(Q_43_Var,"Q43_Nenhuma posição científica disponível" = 'No scientific position available'),
  Q_43_Var= recode(Q_43_Var, "Q43_Perdi o interesse pela pesquisa" = 'I lost interest in research'),
  Q_43_Var= recode(Q_43_Var,"Q43_Não se aplica" = 'Not applicable'),
  Q_43_Var= recode(Q_43_Var,"Q43_Priorização da cidade/país onde mora em relação ao trabalho" = 'Prioritization of the city/country \n where you live in relation to work'),
  Q_43_Var= recode(Q_43_Var,"Q43_Prefiro não responder" = 'Didnt answer'),
  Q_43_Var= recode(Q_43_Var,"Q43_Priorizou estar perto da família"   = 'Prioritized being close to the family'),
  Q_43_Var= recode(Q_43_Var,"Q43_Responsabilidades familiares" = 'Family Responsibilities'),
  Q_43_Var= recode(Q_43_Var,"Q43_Situações de assédio moral"   = 'Situations of moral harassment'),
  Q_43_Var= recode(Q_43_Var,"Q43_Situações de assédio sexual"   = 'Situations of sexual harassment'),
  Q_43_Var= recode(Q_43_Var,"Q43_Situações de discriminação de gênero"   = 'Situations of gender discrimination'),
  Q_43_Var= recode(Q_43_Var,"Q43_Trabalhos mais interessantes em outros campos"   = 'More interesting jobs in other fields'))%>% 
  glimpse()


data_43sum$Group_gender <- factor(data_43sum$Group_gender, levels = c('Women', 'Men'))

data_43sum$Q_43_Var <- factor(data_43sum$Q_43_Var, levels = c('Didnt answer', 'Not applicable',"Retired", 'Situations of sexual harassment', 'Situations of gender discrimination',"Prioritization of the city/country \n where you live in relation to work", 'Family Responsibilities', 'Prioritized being close to the family', 'Situations of moral harassment',"I lost interest in research", 'More interesting jobs in other fields', 'No scientific position available','Better income elsewhere' ,'Financial support is lacking'))

####################Q58 - Have children ########################################

output_Q58 = data_geral_af %>% 
  group_by(`Q58_Você tem filhos?`, Group_gender, Group_time2) %>% 
  summarize(count = n()) %>% 
  mutate(percent = case_when(
    Group_gender == "Women" ~ count/214,
    .default = count/68))

output_Q58 = output_Q58 %>% 
  mutate(`Q58_Você tem filhos?`= recode(`Q58_Você tem filhos?`, 'Sim' = "Yes"),
         `Q58_Você tem filhos?`= recode(`Q58_Você tem filhos?`, 'Não' = "No"),)%>% 
  glimpse()

output_Q58$Group_gender <- factor(output_Q58$Group_gender, levels = c('Women', 'Men'))

####################Q57 - scientific production ########################################
output_Q57 = data_geral_af %>% 
  group_by(`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`, Group_gender, Group_time2) %>% 
  summarize(count = n()) %>% 
  mutate(percent = case_when(
    Group_gender == "Women" ~ count/214,
    .default = count/68))

output_Q57$`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?` <- factor(output_Q57$`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`, levels = c('0', '<1', '1-1.9', '2-2.9', '3-3.9', '4-7.9', '8-9.9', '>10'))


####################Q60 - Have children ########################################

output_Q60 = data_geral_af %>% 
  group_by(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, Group_gender, Group_time2) %>% 
  summarize(count = n()) %>% 
  mutate(percent = case_when(
    Group_gender == "Women" ~ count/214,
    .default = count/68))

output_Q60 = output_Q60 %>% 
  mutate(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`= recode(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, 'Sim' = "Yes"),
         `Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`= recode(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, 'Não' = "No"),
         `Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`= recode(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, 'Prefiro não responder' = "Didnt \n answer"))%>% 
  glimpse()

output_Q60$`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?` <- factor(output_Q58$`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, levels = c('20-29', '30-39', '40-49', '50-59', '> 60'))
output_Q60$Group_gender <- factor(output_Q60$Group_gender, levels = c('Women', 'Men'))

################# Q70 How they see their career ################################
assedio<-data_geral_af %>% 
  filter(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?` == "Sim")

output_Q70 = assedio %>% 
  group_by(`Q64_Como você classifica a sua vida pessoal neste momento?`, Group_gender, Group_time2) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count))

output_Q70$Group_gender <- factor(output_Q70$Group_gender, levels = c('Women', 'Men'))


output_Q70 = output_Q70 %>% 
  mutate(`Q64_Como você classifica a sua vida pessoal neste momento?`= recode(`Q64_Como você classifica a sua vida pessoal neste momento?`, 'Bem sucedida' = "Sucessfull"),
         `Q64_Como você classifica a sua vida pessoal neste momento?`= recode(`Q64_Como você classifica a sua vida pessoal neste momento?`, 'Muito bem sucedida' = "Very sucessfull"),
         `Q64_Como você classifica a sua vida pessoal neste momento?`= recode(`Q64_Como você classifica a sua vida pessoal neste momento?`, 'Muito sem sucesso' = "Very unsucessfull"),
         `Q64_Como você classifica a sua vida pessoal neste momento?`= recode(`Q64_Como você classifica a sua vida pessoal neste momento?`, 'Sem sucesso' = "Unsucessfull"),
         `Q64_Como você classifica a sua vida pessoal neste momento?`= recode(`Q64_Como você classifica a sua vida pessoal neste momento?`, 'Prefiro não responder' = "Didnt answer"))

####### Q- 45 a 55 - Fatores que influenciam a produtividade científica ########

# Data
output_Q45 = data_geral_af %>% 
  group_by(data_geral_af$`Q45_Como os fatores listados influenciam a sua produtividade científica? [Idade]`, Group_gender, Group_time2) %>% 
  summarize("Age" = n()) 
 # mutate(percent45 = Age/sum(Age))

output_Q46 = data_geral_af %>% 
  group_by(data_geral_af$`Q46_Como os fatores listados influenciam a sua produtividade científica? [Raça/Etnia]`, Group_gender, Group_time2) %>% 
  summarize("Ethnicity" = n()) 
  #mutate(percent46 = Ethnicity/sum(Ethnicity))

output_Q47 = data_geral_af %>% 
  group_by(`Q47_Como os fatores listados influenciam a sua produtividade científica? [Nível Socioeconômico]`,  Group_gender, Group_time2) %>% 
  summarize("Socioeconomic level" = n()) 
  #mutate(percent47 = Socioeconomic_Level/sum(Socioeconomic_Level))

output_Q48 = data_geral_af %>% 
  group_by(`Q48_Como os fatores listados influenciam a sua produtividade científica? [Origem geográfica]`, Group_gender, Group_time2) %>% 
  summarize("Geographical origin" = n()) 
  #mutate(percent48 = Geographical_origin/sum(Geographical_origin))

output_Q49 = data_geral_af %>% 
  group_by(`Q49_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades familiares]`, Group_gender, Group_time2) %>% 
  summarize("Family responsibilities" = n())  
  #mutate(percent49 = Family_responsibilities/sum(Family_responsibilities))

output_Q50 = data_geral_af %>% 
  group_by(`Q50_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades com ensino]`,  Group_gender, Group_time2) %>% 
  summarize("Teaching responsibilities" = n()) 
  #mutate(percent50 = Teaching_Responsibilities/sum(Teaching_Responsibilities))

output_Q51 = data_geral_af %>% 
  group_by(`Q51_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades administrativas]`, Group_gender, Group_time2) %>% 
  summarize("Administrative responsibilities" = n())
  #mutate(percent51 = Administrative_responsibilities/sum(Administrative_responsibilities))

output_Q52 = data_geral_af %>% 
  group_by(`Q52_Como os fatores listados influenciam a sua produtividade científica? [Falta de segurança no trabalho]`,  Group_gender, Group_time2) %>% 
  summarize("Lack of job security" = n()) 
#  mutate(percent52 = Lack_jobsecurity/sum(Lack_jobsecurity))

output_Q53 = data_geral_af %>% 
  group_by(`Q53_Como os fatores listados influenciam a sua produtividade científica? [Falta de financiamento]`,  Group_gender, Group_time2) %>% 
  summarize("Lack of funding" = n()) 
 # mutate(percent53 = Lack_of_funding/sum(Lack_of_funding))

output_Q54 = data_geral_af %>% 
  group_by(`Q54_Como os fatores listados influenciam a sua produtividade científica? [Falta de recursos para fazer meu trabalho (por exemplo, ajuda de escritório/escritório, laboratório, equipamentos, infraestrutura, etc.)]`,  Group_gender, Group_time2) %>% 
  summarize("Lack of resources" = n()) 
#  mutate(percent54 = Lack_of_resources/sum(Lack_of_resources))

output_Q55 = data_geral_af %>% 
  group_by(`Q55_Como os fatores listados influenciam a sua produtividade científica? [Discriminação de sexo/gênero]`,  Group_gender, Group_time2) %>%
  summarize("Gender discrimination" = n())  
  #mutate(percent55 = Gender_discrimination/sum(Gender_discrimination))

colnames(output_Q45)[1]<-"Influence"
colnames(output_Q46)[1]<-"Influence"
colnames(output_Q47)[1]<-"Influence"
colnames(output_Q48)[1]<-"Influence"
colnames(output_Q49)[1]<-"Influence"
colnames(output_Q50)[1]<-"Influence"
colnames(output_Q51)[1]<-"Influence"
colnames(output_Q52)[1]<-"Influence"
colnames(output_Q53)[1]<-"Influence"
colnames(output_Q54)[1]<-"Influence"
colnames(output_Q55)[1]<-"Influence"

Q_45_55 <- output_Q45 %>% 
  full_join(output_Q46) %>% 
  full_join(output_Q47) %>% 
  full_join(output_Q48) %>% 
  full_join(output_Q49) %>% 
  full_join(output_Q50) %>% 
  full_join(output_Q51) %>% 
  full_join(output_Q52) %>% 
  full_join(output_Q53) %>% 
  full_join(output_Q54) %>% 
  full_join(output_Q55)

Q_45_55_unite= Q_45_55 %>%  gather("Type", "Count", 4:14)
Q_45_55_unite$Count[is.na(Q_45_55_unite$Count)]<-0
Q_45_55_unite2 = Q_45_55_unite %>% mutate(Influence = recode(Influence, 'Neutro' = "Neutral/No impact", 'Impedimento' = "Impediment", 'Não impactou' = "Neutral/No impact", 'Acelerador'= "Accelerator", 'Grande Acelerador'= "Big Accelerator", 'Grande impedimento'= "Big impediment")) %>% ungroup()
str(Q_45_55_unite2)

Q_45_55_unite2$Type<-as.character(Q_45_55_unite2$Type)
Q_45_55_unite2$Influence<-as.factor(Q_45_55_unite2$Influence)
Q_45_55_unite2$Count<-as.integer(Q_45_55_unite2$Count)

Q_45_55_unite2$Influence <- factor(Q_45_55_unite2$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

Q_45_55_unite2$Type <- factor(Q_45_55_unite2$Type, levels = c("Ethnicity", "Age", "Geographical origin", "Teaching responsibilities", "Administrative responsibilities", "Gender discrimination", "Lack of job security", "Socioeconomic level", "Family responsibilities", "Lack of resources", "Lack of funding"))

############# join outputs

data_geral_19sum$Question <- "Q19"
data_geral_20sum$Question <- "Q20"
data_36sum$Question <- "Q36"
data_38sum$Question <- "Q38"
data_40sum$Question <- "Q40"
data_geral_44sum$Question <- "Q44"

data_geral_19sum <- data_geral_19sum %>% 
  rename("Category" = Q_19_Var) %>% 
  select(- Percent)

data_geral_20sum <- data_geral_20sum %>% 
  rename("Category" = Q_20_Var)  
  #select(- Percent)

data_36sum <- data_36sum %>% 
  rename("Category" = Q_36_Var) %>% 
  select(- percent)

data_38sum <- data_38sum %>% 
  rename("Category" = Q_38_Var) %>% 
  select(- percent)

data_40sum <- data_40sum %>% 
  rename("Category" = Q_40_Var) %>% 
  select(- percent)

data_geral_44sum <- data_geral_44sum %>% 
  rename("Category" = Q_44_Var)

Join_Sum_Q <- data_geral_19sum %>%
  full_join(data_geral_20sum) %>% 
  full_join(data_36sum) %>% 
  full_join(data_38sum) %>% 
  full_join(data_40sum) %>% 
  full_join(data_geral_44sum)


output_Q33$Question <- "Q33"
output_Q34$Question <- "Q34"
output_Q39$Question <- "Q39"
output_Q41$Question <- "Q41"
output_Q57$Question <- "Q57"
output_Q58$Question <- "Q58"
output_Q60$Question <- "Q60"
output_Q61$Question <- "Q61"

output_Q33 <- output_Q33 %>% 
  rename("Category" = Q33_Você.prefere.trabalhar.em.uma.equipe.) %>% 
  select(- percent)

output_Q34 <- output_Q34 %>% 
  rename("Category" = `Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`) 

output_Q39 <- output_Q39 %>% 
  rename("Category" = Question_39_Você.já.sofreu.assédio.sexual.por.algum.colega.de.trabalho.) %>% 
  select(- percent)

output_Q41 <- output_Q41 %>% 
  rename("Category" = Q41_Se.você.fez.pós.graduação..ex..especialização..mestrado.ou.doutorado...a.pessoa.que.te.orientou.por.último.foi.) %>% 
  select(- percent)

output_Q57 <- output_Q57 %>% 
  rename("Category" = Q57_Qual.é.a.sua.MÉDIA.ANO.de.produção.científica..artigos..capítulos.de.livros..livros...nos.últimos.4.anos..ou.seja..de.2019.até.o.momento.) %>% 
  select(- percent)

output_Q58 <- output_Q58 %>% 
  rename("Category" = Q58_Você.tem.filhos.) %>% 
  select(- percent)

output_Q60 <- output_Q60 %>% 
  rename("Category" = Q60_Sua.escolha.profissional.influencia.a.sua.vontade.de.ter.filhos.as..) %>% 
  select(- percent)

output_Q61 <- output_Q61 %>% 
  rename("Category" = Q61_Você.acha.que.a.licença.maternidade.pode.impactar.negativamente.na.profissão.das.mulheres.) %>% 
  select(- percent)

Join_Count_Q <- output_Q33 %>% 
  full_join(output_Q34) %>% 
  full_join(output_Q39) %>% 
  full_join(output_Q41) %>% 
  full_join(output_Q57) %>%
  full_join(output_Q58) %>% 
  full_join(output_Q60) %>% 
  full_join(output_Q61)

write.csv(Join_Count_Q, "./Results/Join_Count_Q.csv")  
write.csv(Join_Sum_Q, "./Results/Join_Sum_Q.csv")  
write.csv(Q_45_55_unite2,"./Results/Q_45_55_unite2.csv") 
