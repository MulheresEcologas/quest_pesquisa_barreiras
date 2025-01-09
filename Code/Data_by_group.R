# Author: Ana Paula Lula Costa
# January 8th 2025
# Data wrangling of the paper: Challenges Faced in Ecological Sciences Careers: A Case Study of Gender Perceptions in Brazil 

# Loading the packages ----
library(tidyverse)# Para manipulação e visualização dos dados
library(ggplot2)#
library(dplyr)#
library(viridis)# Para cores
library(scales)#
library(ggthemes)# Para themes
library(ggrepel)

# Load data from Google Forms csv. ----
data_geral <- readxl::read_xlsx("./Data/respostas_cleaned_final.xlsx")
data_matrix <- read.csv("C:/Users/anapa/OneDrive/Desktop/Nuvem/Projetos/Mulheres na Ecologia/Análises_Artigo_MEco/data_matrix.csv")
colnames(data_geral)
colnames(data_matrix)

data_matrix = data_matrix %>% 
 dplyr::rename("Carimbo de data/hora"= "Carimbo.de.data.hora.x",
        "Carimbo de data" = "Carimbo.de.data.x")

# Grouping and filtering data ----
## First group category: by gender ----
### Q3 ----
unique(data_geral$`Q3_Qual a sua identidade de gênero?`) #look at gender categories

# group into 2 categories "Men" and "Women"
data_geral_i= data_geral %>% 
 mutate(Group_gender = case_when(
   `Q3_Qual a sua identidade de gênero?`== "Homem cisgênero" ~ "Men",
   `Q3_Qual a sua identidade de gênero?` == "Homem Pansexual"~ "Men", 
   `Q3_Qual a sua identidade de gênero?` == 'Mulher cisgênera' ~ "Women",
   .default = "other"))

## Filtering data ----
### Q7 and Q30 country and institutions ----
# Filtering answers of respondents who were born and/or are currently working in Brazil
# Filtering answers of respondents that are working at universities or research institutions
data_geral_a <- data_geral_i %>% 
 filter(`Q7_Qualéoseupaísdeorigem?`== 'Brasil')%>% 
 filter(`Q30_Qual é a melhor descrição da instituição onde você trabalha atualmente?` %in%
      c("Universidade pública", "Universidade Comunitária", "Universidade comunitária", "Universidade privada", "Instituição/Fundação de pesquisa privada", "Instituição de pesquisa pública", "Instituto Federal de Educação, Ciência e Tecnologia"))

### Q25 Currently position ----

data_geral_a= data_geral_a %>% 
 mutate(Group_occupation = case_when(
  `Q25_Qual categoria melhor descreve a sua situação atual?`== "Pesquisador(a) de mestrado/doutorado" ~ "Master_PhD_Candidate",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) associada" ~ "Professor",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor Colaborador Adjunto (temporário)"~ "Professor", 
  `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) titular" ~ "Professor",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) adjunta" ~ "Professor",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisador(a) pós-doutorado' ~ "Researcher",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisador(a) em instituição de pesquisa' ~ "Researcher",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Pesquisadora voluntária' ~ "Researcher",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == 'Professor visitante' ~ "Professor",
  `Q25_Qual categoria melhor descreve a sua situação atual?` == "Professor(a) assistente" ~ "Professor",
  .default = "other"))

## Second group category: by their carrier stage ----
# Used 2 questions as a proxy to categorize the level of experience: time in the current position and age
### Q26 ----
# 1. Professionals that were in the same position for 6 or more years were considered seniors and the ones that were less than 6 years in the current position were considered juniors

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

### Q5 ----
# 2. We categorized as Seniors those who are 40 years old or older. Individuals pursuing a master’s or doctoral degree, regardless of age, were considered to be at the beginning of their academic careers (junior).
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

# Filter all respondents that are not in academia or were self-identified in a gender different than men or women.
# Remove open questions from data

data_geral_af <-data_geral_a %>% 
 filter(Group_occupation != 'other') %>% 
 filter(Group_gender != 'other') %>% 
 select(- c(`Question_62_Descreva o por quê da sua resposta anterior.`,`Q64_Tem alguma outra coisa que você gostaria de adicionar - em relação ao questionário em geral, algum comentário específico, impressões gerais, experiências que gostaria de compartilhar?`, `Q64_Descreva o por quê da sua resposta anterior.`))

### Summarize data set ----
data_geral_count <- data_geral_af %>% 
 group_by(Group_gender, Group_occupation, Group_time2) %>% 
 summarise(Count = n())

data_geral_count = data_geral_count %>% 
 mutate(Group_occupation= recode(Group_occupation, Master_PhD_Candidate = "Mst/PhD")) %>% 
 glimpse()

data_geral_count$Group_gender <- factor(data_geral_count$Group_gender, levels = c('Women', 'Men'))

# Gather answers from multiple-choice questions ----
colnames(data_matrix)
colnames(data_geral_af)

### Include categories in multiple-choice question's dataset ----
data_group <- data_geral_af %>% 
 select(ID, Group_gender, Group_time2)

data_matrix <- full_join(data_matrix, data_group) %>% 
 filter(Group_gender != is.na(Group_gender))

### Q19 Main factors that lead you to move ----
data_geral_19 <- data_matrix %>% 
 gather('Category', 'Q_19_Answer', 9:24) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_19_Answer')
 
data_geral_19sum <- data_geral_19 %>% 
 group_by(Category, Group_gender, Group_time2) %>%
 summarise(Sum = sum(Q_19_Answer))%>%
 ungroup()

data_geral_19sum = data_geral_19sum %>% 
 mutate(
  Category= recode(Category, "Acabou.o.contrato.como.professor.a..temporária" = "End of temporary contract"),
  Category= recode(Category, "Aposentada..o.e." = 'Retired'), 
  Category= recode(Category, "Aumento.significativo.no.meu.cargo.e.ou.salário" = 'Better salary'), 
  Category= recode(Category,"Começou.o.contrato.como.professor.a..temporária.professora.efetiva" = 'Starting a temporary or permanent contract'),
  Category= recode(Category,"Demitida..o.e..do.último.emprego" = 'Fired from my last job'),
  Category= recode(Category,"Escolha.de.programa.de.pós.graduação" = 'Postgraduate programme preference'),
  Category= recode(Category,"Maior.prestígio.do.trabalho" = "Increased status at work"),
  Category= recode(Category,"Maior.segurança.no.trabalho" = 'Work security'),
  Category= recode(Category,"Maior.ênfase.na.pesquisa" = 'Focus on research'),
  Category= recode(Category,"Melhor.qualidade.de.vida" = 'Better life quality'),
  Category= recode(Category,"Morar.mais.perto.de.parentes.e.amigos" = 'Live close to family and friends'),
  Category= recode(Category,"Ênfase.reduzida.na.pesquisa" = 'Less focus on research'),
  Category= recode(Category,"Única.oferta.de.emprego.que.tive" = 'Only job offer that I had'),
  Category= recode(Category,"Mudança.de.companheiro.a..outra.pessoa.significativa" = 'Relocating because my partner has moved'),
  Category= recode(Category,"Não.se.aplica.x" = 'Not applicable'),
  Category= recode(Category,"Prefiro.não.responder.x" = 'Didnt answer'))


data_geral_19sum$Group_gender <- factor(data_geral_19sum$Group_gender, levels = c('Women', 'Men'))

data_geral_19sum$Category <- as.factor(data_geral_19sum$Category)

### Q20 Consequences of moving to advance your career ----
data_geral_20 <- data_matrix %>% 
 gather('Category', 'Q_20_Answer', 26:36) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_20_Answer') 

data_geral_20sum <- data_geral_20 %>% 
 group_by(Category, Group_gender, Group_time2) %>%
 summarise(Sum = sum(Q_20_Answer))%>%
 ungroup()

data_geral_20sum = data_geral_20sum %>% mutate(
 Category= recode(Category, "Com.minha.família.de.origem.parentes..mãe.pai.irmã.o.tia.o.prima.o.etc..x" = "End of temporary contract"),
 Category= recode(Category, "Companheiro.a..outra.pessoa.não.se.mudou.mas.o.relacionamento.permaneceu.intacto" = 'Partner or significant other hasnt moved in with me, and the relationship had no significant negative effects'), 
 Category= recode(Category, "Companheiro.a..outra.pessoa.significativa.mudou.se.comigo.com.efeitos.negativos.mínimos.sobre.relação" = 'Partner or significant other has moved in with me, with no significant negative effects on the relationship'), 
 Category= recode(Category,"Companheiro.a..outra.pessoa.significativa.não.se.mudou.comigo.e.o.relacionamento.sofreu.efeitos.adversos" = 'Partner or significant other has moved in with me, and the relationship had significant negative effects'),
 Category= recode(Category,"Companheiro.a..outra.pessoa.significativa.se.mudou.comigo.mas.com.efeitos.negativos.significativos.no.relacionamento" = 'Partner or significant other hasnt moved in with me, but with significant negative effects on the relationship'),
 Category= recode(Category,"Companheiro.a..outra.pessoa.significativa.se.mudou.comigo.mas.com.problemas.significativos.na.carreira.do.cônjuge.outra.pessoa.significativa" = 'Partner or other significant person has moved in with me, but has career problems'),
 Category= recode(Category,"Filhos.mudaram.com.nenhum.ou.mínimos.efeitos.negativos" = "Children moved with none or minimum adverse effects"),
 Category= recode(Category,"Filhos.não.se.mudaram" = 'Children didnt move'),
 Category= recode(Category,"Filhos.se.mudaram.mas.sofreram.efeitos.adversos.significativos" = 'Children moved but suffered significant adverse effects'),
 Category= recode(Category,"Não.se.aplica.y" = 'Not applicable'),
 Category= recode(Category,"Prefiro.não.responder.y" = 'Didnt answer'))


data_geral_20sum$Group_gender <- factor(data_geral_20sum$Group_gender, levels = c('Women', 'Men'))

data_geral_20sum$Category <- as.factor(data_geral_20sum$Category)

### Q21 with whom do you live? ----
data_geral_21 <- data_matrix %>%
 gather('Category', 'Q_21_Answer', 38:44) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_21_Answer')

data_geral_21sum <- data_geral_21 %>% 
 group_by(Category, Group_gender, Group_time2) %>%
 summarise(Sum = sum(Q_21_Answer))%>%
 ungroup()

data_geral_21sum = data_geral_21sum %>% 
 mutate(
 Category= recode(Category, "Com.amiga.o.s..ou.colega.o.s." = "With friends and colegues"),
 Category= recode(Category, "Com.minha.família.de.origem.parentes..mãe.pai.irmã.o.tia.o.prima.o.etc..y" = 'With my original family'), 
 Category= recode(Category, "Com.minha.meu.companheira.o" = 'With my partner'), 
 Category= recode(Category,"Com.minha.s..meu.s..filha.s..filho.s." = 'With my children'),
 Category= recode(Category,"Em.república.pensionato.etc." = 'in a shared-flat or boarding house'),
 Category= recode(Category,"Sozinho.a." = 'Alone'),
 Category= recode(Category,"Prefiro.não.responder.x.x" = 'Didnt answer'))

data_geral_21sum$Group_gender <- factor(data_geral_21sum$Group_gender, levels = c('Women', 'Men'))

data_geral_21sum$Category <- as.factor(data_geral_21sum$Category)

### Q22 housework ----
data_geral_22 <- data_matrix %>%
 gather('Category', 'Q_22_Answer', 46:52) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_22_Answer')

data_geral_22sum <- data_geral_22 %>% 
 group_by(Category, Group_gender, Group_time2) %>%
 summarise(Sum = sum(Q_22_Answer))%>%
 ungroup()

data_geral_22sum = data_geral_22sum %>% 
 mutate(
 Category= recode(Category, "São.feitas.de.modo.colaborativo...todas.os..es..residentes.são.igualmente.responsáveis.x" = "All residents all equally responsable"),
 Category= recode(Category, "São.feitas.pelo.cônjuge.outra.pessoa.significativa.x" = 'They are done by my partner or other significannt person'), 
 Category= recode(Category, "São.feitas.por.crianças.x" = 'They are done by children'), 
 Category= recode(Category,"São.feitas.por.mim.x" = 'They are done by me'),
 Category= recode(Category,"São.feitas.por.outros.residentes.x" = 'They are done by other residents'),
 Category= recode(Category,"São.feitas.por.uma.ajuda.paga.x" = 'They are done by a paid help'),
 Category= recode(Category,"Prefiro.não.responder.y.y" = 'Didnt answer'))

data_geral_22sum$Group_gender <- factor(data_geral_22sum$Group_gender, levels = c('Women', 'Men'))

data_geral_22sum$Category <- as.factor(data_geral_22sum$Category)

### Q24 ecology graduation ----
data_geral_24 <- data_matrix %>%
 gather('Category', 'Q_24_Answer', 54:62) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_24_Answer')

data_geral_24sum <- data_geral_24 %>% 
 group_by(Category, Group_gender, Group_time2) %>%
 summarise(Sum = sum(Q_24_Answer))%>%
 ungroup()

data_geral_24sum = data_geral_24sum %>% 
 mutate(
 Category= recode(Category, "TCC.na.área.de.ecologia" = "Undergraduate thesis in ecology"),
 Category= recode(Category, "Mestrado.na.área.de.ecologia..concluído.ou.em.andamento." = "Master's degree in ecology"), 
 Category= recode(Category, "Mestrado.em.área.correlata.com.dissertação.voltada.a.área.de.ecologia..concluído.ou.em.andamento." = "Master's degree with a dissertation focused on ecology"), 
 Category= recode(Category,"Doutorado.na.área.de.ecologia..concluído.ou.em.andamento." = "Doctorate in ecology"),
 Category= recode(Category,"Doutorado.em.área.correlata.com.tese.voltada.a.área.de.ecologia..concluído.ou.em.andamento." = "Doctorate with a thesis focused on ecology"),
 Category= recode(Category,"Especialização.em.ecologia..concluído.ou.em.andamento." = "Specialization in ecology"),
 Category= recode(Category,"Nenhuma.das.anteriores.mas.faço.pesquisa.na.área.de.ecologia" = "Conduct research in the field of ecology"),
 Category= recode(Category,"Nenhuma.das.anteriores.mas.possuo.emprego.não.acadêmico.na.área.de.ecologia" = "Have a non-academic job in ecology"),
 Category= recode(Category,"Prefiro.não.responder.x.x.x" = 'Didnt answer'))

data_geral_24sum$Group_gender <- factor(data_geral_24sum$Group_gender, levels = c('Women', 'Men'))

data_geral_24sum$Category <- as.factor(data_geral_24sum$Category)

### Q35 hiring and promoting ----
data_geral_35 <- data_matrix %>% 
 gather('Category', 'Q_35_Answer', 70:76) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_35_Answer')

data_35sum <- data_geral_35 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 summarise(Sum = sum(Q_35_Answer))

data_35sum = data_35sum %>% mutate(
 Category= recode(Category, "Homens.não.são.facilmente.contratados.e.ou.promovidos" = "Men are not easily hired and/or promoted"),
 Category= recode(Category, "Homens.são.facilmente.contratados.e.ou.promovidos" = 'Men are easily hired and/or promoted'), 
 Category= recode(Category, "Mulheres.não.são.facilmente.contratadas.e.ou.promovidas" = 'Women are not easily hired and/or promoted'), 
 Category= recode(Category,"Mulheres.são.facilmente.contratadas.e.ou.promovidas" = 'Women are easily hired and/or promoted'),
 Category= recode(Category,"Não.há.distinção.entre.contratação.e.promoção.de.homens.e.mulheres" = 'No gender disparity in hiring and promotion'),
 Category= recode(Category,"Não.se.aplica.x.x" = 'Not applicable'),
 Category= recode(Category,"Prefiro.não.responder.y.y.y" = 'Didnt answer'))

data_35sum$Group_gender <- factor(data_35sum$Group_gender, levels = c('Women', 'Men'))
data_35sum$Category <- as.factor(data_35sum$Category)

### Q36 % gender in departament ----
data_geral_36 <- data_geral_af %>% 
 gather('Category', 'Q_36_Answer', 37:44) %>% 
 select('ID', 'Group_gender', 'Group_time2','Category', 'Q_36_Answer')

data_36sum <- data_geral_36 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 summarise(Sum = sum(Q_36_Answer))

data_36sum = data_36sum %>% mutate(
 Category= recode(Category, "Q36_Homens são facilmente selecionados para cargos de coordenação" = "Men are easily selected for leadership positions"),
 Category= recode(Category, "Q36_Há mais homens que mulheres em meu departamento" = 'Men outnumber women in my department'), 
 Category= recode(Category, "Q36_Há mais mulheres que homens no meu departamento" = 'Women outnumber men in my department'), 
 Category= recode(Category,"Q36_Mulheres são facilmente selecionadas para cargos de coordenação" = 'Women are easily selected for leadership positions'),
 Category= recode(Category,"Q36_Não há distinção entre homens e mulheres para cargos de coordenação" = 'No gender-based leadership'),
 Category= recode(Category,"Q36_Não há distinção entre proporção de homens e mulheres" = 'No gender disparity'),
 Category= recode(Category,"Q36_Não se aplica" = 'Not applicable'),
 Category= recode(Category,"Q36_Prefiro não responder" = 'Didnt answer'))

### Q38 % moral harassment ----
data_geral_38 <- data_geral_af %>% 
 gather('Category', 'Q_38_Answer', 47:57) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_38_Answer')

data_38sum <- data_geral_38 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 summarise(Sum = sum(Q_38_Answer))

data_38sum = data_38sum %>% mutate(
 Category= recode(Category, "Q38_Algum homem já levou o crédito pelo trabalho feito por você" = "A man ever took credit for your work"),
 Category= recode(Category, "Q38_Não passei por nenhuma das situações descritas nas opções anteriores" = 'Havent experienced any of the situations'), 
 Category= recode(Category, "Q38_Não se aplica" = 'Not applicable'),
 Category= recode(Category,"Q38_Prefiro não responder" = 'Didnt answer'),
 Category= recode(Category, "Q38_Você já foi vista como agressiva ou desagradável por exercer sua autoridade ou mostrar sua opinião" = 'Perceived as aggressive or unpleasant for exercising authority'),
 Category= recode(Category,"Q38_Você já passou por situações constrangedoras que envolveram piadas relacionadas ao seu gênero (ex: piadas sobre estar de TPM)" = 'Being subjected to gender-based jokes'),
 Category= recode(Category,"Q38_Você já perdeu alguma promoção/benefício (bolsa produtividade) para algum homem, mesmo que tenha se empenhado igualmente" = 'Loss of promotion/grant for a men'),
 Category= recode(Category,"Q38_Você já sentiu que o seu gênero foi decisivo ao não conseguir algum trabalho de liderança" = 'Career advancement influenced by your gender'),
 Category= recode(Category,"Q38_Você já sentiu que o seu gênero foi decisivo ao não ser chamada para algum trabalho de campo" = 'Field recruitment influenced by your Gender'),
 Category= recode(Category,"Q38_Você já sentiu que seu gênero foi decisivo para ter uma opinião aceita" = "Opinion acceptance influenced by gender"),
 Category= recode(Category,"Q38_Você já sofreu alguma discriminação por estar grávida ou por ser mulher e poder engravidar" = 'Discrimination on the basis of pregnancy'))

### Q40 sexual harassment ----
data_geral_40 <- data_geral_af %>% 
 gather('Category', 'Q_40_Answer', 60:68) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_40_Answer')

data_40sum <- data_geral_40 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 summarise(Sum = sum(Q_40_Answer))

data_40sum = data_40sum %>% mutate(
 Category= recode(Category, "Q40_A situação foi exposta e a pessoa sofreu as devidas consequências" = "The harasser has been exposed with consequences"),
 Category= recode(Category, "Q40_A situação foi exposta mas nada aconteceu ao assediador" = 'The harasser has been exposed with no consequences'), 
 Category= recode(Category, "Q40_A situação levou a minha demissão ou saída da instituição/empresa" = 'The situation led to my resignation'),
 Category= recode(Category,"Q40_A situação levou à demissão ou saída do assediador da instituição/empresa" = 'The situation led to the harasser resignation'),
 Category= recode(Category, "Q40_A situação não foi exposta" = 'The situation was not exposed'),
 Category= recode(Category,"Q40_Não se aplica" = 'Not applicable'),
 Category= recode(Category,"Q40_O assediador era meu chefe/orientador" = 'The harasser was my supervisor'),
 Category= recode(Category,"Q40_Prefiro não responder" = 'Didnt answer'),
 Category= recode(Category,"Q40_Você foi coagida a não expor o assédio" = 'Coerced not to disclose the situation'))

### Q43 leaving academia ----
data_geral_43 <- data_geral_af %>% 
 gather('Category', 'Q_43_Answer', 72:85) %>% 
 select('ID', 'Group_gender','Group_time2', 'Category', 'Q_43_Answer')

data_43sum <- data_geral_43 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 summarise(Sum = sum(Q_43_Answer))

data_43sum = data_43sum %>% mutate(
 Category= recode(Category, "Q43_Aposentada" = "Retired"),
 Category= recode(Category, "Q43_Falta apoio financeiro (Concursos e/ou editais de pós-doutorado)" = 'Financial support is lacking'), 
 Category= recode(Category, "Q43_Melhor renda em outro lugar" = 'Better income elsewhere'),
 Category= recode(Category,"Q43_Nenhuma posição científica disponível" = 'No scientific position available'),
 Category= recode(Category, "Q43_Perdi o interesse pela pesquisa" = 'I lost interest in research'),
 Category= recode(Category,"Q43_Não se aplica" = 'Not applicable'),
 Category= recode(Category,"Q43_Priorização da cidade/país onde mora em relação ao trabalho" = 'Prioritization of the city/country where you live in relation to work'),
 Category= recode(Category,"Q43_Prefiro não responder"  = 'Didnt answer'),
 Category= recode(Category,"Q43_Priorizou estar perto da família" = 'Prioritized being close to the family'),
 Category= recode(Category,"Q43_Responsabilidades familiares" = 'Family Responsibilities'),
 Category= recode(Category,"Q43_Situações de assédio moral" = 'Situations of moral harassment'),
 Category= recode(Category,"Q43_Situações de assédio sexual" = 'Situations of sexual harassment'),
 Category= recode(Category,"Q43_Situações de discriminação de gênero" = 'Situations of gender discrimination'),
 Category= recode(Category,"Q43_Trabalhos mais interessantes em outros campos" = 'More interesting jobs in other fields'))

data_43sum$Group_gender <- factor(data_43sum$Group_gender, levels = c('Women', 'Men'))

data_43sum$Category <- factor(data_43sum$Category, levels = c('Didnt answer', 'Not applicable',"Retired", 'Situations of sexual harassment', 'Situations of gender discrimination',"Prioritization of the city/country where you live in relation to work", 'Family Responsibilities', 'Prioritized being close to the family', 'Situations of moral harassment',"I lost interest in research", 'More interesting jobs in other fields', 'No scientific position available','Better income elsewhere' ,'Financial support is lacking'))

### Q44 field work ----
data_geral_44 <- data_matrix %>% 
 gather('Category', 'Q_44_Answer', 78:83) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_44_Answer')

data_44sum <- data_geral_44 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 mutate(Q_44_Answer = as.double(Q_44_Answer)) %>% 
 summarise(Sum = sum(Q_44_Answer))

data_44sum = data_44sum %>% mutate(
 Category= recode(Category, "Alternativa.para.proporcionar.experiência.educacional" = "Educational experience"),
 Category= recode(Category, "Companhia" = 'Company'), 
 Category= recode(Category, "Preocupações.com.a.segurança.pessoal" = 'Worried about personal security'),
 Category= recode(Category, "Quantidade.de.trabalho..sendo.necessária.assistência" = 'Amount of work'),
 Category= recode(Category,"Não.se.aplica.y.y" = 'Not applicable'),
 Category= recode(Category,"Prefiro.não.responder.x.x.x.x" = 'Didnt answer'))

data_44sum$Group_gender <- factor(data_44sum$Group_gender, levels = c('Women', 'Men'))

data_44sum$Category <- factor(data_44sum$Category, levels = c('Didnt answer', 'Not applicable','Company', "Educational experience", 'Worried about personal security', 'Amount of work'))

### Q63 children and field work ----
data_geral_63 <- data_matrix %>% 
 gather('Category', 'Q_63_Answer', 87:94) %>% 
 select('ID', 'Group_gender', 'Group_time2', 'Category', 'Q_63_Answer')

data_63sum <- data_geral_63 %>% 
 group_by(Category, Group_gender, Group_time2) %>% 
 mutate(Q_63_Answer = as.double(Q_63_Answer)) %>% 
 summarise(Sum = sum(Q_63_Answer))

data_63sum = data_63sum %>% mutate(
 Category= recode(Category, "Companheiro.a..ou.cônjuge.cuida.da.s..criança.s..em.casa" = "Partner takes care of children at home"),
 Category= recode(Category, "Companheiro.a..ou.cônjuge.cuida.da.s..criança.s..no.campo" = "Partner takes care of children in the field"), 
 Category= recode(Category, "Enviei.a.s..criança.s..para.um.a..amigo.a..ou.alguém.da.minha.família.cuidar." = "I sent the children to a friend or family member"),
 Category= recode(Category, "Contratei.alguém.para.cuidar.da.s..criança.s..em.casa.ou.em.outro.lugar.que.não.seja.o.campo" = "I hired someone to take care of the children"),
 Category= recode(Category,"Não.tenho.filhos" = "I don't have children"),
 Category= recode(Category,"Trouxe.ou.contratei.alguém.para.cuidar.da.s..criança.s..no.campo" = "I brought/hired someone to take care of the children in the field"),
 Category= recode(Category,"Não.se.aplica" = 'Not applicable'),
 Category= recode(Category,"Prefiro.não.responder.y.y.y.y" = 'Didnt answer'))

data_63sum$Group_gender <- factor(data_63sum$Group_gender, levels = c('Women', 'Men'))

data_63sum$Category <- factor(data_63sum$Category, levels = c('Didnt answer', 'Not applicable',"I don't have children", "Partner takes care of children at home", "Partner takes care of children in the field", "I sent the children to a friend or family member", "I hired someone to take care of the children","I brought/hired someone to take care of the children in the field"))

### Join multiple-choice data ----
data_geral_19sum$Question <- "Q14"
data_geral_20sum$Question <- "Q15"
data_geral_21sum$Question <- "Q16"
data_geral_22sum$Question <- "Q17"
data_geral_24sum$Question <- "Q19"
data_35sum$Question <- "Q28"
data_36sum$Question <- "Q29"
data_38sum$Question <- "Q31"
data_40sum$Question <- "Q33"
data_43sum$Question <- "Q36"
data_44sum$Question <- "Q37"
data_63sum$Question <- "Q44"

Join_Sum_Q <- data_geral_19sum %>%
 full_join(data_geral_20sum) %>%
  full_join(data_geral_21sum) %>%
  full_join(data_geral_22sum) %>%
  full_join(data_geral_24sum) %>%
  full_join(data_35sum) %>% 
 full_join(data_36sum) %>% 
 full_join(data_38sum) %>% 
 full_join(data_40sum) %>% 
 full_join(data_43sum) %>% 
 full_join(data_44sum) %>% 
  full_join(data_63sum)

# Translating and cleaning data of single-choice questions ----
## Q4 identity ----
data_geral_af = data_geral_af %>% 
 mutate(`Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Amarela' = "Asian"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Branca' = "White"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Parda' = "Mixed"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Preta' = "Black"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Indígena' = "Indigenous"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Prefiro não me classificar' = "Not identify"),
   `Q4_Como você se identifica?`= recode(`Q4_Como você se identifica?`, 'Prefiro não responder' = "Not identify"))

unique(data_geral_af$`Q4_Como você se identifica?`)

## Q5 Age ----

data_geral_af = data_geral_af %>% 
 mutate(`Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Acima de 60 anos' = "> 60"),
   `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 20 a 29 anos' = "20-29"),
   `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 30 a 39 anos' = "30-39"),
   `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 40 a 49 anos' = "40-49"),
   `Q5_Qual é a sua idade?`= recode(`Q5_Qual é a sua idade?`, 'Entre 50 a 59 anos' = "50-59"))

unique(data_geral_af$`Q5_Qual é a sua idade?`)

## Q9 and Q13 state of birth and currently state ----
data_geral_af<- data_geral_af %>% 
 mutate(`Q9_Qualéoseuestadodeorigem?` = 
          recode(`Q9_Qualéoseuestadodeorigem?`, 
                 "Minasgerais" = "Minas Gerais",
                 "Sãopaulo" = "São Paulo",
                 "Distritofederal" = "Distrito Federal",
                 "Riodejaneiro" = "Rio De Janeiro",
                 "Riograndedosul" = "Rio Grande Do Sul",
                 "Santacatarina" = "Santa Catarina",
                 .default = as.character(`Q9_Qualéoseuestadodeorigem?`)))

unique(data_geral_af$`Q9_Qualéoseuestadodeorigem?`)

data_geral_af<- data_geral_af %>% 
 mutate(`Q13_Emqualestadovocêresideatualmente?` = 
          recode(`Q13_Emqualestadovocêresideatualmente?`,
                 "Minasgerais" = "Minas Gerais",
                 "Sãopaulo" = "São Paulo",
                 "Distritofederal" = "Distrito Federal",
                 "Riodejaneiro" = "Rio De Janeiro",
                 "Riograndedosul" = "Rio Grande Do Sul",
                 "Santacatarina" = "Santa Catarina",
                 "Matogrosso" = "Mato Grosso",
                 .default = as.character(`Q13_Emqualestadovocêresideatualmente?`)))

unique(data_geral_af$`Q13_Emqualestadovocêresideatualmente?`)

## Q28 income ----
data_geral_af<- data_geral_af %>% 
 mutate(`Q28_Qual é a sua renda mensal?` = 
          recode(`Q28_Qual é a sua renda mensal?`,
                 "1 a 2 salários mínimos" = "1-2 minimum salary",
                 "2 a 3 salários mínimos" = "2-3 minimum salary",
                 "Mais que 10 salários mínimos" = ">10 minimum salary",
                 "Mais que 5 salários mínimos" = ">5 minimum salary",
                 "3 a 5 salários mínimos" = "3-5 minimum salary",
                 "Prefiro não responder" = "Didnt answer",
                 .default = as.character(`Q28_Qual é a sua renda mensal?`)))

unique(data_geral_af$`Q28_Qual é a sua renda mensal?`)

## Q31 more than one job ----
data_geral_af = data_geral_af %>% 
 mutate(`Q31_Você possui mais de uma fonte de renda?`= 
          recode(`Q31_Você possui mais de uma fonte de renda?`,
                 'Sim' = "Yes", 
                 'Não' = "No",
                 "Prefiro não responder" = "Didnt answer",
                 .default = as.character(`Q31_Você possui mais de uma fonte de renda?`)))

unique(data_geral_af$`Q31_Você possui mais de uma fonte de renda?`)

## Q33 team preference ----
data_geral_af = data_geral_af %>% 
 mutate(`Q33_Você prefere trabalhar em uma equipe:` = 
          recode(`Q33_Você prefere trabalhar em uma equipe:`,
                 'Primordialmente feminina' = "Primordially female",
                 'Primordialmente masculina' = "Primordially male",
                 'Indiferente' = "Indifferent",
                 "Prefiro não responder" = "Didnt answer",
                 .default = as.character(`Q33_Você prefere trabalhar em uma equipe:`)))

unique(data_geral_af$`Q33_Você prefere trabalhar em uma equipe:`)

## Q34 collegues ----
data_geral_af = data_geral_af %>% 
 mutate(`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:` = 
          recode(`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`, 'Mulheres' = "Women",
                  'Homens' = "Men",
                  'Ambos' = "Both",
                  "Prefiro não responder" = "Didnt answer",
                  .default = as.character(`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`))) 

unique(data_geral_af$`Q34_Colegas que mais auxiliam em sua posição atual são principalmente:`)

## Q39 % sexual harassment ----
data_geral_af = data_geral_af %>% 
 mutate(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`= 
          recode(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`, 'Sim' = "Yes",
                 'Não' = "No",
                 'Prefiro não responder' = "Didnt answer",
                 .default = as.character(`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`))) 

unique(data_geral_af$`Question_39_Você já sofreu assédio sexual por algum colega de trabalho?`)

## Q42  postgraduate program ----
data_geral_af = data_geral_af %>% 
  mutate(`Q42_Se você cursou pós-graduação, marque, entre as seguintes afirmações, aquela que melhor descreve o papel da pessoa que te orientou em sua carreira:` = 
           recode(`Q42_Se você cursou pós-graduação, marque, entre as seguintes afirmações, aquela que melhor descreve o papel da pessoa que te orientou em sua carreira:`, 
                  'A pessoa contribuiu de maneira significativa para o meu sucesso' = "The person made a significant contribution to my success",
                  'A pessoa fez uma pequena contribuição para o meu sucesso' = "The person made a small contribution to my success",
                  'A pessoa não afetou meu sucesso' = "The person didnt affect  my success",
                  'A pessoa representou um impedimento significativo para o meu sucesso' = "The person represented a significant impediment to my success",
                  'A pessoa representou um pequeno impedimento para o meu sucesso' = "The person represented a small impediment to my success",
                  'Prefiro não responder' = "Didnt answer",
                  .default = as.character(`Q42_Se você cursou pós-graduação, marque, entre as seguintes afirmações, aquela que melhor descreve o papel da pessoa que te orientou em sua carreira:`))) 

## Q57 - scientific production ----
data_geral_af <- data_geral_af %>% 
 mutate(`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?` =
          recode(`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`, "2-2.9" = "1-3.9",
                 "3-3.9"= "1-3.9",
                 "1-1.9" = "1-3.9",
                 "Não se aplica" = "Didnt answer", 
                 .default = as.character(`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`)))

unique(data_geral_af$`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`)

data_geral_af$`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?` <- factor(data_geral_af$`Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?`)

## Q58 - Have children ----
data_geral_af = data_geral_af %>% 
  mutate(`Q58_Você tem filhos?`= 
           recode(`Q58_Você tem filhos?`,
                  'Sim' = "Yes",
                  'Não' = "No",
                  .default = as.character(`Q58_Você tem filhos?`)))

unique(data_geral_af$`Q58_Você tem filhos?`)

## Q60 - desire to have children ----
data_geral_af = data_geral_af %>% 
 mutate(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`= 
          recode(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`, 'Sim' = "Yes",
                 'Não' = "No",
                 'Prefiro não responder' = "Didnt answer",
                 .default = as.character(`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`)))

unique(data_geral_af$`Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?`)

## Q64 How do you see their career ----

data_geral_af = data_geral_af %>% 
 mutate(`Q64_Como você classifica a sua vida pessoal neste momento?`= 
          recode(`Q64_Como você classifica a sua vida pessoal neste momento?`,
                 'Bem sucedida' = "Sucessfull",
                 'Muito bem sucedida' = "Very sucessfull",
                 'Muito sem sucesso' = "Very unsucessfull",
                 'Sem sucesso' = "Unsucessfull",
                 'Prefiro não responder' = "Didnt answer",
                 .default = as.character(`Q64_Como você classifica a sua vida pessoal neste momento?`)))

unique(data_geral_af$`Q64_Como você classifica a sua vida pessoal neste momento?`)

## Q- 45 a 55 - Productivity and Career Factors ----
### Summarize data of each factor ----
output_Q45 = data_geral_af %>% 
 group_by(data_geral_af$`Q45_Como os fatores listados influenciam a sua produtividade científica? [Idade]`, Group_gender, Group_time2) %>% 
 summarize("Age" = n()) 
 
output_Q46 = data_geral_af %>% 
 group_by(data_geral_af$`Q46_Como os fatores listados influenciam a sua produtividade científica? [Raça/Etnia]`, Group_gender, Group_time2) %>% 
 summarize("Ethnicity" = n()) 
 
output_Q47 = data_geral_af %>% 
 group_by(`Q47_Como os fatores listados influenciam a sua produtividade científica? [Nível Socioeconômico]`, Group_gender, Group_time2) %>% 
 summarize("Socioeconomic level" = n()) 
 
output_Q48 = data_geral_af %>% 
 group_by(`Q48_Como os fatores listados influenciam a sua produtividade científica? [Origem geográfica]`, Group_gender, Group_time2) %>% 
 summarize("Geographical origin" = n()) 
 
output_Q49 = data_geral_af %>% 
 group_by(`Q49_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades familiares]`, Group_gender, Group_time2) %>% 
 summarize("Family responsibilities" = n()) 
 
output_Q50 = data_geral_af %>% 
 group_by(`Q50_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades com ensino]`, Group_gender, Group_time2) %>% 
 summarize("Teaching responsibilities" = n()) 
 
output_Q51 = data_geral_af %>% 
 group_by(`Q51_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades administrativas]`, Group_gender, Group_time2) %>% 
 summarize("Administrative responsibilities" = n())
 
output_Q52 = data_geral_af %>% 
 group_by(`Q52_Como os fatores listados influenciam a sua produtividade científica? [Falta de segurança no trabalho]`, Group_gender, Group_time2) %>% 
 summarize("Lack of job security" = n()) 

output_Q53 = data_geral_af %>% 
 group_by(`Q53_Como os fatores listados influenciam a sua produtividade científica? [Falta de financiamento]`, Group_gender, Group_time2) %>% 
 summarize("Lack of funding" = n()) 
 
output_Q54 = data_geral_af %>% 
 group_by(`Q54_Como os fatores listados influenciam a sua produtividade científica? [Falta de recursos para fazer meu trabalho (por exemplo, ajuda de escritório/escritório, laboratório, equipamentos, infraestrutura, etc.)]`, Group_gender, Group_time2) %>% 
 summarize("Lack of resources" = n()) 

output_Q55 = data_geral_af %>% 
 group_by(`Q55_Como os fatores listados influenciam a sua produtividade científica? [Discriminação de sexo/gênero]`, Group_gender, Group_time2) %>%
 summarize("Gender discrimination" = n()) 
 
### Name new collum ----
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

### Join data ----
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

Q_45_55_unite= Q_45_55 %>% gather("Type", "Count", 4:14)
Q_45_55_unite$Count[is.na(Q_45_55_unite$Count)]<-0

Q_45_55_unite2$Type<-as.character(Q_45_55_unite2$Type)
Q_45_55_unite2$Influence<-as.factor(Q_45_55_unite2$Influence)
Q_45_55_unite2$Count<-as.integer(Q_45_55_unite2$Count)

### Translate factor data ----
Q_38_Influence = Q_45_55_unite %>% 
 mutate(Influence = recode(Influence, 'Neutro' = "Neutral/No impact", 'Impedimento' = "Impediment", 'Não impactou' = "Neutral/No impact", 'Acelerador'= "Accelerator", 'Grande Acelerador'= "Big Accelerator", 'Grande impedimento'= "Big impediment")) %>%
 ungroup()

Q_38_Influence$Influence <- factor(Q_45_55_unite2$Influence, levels = c("Big Accelerator", "Accelerator", "Neutral/No impact", "Impediment","Big impediment"))

Q_38_Influence$Type <- factor(Q_45_55_unite2$Type, levels = c("Ethnicity", "Age", "Geographical origin", "Teaching responsibilities", "Administrative responsibilities", "Gender discrimination", "Lack of job security", "Socioeconomic level", "Family responsibilities", "Lack of resources", "Lack of funding"))

## Clean data and rename columns ----

data_cleaned <- data_geral_af %>% 
  select(- c("Carimbo de data/hora", "Carimbo de data", "Q6_Qual é a sua idade?", "Q8_Qualéoseuestadodeorigem?", "Q12_Emqualestadovocêresideatualmente?","Q27_Há quanto tempo você está na situação acima?",  "Q29_Qual é a sua renda mensal? (em real)" , "Q36_Há mais homens que mulheres em meu departamento":"Q36_Não se aplica", "Q38_Algum homem já levou o crédito pelo trabalho feito por você":"Q38_Prefiro não responder", "Q40_A situação foi exposta e a pessoa sofreu as devidas consequências":"Q40_Não se aplica", "Q43_Não se aplica":"Q43_Prefiro não responder", "Q56_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?", "Group_time", "case_when(...)")) %>% 
  dplyr::rename("COD_ID" = "ID",
                "Q4_PS" = "Q3_Qual a sua identidade de gênero?",
                "Q5_PS" = "Q4_Como você se identifica?",
                "Q6_PS"  = "Q5_Qual é a sua idade?",
                "Q7_PS"  = "Q7_Qualéoseupaísdeorigem?",
                "Q8_PS"  = "Q9_Qualéoseuestadodeorigem?",
                "Q9_PS"  = "Q10_Qualéoseumunicípio/cidadedeorigem?",
                "Q10_PS"  = "Q11_Emqualpaísquevocêresideatualmente?",
                "Q11_PS"  = "Q13_Emqualestadovocêresideatualmente?", 
                "Q12_PS"  = "Q14_Emqualmunicípio/cidadevocêresideatualmente?",
                "Q7_Q10_PS"  = "Q15_Mudou de país",
                "Q8_Q11_PS"  = "Q16_Mudou de estado (mesmo país)",
                "Q9_Q12_PS"  = "Q17_Mudou de cidade (mesmo estado/mesmo país)",
                "Q13_DC"  = "Q18_Você mora na mesma cidade onde está localizada a instituição/empresa a qual você está vinculada (o/e) atualmente?",
                "Q14_DC"  = "Question_19_Se você se mudou para uma nova cidade, estado ou país nos últimos cinco anos, quais foram os fatores motivadores? (Marque todas as opções que se aplicam)",                                              
                "Q15_DC" = "Question_20_Se você já mudou de residência com o objetivo de progredir em sua carreira verifique as categorias que descrevem as consequências de seus movimentos. (Marque todas as opções que se aplicam)",          
                "Q16_DC" = "Question_21_Como/com quem você mora? (Marque todas as opções que se aplicam)",                                                             "Q17_DC" = "Question_22_Quem faz as tarefas domésticas semanais (por exemplo, cozinhar, limpar, lavar roupa) em sua casa? (Marque todas as opções que se aplicam)",
                "Q18_EI" = "Q23_Em qual curso de graduação você se formou?",
                "Q19_EI" = "Question_24_Qual a sua formação na área de ecologia?",
                "Q20_EI" = "Q25_Qual categoria melhor descreve a sua situação atual?",
                "Q21_EI" = "Q26_Há quanto tempo você está na situação acima?",
                "Q22_EI" = "Q28_Qual é a sua renda mensal?",
                "Q23_EI" = "Q30_Qual é a melhor descrição da instituição onde você trabalha atualmente?",
                "Q24_EI" = "Q31_Você possui mais de uma fonte de renda?",
                "Q25_EI" = "Q32_Se respondeu SIM na pergunta anterior, qual outro trabalho você desempenha?",
                "Q26_WE" = "Q33_Você prefere trabalhar em uma equipe:",
                "Q27_WE" = "Q34_Colegas que mais auxiliam em sua posição atual são principalmente:",
                "Q28_WE" = "Question_35_Em seu trabalho atual, você observa quais das situações abaixo? (Marque todas as opções que se aplicam)",
                "Q29_WE" = "Question_36_Caso você atue em alguma instituição pública, você observa quais das situações abaixo? (Marque todas as opções que se aplicam)",
                "Q30_WE"  = "Q37_Questões de desigualdade de gênero já foram ou são debatidas na sua instituição?",
                "Q31_WE" = "Question_38_Você já passou por algumas destas situações em sua trajetória profissional?(Marque todas as opções que se aplicam)",
                "Q32_WE" = "Question_39_Você já sofreu assédio sexual por algum colega de trabalho?",
                "Q33_WE" = "Question_40_Se você respondeu SIM na pergunta anterior e se sentir à vontade para responder, indique quais situações se aplicam ao ocorrido: (Marque todas as opções que se aplicam)",
                "Q34_GP" = "Q41_Se você fez pós-graduação (ex: especialização, mestrado ou doutorado), a pessoa que te orientou por último foi:",
                "Q35_AP" = "Q42_Se você cursou pós-graduação, marque, entre as seguintes afirmações, aquela que melhor descreve o papel da pessoa que te orientou em sua carreira:",
                "Q36_GP" = "Question_43_Se você não está mais no meio acadêmico, quais foram os motivos que a/o levaram a sair? (Marque todas as opções que se aplicam)",
                "Q37_GP"  = "Question_44_Se você já realizou trabalho de campo e trouxe alguém com você como assistente de campo, identifique a razão específica para fazê-lo. (Marque todas as opções que se aplicam)",
                "Q38_AGE_GP" = "Q45_Como os fatores listados influenciam a sua produtividade científica? [Idade]",
                "Q38_RACE_GP" = "Q46_Como os fatores listados influenciam a sua produtividade científica? [Raça/Etnia]",
                "Q38_sOCIO_GP" = "Q47_Como os fatores listados influenciam a sua produtividade científica? [Nível Socioeconômico]",
                "Q38_GEO_GP" = "Q48_Como os fatores listados influenciam a sua produtividade científica? [Origem geográfica]",
                "Q38_FAM_GP" = "Q49_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades familiares]",
                "Q38_TEAC_GP" = "Q50_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades com ensino]",
                "Q38_ADM_GP" = "Q51_Como os fatores listados influenciam a sua produtividade científica? [Responsabilidades administrativas]",
                "Q38_JOB_GP" = "Q52_Como os fatores listados influenciam a sua produtividade científica? [Falta de segurança no trabalho]",
                "Q38_FUND_GP" = "Q53_Como os fatores listados influenciam a sua produtividade científica? [Falta de financiamento]",
                "Q38_RESOURCE_GP" = "Q54_Como os fatores listados influenciam a sua produtividade científica? [Falta de recursos para fazer meu trabalho (por exemplo, ajuda de escritório/escritório, laboratório, equipamentos, infraestrutura, etc.)]",
                "Q38_GENDER_GP" = "Q55_Como os fatores listados influenciam a sua produtividade científica? [Discriminação de sexo/gênero]",
                "Q39_GP" = "Q57_Qual é a sua MÉDIA/ANO de produção científica (artigos, capítulos de livros, livros), nos últimos 4 anos, ou seja, de 2019 até o momento?",
                "Q40_P"  = "Q58_Você tem filhos?",
                "Q41_P" = "Q59_Se você respondeu SIM na pergunta anterior, especifique quantos filhos você tem.",
                "Q42_P" = "Q60_Sua escolha profissional influencia a sua vontade de ter filhos(as)?",
                "Q43_P" = "Q61_Você acha que a licença maternidade pode impactar negativamente na profissão das mulheres?",
                "Q44_P" = "Q63_Se você tem filhos e realizou trabalho de campo, como você equilibrou o trabalho de campo com cuidado parental? (Marque todas as opções que se aplicam)",
                "Q45_S" = "Q64_Como você classifica a sua vida pessoal neste momento?",
                "Q46_S" = "Q64_Você vê uma carreira de sucesso como profissional no ramo da Ecologia?",
                "GENDER_CAT" = "Group_gender",
                "ACD_OCC" = "Group_occupation",
                "C_STAGE_CAT" = "Group_time2"
  )
str(data_cleaned)

write.csv(data_cleaned,"./Data/data_cleaned.csv")

# Prepare single-choice data for analysis ----

output_Q26 <- data_cleaned %>% 
  group_by(Q26_WE, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q26_WE != "Didnt answer") %>% 
 rename("Category" = Q26_WE)

output_Q27 = data_cleaned %>% 
  group_by(Q27_WE, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q27_WE != "Didnt answer") %>%
  rename("Category" = Q27_WE)

output_Q32 <- data_cleaned %>% 
 group_by(Q32_WE, GENDER_CAT, C_STAGE_CAT) %>% 
 summarize(count = n(), .groups = "drop") %>%
 filter(Q32_WE != "Didnt answer") %>%
 rename("Category" = Q32_WE) 

output_Q34 <- data_cleaned %>% 
  group_by(Q34_AP, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q34_AP != "Didnt answer") %>%
  rename("Category" = Q34_AP) 

output_Q39 <- data_cleaned %>% 
  group_by(Q39_AP, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q39_AP != "Didnt answer") %>%
  rename("Category" = Q39_AP) 

output_Q40 <- data_cleaned %>% 
  group_by(Q40_P, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q40_P != "Didnt answer") %>%
  rename("Category" = Q40_P) 

output_Q42 <- data_cleaned %>% 
  group_by(Q42_P, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q42_P != "Didnt answer") %>%
  rename("Category" = Q42_P) 

output_Q43 <- data_cleaned %>% 
  group_by(Q43_P, GENDER_CAT, C_STAGE_CAT) %>% 
  summarize(count = n(), .groups = "drop") %>%
  filter(Q43_P != "Didnt answer") %>%
  rename("Category" = Q43_P) 

output_Q26$Question <- "Q26"
output_Q27$Question <- "Q27"
output_Q32$Question <- "Q32"
output_Q34$Question <- "Q34"
output_Q39$Question <- "Q39"
output_Q40$Question <- "Q40"
output_Q42$Question <- "Q42"
output_Q43$Question <- "Q43"

Join_Count_Q <- output_Q26 %>% 
 full_join(output_Q27) %>% 
 full_join(output_Q32) %>% 
 full_join(output_Q34) %>% 
 full_join(output_Q39) %>%
 full_join(output_Q40) %>% 
 full_join(output_Q42) %>% 
 full_join(output_Q43)

write.csv(Join_Count_Q, "./Data/Join_Count_Q.csv")
write.csv(Join_Sum_Q, "./Data/Join_Sum_Q.csv") 
write.csv(Q_38_Influence,"./Data/Q_38_Influence.csv") 
