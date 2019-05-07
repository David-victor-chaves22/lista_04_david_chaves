#####################################################################################
#####################################################################################
#####################################################################################
#############                                                           #############
#############           UNIVERSIDADE FEDERAL DE PERNAMBUCO              ############# 
############# PROGRAMA DE POS-GRADUCAO EM CIENCIA POLITICA - MESTRADO   ############# 
#############                 RECIFE, 07 DE MAIO DE 2017                ############# 
#############          DISCENTE: DAVID VICTOR DE MELO CHAVES            ############# 
#############                 DOCENTE: DAVI MOREIRA                     ############# 
#############                                                           ############# 
#####################################################################################
#####################################################################################
#####################################################################################

##### QUESTAO 01 #####

#LINK: https://github.com/David-victor-chaves22/lista_04_david_chaves

##### QUESTAO 02 #####


install.packages("magrittr")
# Instalar o pacote magittr

install.packages("readxl")
# primeiramente fiz a instalacao do pacote readxl para carregar os dados do PNUD

install.packages ("GGally")


library(magrittr)
library(tidyverse)
library(GGally)
library(ffbase)
library(readr)
library(readxl)
library(rlang)
# solicita pacotes

setwd ("C:/Users/David/Desktop/Mestrado/2019_1/analise_de_dados/dados_encontro_2_ufpe/dados_encontro_2_ufpe")
# diretorio onde a base esta salva 

getwd()
# direciona a base ao diretorio

load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

# abrindo banco do censo 2016 no banco de dados do Encontro 2

#########################
getwd()
# tranformar a base

pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)
# para conhecer a base

head(pnud)

unique(pnud$ANO)

pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)
# selecionar os dados de 2010 em PE

rm(pnud)  
# removendo base pnud


turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_turmas = n(), 
            turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

# processamento da base de dados TURMAS


dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))

summary(escolas_pe_sel)
# verificando as bases

docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(),
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))

summary(docentes_pe_sel)
# verificando docentes

matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(), 
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T)
  )

# verificando matriculas


dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))

summary(matriculas_pe_sel)

# verificando a base 


## Unindo a base de dados do censo e pnud: 

# matriculas

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO")
)
# para unir a base do censo e do penud 

dim(pnud_pe_2010)

dim(matriculas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)
# verificando a base

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

dim(escolas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# abrir a parte referente a escola



censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

dim(turmas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)
# abrir a parte referente a escola 


censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

dim(docentes_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)
# abrir a parte referente a docentes

## salvando a nova base de dados

setwd()

getwd()

dir()

save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")

write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)

rm(list = ls())  # limpando area de trabalho

# salvando a nova base 

getwd()

# abrindo a base de dados:

load("2016_censo_pnud_pe_sel.RData")

# para abrir a nova base de dados e 
# observando  o caminho da base de dados:


dim(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel)

head(censo_pnud_pe_sel)
# verificando a base

################### pontos ######################## 

# ponto 01: Não deve haver docente com mais de 70 anos ou com menos de 18 anos 

summary(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel$alunos_media_idade)

load("docentes_pe_censo_escolar_2016.RData")

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)

dim(docentes_pe_selecao)


# ponto 02: Não deve haver aluno com mais de 25 anos ou com menos de 1 ano 

load("matricula_pe_censo_escolar_2016.RData")

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

dim(matricula_pe_selecao)

summary(matricula_pe_selecao$NU_IDADE)

# ponto 03: Apresente estatísticas descritivas do número de alunos por docente nos municípios do Estado ##

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

DocentesAlunos <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes

# estatíitcas dos Docentes por alunos

DocentesAlunos

summary(DocentesAlunos)

plot(DocentesAlunos)

ggplot(censo_pnud_pe_sel, aes(DocentesAlunos))+geom_histogram()


# ponto 04: Apresente o município com maior número de alunos por docente e seu IDHM #

names(censo_pnud_pe_sel)

summary(DocentesAlunos)

# para juntar as variáveis

censo_pnud_pe_sel_docentesalunos <- censo_pnud_pe_sel %>%  mutate(DocentesAlunos)

View(censo_pnud_pe_sel_docentesalunos)

censo_pnud_pe_sel_docentesalunos["177", ]


# Tupanatinga é o muninicio com maior numero de aluno por docente

plot(censo_pnud_pe_sel_docentesalunos$DocentesAlunos)

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos))+geom_histogram()




#### Questão 2f ####
## Faça o teste do coeficiente de correlação linear de pearson e apresente sua resposta ##

cor(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

cor.test(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)


# correlação negativa, não tem relação estatisticamente positiva entre as variaveis. 

##### QUESTAO 03 #####

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos, IDHM, color = IDHM))+geom_point()


