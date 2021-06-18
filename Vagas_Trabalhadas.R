#Bibliotecas
require(dplyr)

#Autenticacao

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)


#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h-U1JOLaMijRuP6_6RjpZDIAUEgPcIKQekASy4HLxoQ/edit?usp=drive_web&ouid=114871898180865383340", sheet = "CONTROLE VAGAS (PEBMED)", skip=1, col_types = 'c')

col_selec <- c('STATUS', 'NOME DA VAGA OCUPADA', 'MOTIVO DA VAGA', 'DATA DE RECEBIMENTO DO FORMS', 'DATA DE ABERTURA NA GUPY', 'DIAS ATÉ INÍCIO DO PROCESSO', 'FUNÇÃO', 'EXPECTATIVA DE CONTRATAÇÃO', 'CARGO', 'LOCAL', 'MODELO', 'NÍVEL DE DIFICULDADE', 'RESPONSÁVEL', 'ÁREA', 'PROJETO', 'GESTOR(A) RESPONSÁVEL', 'ETAPA', 'LINK DA VAGA DIVULGADA', 'TEMPO DA VAGA BLOQUEADA', 'TEMPO TOTAL DA VAGA EM ABERTO', 'DATA DE FECHAMENTO', 'DATA DE ADMISSÃO', 'MÊS DE ADMISSÃO', 'QUARTER VAGA FECHADA', 'QUARTER \nDE INÍCIO', 'NOME COMPLETO DO CANDIDATO APROVADO', 'OBSERVAÇÕES GERAIS', 'OBSERVAÇÕES PARA LÍDERES')

#Tirar linhas vazias (Coluna Status), criar flag de Tecnologia e substituir ocorrencias de "-"

df2 <- df %>%
  select(all_of(col_selec)) %>% 
  filter(!is.na(STATUS)) %>% 
  mutate(flag_tech = ifelse(ÁREA == 'TECNOLOGIA',1,0)) %>%
  na_if("-")
