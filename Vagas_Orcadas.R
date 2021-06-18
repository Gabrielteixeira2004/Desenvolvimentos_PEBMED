#Bibliotecas
require(dplyr)

#Autenticacao
segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)


converte <- function(x) as.numeric(sub(",", "\\.", gsub("R\\$|\\.", "", x)))

#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h-U1JOLaMijRuP6_6RjpZDIAUEgPcIKQekASy4HLxoQ/edit?usp=drive_web&ouid=114871898180865383340", sheet = "VAGAS NO ORÇAMENTO", col_types = 'c')

#Tirar linhas vazias e Orcado de Tecnologia

df2 <- df %>%
  filter(is.na(STATUS),
         !is.na(`Nome da Vaga Ocupada`),
         Área != 'TECNOLOGIA') %>%
  select('Área', 'Nome da Vaga Ocupada', 'Cargo no Orçamento da Afya', 'PREVISÃO DE ENTRADA', 'Nome do Profissional', 'Motivo da abertura da Vaga', 'Projeto') %>%
  rename_all(toupper) %>% 
  mutate_if(is.character, toupper)
