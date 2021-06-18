#Bibliotecas
require(dplyr)

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)

converte <- function(x) as.numeric(sub(",", "\\.", gsub("R\\$|\\.", "", x)))

#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Ds0a7jn8NkP8Y-G_KtHij-72KTZWvHeE28agt0D690M/edit#gid=2002813053", col_types = 'c')

df2 <- df %>%
  select(1:7, 9:20) %>%
  filter(!is.na(df["Centro de Custo"])) %>% 
  tidyr::pivot_longer( cols = 8:19, names_to = 'Competência', values_to = 'Valor', values_drop_na = TRUE) %>% 
  mutate(Competência = gsub(".", "",Competência,fixed = TRUE ),
         Valor = converte(Valor),
         Evento = 'Salário',
         EMPRESA = 'iClinic',
         Colaborador = replace(Colaborador, is.na(Colaborador), "SEM NOME") 
  )



#Renomear Colunas
nomes_antigos <- c("Centro de Custo")

nomes_novos <- c("DESCRICAO CC")

df2 <- df2 %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)

df2 <- df2 %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)
