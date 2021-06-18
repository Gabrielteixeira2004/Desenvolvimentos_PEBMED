#Bibliotecas

require(dplyr)
require(lubridate)

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)


#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1A7JIMxzUThFdB4klamkT6Cm1dP-rQ91Dc0AhxT99bH0/edit#gid=55058477", sheet="CLT ativos", skip=1, col_types = 'c')

df2 <- df %>%
  select("NOME", "CLT/ESTAG", "ADMISSÃO ESTAG", "ADMISSÃO CLT", "SALÁRIO BRUTO", "FUNÇÃO", "ÁREA", "CENTRO DE CUSTO", "CPF","DN",  "IDADE", "SEXO", "COR/RAÇA", "GRAU DE ESCOLARIDADE", "MATRÍCULA", "LÍDER") %>% 
  mutate(na_if(.,"-"),
         DESLIGAMENTO = '31/12/2099',
         ÁREA = ifelse(is.na(ÁREA),'SEM ÁREA',ÁREA)
         )

# Alterar tipo de coluna

listadt <- c("DN", "ADMISSÃO ESTAG", "ADMISSÃO CLT")

df2[listadt] <- lapply(df2[listadt], dmy)


#Renomear Colunas
nomes_antigos <- c("CLT/ESTAG","DN")

nomes_novos <- c("TIPO COLABORADOR","DATA DE NASCIMENTO")

df2 <- df2 %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)
