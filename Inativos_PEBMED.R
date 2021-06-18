#Bibliotecas
require(dplyr)
require(lubridate)

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)

converte <- function(x) as.numeric(sub("-","",sub(",", "\\.", gsub("R\\$|\\.", "", x))))


#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1A7JIMxzUThFdB4klamkT6Cm1dP-rQ91Dc0AhxT99bH0/edit#gid=55058477", sheet="Inativos",col_types = 'c')

#Renomear Colunas
nomes_antigos <- c("CLT/PJ/RPA","Salário Bruto", "Idade")

nomes_novos <- c("TIPO COLABORADOR","SALÁRIO BRUTO", "IDADE")

#Transformação

df2 <- df %>%
  select("NOME", "CLT/PJ/RPA", "ADMISSÃO PJ", "ADMISSÃO CLT","DESLIGAMENTO", "Salário Bruto", "FUNÇÃO", "ÁREA", "CENTRO DE CUSTO", "CPF","DN",  "Idade", "SEXO", "COR/RAÇA", "GRAU DE ESCOLARIDADE") %>%
  mutate(na_if(.,"-"),
         DESLIGAMENTO = as.Date(dmy(DESLIGAMENTO)),
         `Salário Bruto` = converte(`Salário Bruto`),
         Idade = converte(Idade)) %>% 
  filter(`CLT/PJ/RPA` != "RPA",
         DESLIGAMENTO >= as.Date('2021-01-01')) %>% 
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)


#Alterar tipo

listadt <- c("ADMISSÃO PJ","ADMISSÃO CLT", "DN")

df2[listadt] <- lapply(df2[listadt], dmy)
