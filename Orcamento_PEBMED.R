#Bibliotecas
require(dplyr)

#Autenticacao
segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)

converte <- function(x) as.numeric(sub("-","",sub(",", "\\.", gsub("R\\$|\\.", "", x))))

#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_hVFAQuQpRAfOhs0nwTc7WyZkwFC05jCMSrH-3ID5_U/edit#gid=881005633", sheet = "Orçamento_Plano_HC_PROJEÇÃO", skip=1, col_types = 'c')

df2 <- df %>%
  select(1:32) %>% 
  mutate(na_if(.,"0,00")) %>% 
  tidyr::pivot_longer(cols = 21:32, names_to = 'Atributo', values_to = 'Valor', values_drop_na = TRUE) %>%
  mutate(Valor = converte(Valor))



#Renomear Colunas
nomes_antigos <- c("Cód. do centro de custo","Descrição do centro de custo","Funcionário ou Nome da Vaga","Descrição do cargo","Descrição da unidade")

nomes_novos <- c("Cód. CC","DESCRICAO CC","Colaborador","Cargo","EMPRESA")

df2 <- df2 %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)
