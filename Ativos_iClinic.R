#Bibliotecas
require(dplyr)
require(lubridate)

#Autenticacao

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)

#Funcoes
converte <- function(x) as.numeric(sub("-","",sub(",", "\\.", gsub("R\\$|\\.", "", x))))

#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1YhKvU9fw1H0nDwbvvlHkJoNQktSmRi9Qw30jDF-91oc/edit#gid=0", sheet="Dados Gerais - Ativos", col_types = 'c')

#Renomear Colunas
nomes_antigos <- c("Equipe","Nascimento","Escolaridade","Cargo","Data Oficial")

nomes_novos <- c("ÁREA","DATA DE NASCIMENTO","GRAU DE ESCOLARIDADE","FUNÇÃO","ADMISSÃO")


#Tratamento

df2 <- df %>%
  select("Nome", "Matrícula", "Sexo", "Regime", "Equipe", "Time", "Cargo", "Nascimento", "Idade", "CPF", "Salário",  "Data Oficial",  "Escolaridade", "Tempo de casa") %>%
  filter(!is.na(df$Cargo)) %>% 
  tidyr::separate(Regime, c('Regime.1','Regime.2')," - ") %>% 
  mutate(Salário = converte(Salário),
         DESLIGAMENTO = dmy('31/12/2099'),
         Equipe = tidyr::replace_na(Equipe, 'SEM ÁREA'),
         EMPRESA = 'iClinic',
         'TIPO COLABORADOR' = ifelse(Regime.1 == "CC", "Diretor Estatutário", Regime.1),
         'TIPO TRABALHO' = ifelse(is.na(Regime.2), "Presencial",
                                  ifelse(Regime.2 == "R", "Remoto","Presencial"))
         
  ) %>% 
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos) 


colnames(df2) <- toupper(colnames(df2))



#Tipo Alterado

listadt <- c("DATA DE NASCIMENTO",  "ADMISSÃO")

df2[listadt] <- lapply(df2[listadt], dmy)

listanum <- c("SALÁRIO", "IDADE")

df2[listanum] <- lapply(df2[listanum], as.numeric)