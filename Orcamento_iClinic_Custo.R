#Bibliotecas
require(dplyr)

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
googlesheets4::gs4_auth(path = segredo)

#Funcoes
converte <- function(x) as.numeric(sub("-","",sub(",", "\\.", gsub("R\\$|\\.", "", x))))


#Dados

df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Ds0a7jn8NkP8Y-G_KtHij-72KTZWvHeE28agt0D690M/edit#gid=2002813053", sheet = 'Orç -Custo Folha', col_types = 'c')

#Renomear Colunas
nomes_antigos <- c("Categoria", "Centro de Custo")

nomes_novos <- c("Evento", "DESCRICAO CC")

#Transformação

df2 <- df %>%
  select(1:7,9:20) %>%
  filter(!is.na("Estrutura DRE"),
         Categoria != "Dissídio 2019") %>%
  mutate(    
    na_if(.,'R$ 0,00'),
    na_if(.,'0')) %>% 
  tidyr::pivot_longer( cols = 8:19, names_to = 'Competência', values_to = 'Custo', values_drop_na = TRUE) %>% 
  mutate(

    Competência =   gsub("\\.","",Competência) ,
    Custo = converte(Custo),
    EMPRESA = "iClinic"
  )  %>% 
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)


