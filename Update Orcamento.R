

# Bibliotecas ####
require(dplyr)
require(lubridate)
require(googlesheets4)
require(tidyr)

# Autenticacao ####

segredo <-
  'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

googledrive::drive_auth(path = segredo)
gs4_auth(path = segredo)

# Funcoes #####
padronizar <- function(x) {
  x %>%
    mutate_if(is.character, toupper) %>%
    rename_all(toupper) %>%
    na_if('-')
}

converte <-
  function(x)
    as.numeric(sub('-', '', sub(',', '\\.', gsub('R\\$|\\.', '', x))))

rm_accent <- function(str, pattern = 'all') {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c('´', '^') retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: 'all' (retira todos os acentos, que são '´', '`', '^', '~', '¨', 'ç')
  if (!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if (any(pattern == 'Ç'))
    pattern[pattern == 'Ç'] <- 'ç'
  
  symbols <- c(
    acute = 'áéíóúÁÉÍÓÚýÝ',
    grave = 'àèìòùÀÈÌÒÙ',
    circunflex = 'âêîôûÂÊÎÔÛ',
    tilde = 'ãõÃÕñÑ',
    umlaut = 'äëïöüÄËÏÖÜÿ',
    cedil = 'çÇ'
  )
  
  nudeSymbols <- c(
    acute = 'aeiouAEIOUyY',
    grave = 'aeiouAEIOU',
    circunflex = 'aeiouAEIOU',
    tilde = 'aoAOnN',
    umlaut = 'aeiouAEIOUy',
    cedil = 'cC'
  )
  
  accentTypes <- c('´', '`', '^', '~', '¨', 'ç')
  
  if (any(c('all', 'al', 'a', 'todos', 't', 'to', 'tod', 'todo') %in% pattern))
    # opcao retirar todos
    return(chartr(
      paste(symbols, collapse = ''),
      paste(nudeSymbols, collapse = ''),
      str
    ))
  
  for (i in which(accentTypes %in% pattern))
    str <- chartr(symbols[i], nudeSymbols[i], str)
  
  return(str)
}

# Arquivo Dataset ####
arquivo_dataset = '1vuT8i9Bzr6P9wYD7oAJNM-fXoDakg8LL65sBGPDcIfE'
# Dados Compartilhados ####
mes <-
  data.frame(
    mes_extenso = c(
      'JANEIRO',
      'FEVEREIRO',
      'MARÇO',
      'ABRIL',
      'MAIO',
      'JUNHO',
      'JULHO',
      'AGOSTO',
      'SETEMBRO',
      'OUTUBRO',
      'NOVEMBRO',
      'DEZEMBRO'
    ),
    
    MES_NUM = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    mes_abrev = c(
      'JAN',
      'FEV',
      'MAR',
      'ABR',
      'MAI',
      'JUN',
      'JUL',
      'AGO',
      'SET',
      'OUT',
      'NOV',
      'DEZ'
    )
  )
# Orçamento --------
## Orçamento_PEBMED ####

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1_hVFAQuQpRAfOhs0nwTc7WyZkwFC05jCMSrH-3ID5_U/edit#gid=881005633',
    sheet = 'Orçamento_Plano_HC_PROJEÇÃO',
    skip = 1,
    col_types = 'c'
  )

orc_PEB <- df %>%
  
  select(
    'Origem',
    'Movimentação',
    'Cód. da unidade',
    'Descrição da unidade',
    'Cód. do centro de custo',
    'Descrição do centro de custo',
    'Cód. do Ajuste',
    'Descrição do Ajuste',
    'Cód. do projeto',
    'Descrição do projeto',
    'Cód. do Funcionário',
    'Funcionário ou Nome da Vaga',
    '...13',
    'Cód. do cargo',
    'Descrição do cargo',
    'Cód. do sindicato',
    'Descrição do sindicato',
    'Tipo',
    'Cód. Contábil da Conta do Evento',
    'Evento',
    'Mês 1',
    'Mês 2',
    'Mês 3',
    'Mês 4',
    'Mês 5',
    'Mês 6',
    'Mês 7',
    'Mês 8',
    'Mês 9',
    'Mês 10',
    'Mês 11',
    'Mês 12',
    'CONFERÊNCIA',
    'ORIGEM PLANO AFYA'
  ) %>%
  mutate(na_if(., '0,00')) %>%
  tidyr::pivot_longer(
    cols = 21:32,
    names_to = 'Atributo',
    values_to = 'Valor',
    values_drop_na = TRUE
  ) %>%
  mutate(
    Valor = converte(Valor),
    Custo = Valor,
    Competência = make_date(2021, as.integer(sub('Mês', '', Atributo)), 1)
  ) %>%
  select(-Atributo)

#Renomear Colunas
nomes_antigos <-
  c(
    'Cód. do centro de custo',
    'Descrição do centro de custo',
    'Funcionário ou Nome da Vaga',
    'Descrição do cargo',
    'Descrição da unidade'
  )

nomes_novos <-
  c('Cód. CC', 'DESCRICAO CC', 'Colaborador', 'Cargo', 'EMPRESA')

orc_PEB <- orc_PEB %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)


## Orçamento_iClinic ####

#Dados





df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1Ds0a7jn8NkP8Y-G_KtHij-72KTZWvHeE28agt0D690M/edit#gid=2002813053',
    col_types = 'c'
  )

orc_iC <- df %>%
  select(
    'Cód. CC',
    'Centro de Custo',
    'Cargo',
    'Colaborador',
    'jan./2021',
    'fev./2021',
    'mar./2021',
    'abr./2021',
    'mai./2021',
    'jun./2021',
    'jul./2021',
    'ago./2021',
    'set./2021',
    'out./2021',
    'nov./2021',
    'dez./2021'
  ) %>%
  filter(!is.na(df['Centro de Custo'])) %>%
  pivot_longer(
    cols = 5:16,
    names_to = 'Competência',
    values_to = 'Valor',
    values_drop_na = TRUE
  ) %>%
  mutate(Competência = toupper(Competência)) %>%
  separate(col = Competência,
           sep = './',
           into = c('M', 'A')) %>%
  left_join(mes %>% select(MES_NUM, mes_abrev), by = c('M' = 'mes_abrev')) %>%
  mutate(
    Competência = make_date(A, MES_NUM, 1),
    Valor = converte(Valor),
    Evento = 'Salário',
    EMPRESA = 'iClinic',
    Colaborador = replace(Colaborador, is.na(Colaborador), 'SEM NOME')
  ) %>%
  rename('DESCRICAO CC' = 'Centro de Custo')

df_final <- plyr::rbind.fill(orc_PEB, orc_iC) %>%
  mutate(
    Cargo = stringr::str_squish(Cargo),
    `DESCRICAO CC` = toupper(`DESCRICAO CC`),
    CHAVE_AREA = toupper(paste(EMPRESA, `DESCRICAO CC`, sep = '_'))
  ) %>%
  select(
    'EMPRESA',
    'DESCRICAO CC',
    'Colaborador',
    'Cargo',
    'Evento',
    'Valor',
    'Custo',
    'Competência',
    'CHAVE_AREA'
  ) %>%
  rename(COLABORADOR = Colaborador,
         FUNCAO = Cargo)


#Alterar tipo

lista <- c('DESCRICAO CC', 'FUNCAO', 'COLABORADOR', 'Evento')

df_final[lista] <- lapply(df_final[lista], toupper)

### Escrever ORCAMENTO_HC ####

write.csv(df_final,
          "./Dataset/ORCAMENTO_HC.csv",
          row.names = FALSE,
          na = '')



df_hc <-
  df_final %>%
  select(EMPRESA, CHAVE_AREA, COLABORADOR, FUNCAO) %>%
  rename(NOME = COLABORADOR)

# sheet_write(df_final,
#             ss = arquivo_dataset,
#             sheet = 'ORCAMENTO_HC')



## Orçamento iClinic Custo ####

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1Ds0a7jn8NkP8Y-G_KtHij-72KTZWvHeE28agt0D690M/edit#gid=2002813053',
    sheet = 'Orç -Custo Folha',
    col_types = 'c'
  )


#Transformação

orc_custo_ic <- df %>%
  
  select(
    'Obs',
    'Pagamento',
    'Impacto',
    'Categoria',
    'Estrutura DRE',
    'CC Conta Azul',
    'Centro de Custo',
    'jan./2021',
    'fev./2021',
    'mar./2021',
    'abr./2021',
    'mai./2021',
    'jun./2021',
    'jul./2021',
    'ago./2021',
    'set./2021',
    'out./2021',
    'nov./2021',
    'dez./2021'
  ) %>%
  filter(!is.na(`Estrutura DRE`),
         Categoria != 'Dissídio 2019') %>%
  mutate(na_if(., 'R$ 0,00'),
         na_if(., '0')) %>%
  tidyr::pivot_longer(
    cols = 8:19,
    names_to = 'Competência',
    values_to = 'Custo',
    values_drop_na = TRUE
  ) %>%
  mutate(Competência = toupper(Competência)) %>%
  separate(col = Competência,
           sep = './',
           into = c('M', 'A')) %>%
  left_join(mes %>% select(mes_abrev, MES_NUM), by = c('M' = 'mes_abrev')) %>%
  mutate(
    Competência =   make_date(A, MES_NUM, 1) ,
    Custo = converte(Custo),
    EMPRESA = 'iClinic',
    `Centro de Custo` = toupper(`Centro de Custo`),
    CHAVE_AREA = toupper(paste(EMPRESA, `Centro de Custo`, sep = '_'))
  )  %>%
  rename('Evento' = 'Categoria',
         'DESCRICAO CC' = 'Centro de Custo') %>%
  select(-A, -M)


#Extrair o custo PEBMED

orc_custo_peb <- df_final %>%
  filter(EMPRESA == 'PEBMED')


## Orçamento custo final ####

df_final <- plyr::rbind.fill(orc_custo_ic, orc_custo_peb) %>%
  select('EMPRESA',
         'DESCRICAO CC',
         'Evento',
         'Competência',
         'Custo',
         'CHAVE_AREA') %>%
  mutate(`DESCRICAO CC` = stringr::str_squish(`DESCRICAO CC`))


write.csv(df_final,
          "./Dataset/ORCAMENTO_CUSTO.csv",
          row.names = FALSE,
          na = '')

df_custo <- df_final %>% select(CHAVE_AREA, EMPRESA)

summary(as.factor(df_custo$CHAVE_AREA))

# sheet_write(df_final,
#             ss = arquivo_dataset,
#             sheet = 'ORCAMENTO_CUSTO')


# Realizado ####
## Ativos e Inativos PEBMED ####

#Dados

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1vuT8i9Bzr6P9wYD7oAJNM-fXoDakg8LL65sBGPDcIfE/edit#gid=88683936',
    sheet = 'PEBMED_Ativos_e_Inativos'
  )


real_PEB <- df %>%
  filter(COLABORADOR == 'COLABORADOR') %>%
  mutate(EMPRESA = 'PEBMED') %>%
  rename(`ADMISSÃO` = `ADMISSAO`,
         `SALÁRIO` = `SALÁRIO BRUTO`) %>%
  select(
    'EMPRESA',
    'NOME',
    'TIPO COLABORADOR',
    'ADMISSÃO',
    'DESLIGAMENTO',
    'SALÁRIO',
    'FUNÇÃO',
    'ÁREA',
    'CENTRO DE CUSTO',
    'CPF',
    'DATA DE NASCIMENTO',
    'IDADE',
    'SEXO',
    'COR/RAÇA',
    'GRAU DE ESCOLARIDADE',
    'MATRÍCULA',
    'LÍDER'
  )

#Alterar tipo

listadt <- c('DATA DE NASCIMENTO', 'ADMISSÃO', 'DESLIGAMENTO')

real_PEB[listadt] <- lapply(real_PEB[listadt], as.Date)


## Ativos iClinic ####

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1YhKvU9fw1H0nDwbvvlHkJoNQktSmRi9Qw30jDF-91oc/edit#gid=0',
    sheet = 'Dados Gerais - Ativos',
    col_types = 'c'
  )

#Renomear Colunas
nomes_antigos <-
  c('Equipe', 'Nascimento', 'Escolaridade', 'Cargo', 'Data Oficial')

nomes_novos <-
  c('ÁREA',
    'DATA DE NASCIMENTO',
    'GRAU DE ESCOLARIDADE',
    'FUNÇÃO',
    'ADMISSÃO')


#Tratamento

df_at <- df %>%
  select(
    'Empresa',
    'Nome',
    'Matrícula',
    'Sexo',
    'Regime',
    'Equipe',
    'Time',
    'Cargo',
    'Nascimento',
    'Idade',
    'CPF',
    'Salário',
    'Data Oficial',
    'Escolaridade',
    'Tempo de casa'
  ) %>%
  filter(!is.na(df$Cargo)) %>%
  tidyr::separate(Regime, c('Regime.1', 'Regime.2'), ' - ') %>%
  mutate(DESLIGAMENTO = '31/12/2099') %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)


colnames(df_at) <- toupper(colnames(df_at))


## Inativos iClinic ####

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1YhKvU9fw1H0nDwbvvlHkJoNQktSmRi9Qw30jDF-91oc/edit#gid=0',
    sheet = 'Inativos',
    col_types = 'c'
  )


#Renomear Colunas
nomes_antigos <-
  c('Data Oficial',
    'Equipe',
    'Nascimento',
    'Desligamento',
    'D. de saída')

nomes_novos <-
  c('ADMISSÃO',
    'ÁREA',
    'DATA DE NASCIMENTO',
    'TIPO DE DESLIGAMENTO',
    'DESLIGAMENTO')


df_in <- df %>%
  select(
    'Nome',
    'Matrícula',
    'Sexo',
    'Regime',
    'Equipe',
    'Time',
    'Cargo',
    'Nascimento',
    'CPF',
    'Salário',
    'Data Oficial',
    'D. de saída',
    'Desligamento'
  ) %>%
  tidyr::separate(Regime, c('Regime.1', 'Regime.2'), ' - ') %>%
  mutate(na_if(., '-')) %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos)

colnames(df_in) <- toupper(colnames(df_in))


df_final <- plyr::rbind.fill(df_at, df_in) %>%
  mutate(
    SALÁRIO = converte(SALÁRIO),
    IDADE = converte(IDADE),
    ÁREA = tidyr::replace_na(ÁREA, 'SEM ÁREA'),
    EMPRESA = ifelse(
      grepl('MEDICINAE', toupper(ÁREA)),
      'Medicinae',
      ifelse(is.na(EMPRESA) , 'iClinic', EMPRESA)
    ),
    'TIPO COLABORADOR' = ifelse(REGIME.1 == 'CC', 'Diretor Estatutário', REGIME.1),
    'TIPO TRABALHO' = ifelse(
      is.na(REGIME.2),
      'Presencial',
      ifelse(REGIME.2 == 'R', 'Remoto', 'Presencial')
    )
  )

#Alterar tipo

listadt <- c('DATA DE NASCIMENTO', 'ADMISSÃO', 'DESLIGAMENTO')

df_final[listadt] <- lapply(df_final[listadt], dmy)


listanum <- c('SALÁRIO', 'IDADE')

df_final[listanum] <- lapply(df_final[listanum], as.numeric)



## df_realizado ####

df_realizado <- plyr::rbind.fill(df_final, real_PEB) %>%
  mutate(
    FUNÇÃO = replace_na(stringr::str_squish(FUNÇÃO), 'SEM FUNÇÃO') ,
    CHAVE_AREA = stringr::str_squish(paste(EMPRESA,  ÁREA, sep = '_')),
    EMPRESA = ifelse(toupper(EMPRESA) == 'ICLINIC', 'iClinic', EMPRESA)
  )

df_realizado %>%
  filter(is.na(EMPRESA)) %>%
  summarise(n())

summary(as.factor(df_realizado$EMPRESA))

#Maiuscula
lista <- c('ÁREA', 'NOME', 'FUNÇÃO', 'CHAVE_AREA')

df_realizado[lista] <- lapply(df_realizado[lista], toupper)

### Escrever REALIZADO ####

write.csv(df_realizado,
          "./Dataset/REALIZADO.csv",
          row.names = FALSE,
          na = '')

# sheet_write(df_realizado,
#             ss = arquivo_dataset,
#             sheet = 'REALIZADO')



# Informação NOME, ÁREA E EMPRESA Midman ####

df_realizado2 <-
  df_realizado %>%
  select(EMPRESA, CHAVE_AREA, NOME, FUNÇÃO) %>% rename(FUNCAO = FUNÇÃO)


mid_man <-
  plyr::rbind.fill(df_realizado2, df_hc, df_custo)


summary(as.factor(mid_man$CHAVE_AREA))

df <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1yD8MS6-FbCVCWF-Funks_CUdZ6ocsod9xAgeaE0riB0/edit#gid=0",
    sheet = "De-Para",
    col_types = 'c'
  ) %>% mutate(CHAVE_AREA = paste(Empresas, Original, sep = "_"))

nome <- mid_man %>%
  select(NOME) %>%
  distinct() %>%
  filter(!is.na(NOME)) %>%
  arrange(NOME)


area <- mid_man %>%
  select(CHAVE_AREA, EMPRESA) %>%
  distinct() %>%
  arrange(CHAVE_AREA) %>%
  left_join(df %>% select(CHAVE_AREA, AREA)) %>%
  mutate(AREA = ifelse(is.na(AREA), sub('.*_', '', CHAVE_AREA), AREA)) %>%
  arrange(AREA)

funcao <- mid_man %>%
  select(FUNCAO) %>%
  filter(!is.na(FUNCAO)) %>%
  distinct() %>%
  arrange(FUNCAO)


write.csv(nome,
          "./Dataset/NOME_ORCADO.csv",
          row.names = FALSE,
          na = '')

write.csv(area,
          "./Dataset/AREA_ORCADO.csv",
          row.names = FALSE,
          na = '')

write.csv(funcao,
          "./Dataset/FUNCAO_ORCADO.csv",
          row.names = FALSE,
          na = '')
