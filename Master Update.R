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

# Funcoes ####
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


proper = function(s)
{
  sub('(.)', ('\\U\\1'), tolower(s), pe = TRUE)
}


arquivo_dataset = '1vuT8i9Bzr6P9wYD7oAJNM-fXoDakg8LL65sBGPDcIfE'


# Dados Compartilhados ####

compl <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1h-U1JOLaMijRuP6_6RjpZDIAUEgPcIKQekASy4HLxoQ/edit?usp=drive_web&ouid=114871898180865383340',
    sheet = 'DADOS',
    col_types = 'c'
  )


compl_exp <- compl %>%
  select(função, etapa, `expectativa de fechamento`)

compl_tech <- compl %>%
  select(`PROCESSO DE TECNOLOGIA`,
         `ETAPA ATUAL`,
         `N° DE CAND`,
         `OBSERVAÇÕES TECNOLOGIA`) %>%
  filter(!is.na(`PROCESSO DE TECNOLOGIA`))

compl_pos <- compl %>%
  select(`N° da posição`, `OBSERVAÇÕES VAGAS N ABERTAS`) %>%
  filter(!is.na(`N° da posição`))

compl_area <- googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1yD8MS6-FbCVCWF-Funks_CUdZ6ocsod9xAgeaE0riB0/edit#gid=0',
  sheet = 'De-Para',
  col_types = 'c'
)

Cargo <- compl %>%
  select('De SLA Médio', 'Para SLA Médio') %>%
  rename(., CARGO = `De SLA Médio`) %>%
  padronizar()

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








# Vagas ####

df_PEBMED <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1h-U1JOLaMijRuP6_6RjpZDIAUEgPcIKQekASy4HLxoQ/edit?usp=drive_web&ouid=114871898180865383340',
    sheet = 'CONTROLE VAGAS (PEBMED)',
    skip = 1,
    col_types = 'c'
  ) %>%
  mutate(EMPRESA = 'PEBMED')



df_prescricao_cliquefarma <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1LxMgPKhE1vI0hJUEjH5tv--19W6EQUYfvU2lfWjC7s4/edit#gid=2059167292',
    sheet = '[NOVO] CONTROLE DE VAGAS',
    col_types = 'c'
  ) %>%
  padronizar() %>%
  select(
    'STATUS',
    'UNIDADE',
    'MOTIVO DA VAGA',
    'DATA DE ABERTURA NA GUPY',
    'FUNÇÃO',
    'EXPECTATIVA DE CONTRATAÇÃO',
    'CARGO',
    'LOCAL',
    'MODELO',
    'RESPONSÁVEL',
    'ÁREA',
    'GESTOR(A) RESPONSÁVEL',
    'ETAPA',
    'LINK DA VAGA DIVULGADA',
    'TEMPO DA VAGA BLOQUEADA',
    'TEMPO TOTAL DA VAGA EM ABERTO',
    'DATA DE FECHAMENTO',
    'DATA DE ADMISSÃO',
    'MÊS DE ADMISSÃO',
    'QUARTER VAGA FECHADA',
    'QUARTER DE INÍCIO',
    'NOME COMPLETO DO CANDIDATO APROVADO',
    'LINKEDIN',
    'DIVERSIDADE',
    'FONTE',
    'ESPECIFICAR FONTE',
    'É INDICAÇÃO DO RH OU GESTOR?',
    'VALOR DO PRÊMIO',
    'MÊS PREMIAÇÃO',
    'REMUNERAÇÃO ORÇADA',
    'REMUNERAÇÃO FECHADA',
    'OBSERVAÇÕES GERAIS'
  ) %>%
  rename(
    EMPRESA = UNIDADE,
    `É INDICAÇÃO DO RH, GESTOR OU CONSULTOR?` = `É INDICAÇÃO DO RH OU GESTOR?` ,
    `VALOR PARA PAGAMENTO` = `VALOR DO PRÊMIO`,
    `VAGA DIVULGADA` = FUNÇÃO
  ) %>%
  mutate(STATUS = case_when (
    grepl('ABERTA', STATUS) ~ 'ABERTA',
    STATUS == 'ESPERA' ~ 'STANDBY',
    TRUE ~ STATUS
  )) %>%
  filter(!MODELO %in% c('PJ', 'RPA'))


df <- plyr::rbind.fill(df_PEBMED, df_prescricao_cliquefarma)


#Tirar linhas vazias (Coluna Status), criar flag de Tecnologia e merges


df2 <- df %>%
  padronizar() %>%
  left_join(
    y = compl_exp %>% rename(exp1 = `expectativa de fechamento`),
    by = c('VAGA DIVULGADA' = 'função' , 'ETAPA' = 'etapa')
  ) %>%
  left_join(
    y = compl_exp %>% select(etapa, `expectativa de fechamento`) %>% rename(exp2 = `expectativa de fechamento`) ,
    by = c('ETAPA' = 'etapa')
  ) %>%
  left_join(Cargo) %>%
  separate(`MOTIVO DE DECLÍNIO`,
           c('MOTIVO DE DECLÍNIO', 'MOTIVO DE DECLÍNIO DETALHE'),
           sep = ' - ') %>%
  mutate(
    ÁREA = replace_na(ÁREA, 'NÃO INFORMADO'),
    flag_tech = as.integer(ifelse(ÁREA == 'TECNOLOGIA', 1, 0)),
    `FECHAMENTO ETAPA` = ifelse(
      flag_tech == 1,
      paste('Q', ceiling(month(today(
        
      )) / 3), sep = ''),
      ifelse(!is.na(exp1), exp1, exp2)
    ),
    `DATA DE ABERTURA NA GUPY` = replace_na(`DATA DE ABERTURA NA GUPY`, '31/12/2099'),
    `DATA DE RECEBIMENTO DO FORMS` = dmy(replace_na(
      `DATA DE RECEBIMENTO DO FORMS`, '31/12/2099'
    )),
    `DATA DE FECHAMENTO` = dmy(`DATA DE FECHAMENTO`),
    `DATA DE FECHAMENTO REAL` = `DATA DE FECHAMENTO`,
    `DATA DE FECHAMENTO` = replace_na(`DATA DE FECHAMENTO`, as.Date(now())),
    `PARA SLA MÉDIO` = replace_na(`PARA SLA MÉDIO`, 'CARGO NÃO ENCONTRADO'),
    `PROCESSO DE TECNOLOGIA` = compl_tech$`PROCESSO DE TECNOLOGIA`,
    `ETAPA ATUAL TEC` = compl_tech$`ETAPA ATUAL`,
    `N° DE CAND` = compl_tech$`N° DE CAND`,
    `OBSERVAÇÕES TECNOLOGIA` = compl_tech$`OBSERVAÇÕES TECNOLOGIA`,
    `TEMPO TOTAL DA VAGA EM ABERTO` = ifelse(`TEMPO TOTAL DA VAGA EM ABERTO` < 0 , NA, `TEMPO TOTAL DA VAGA EM ABERTO`),
    `TEMPO FINAL VAGA` = as.integer(`TEMPO TOTAL DA VAGA EM ABERTO`) - as.integer(replace_na(`TEMPO DA VAGA BLOQUEADA`, 0)),
    `FECHAMENTO ETAPA - NUM` = as.integer(ifelse(
      grepl('Q', `FECHAMENTO ETAPA`) |
        is.na(`FECHAMENTO ETAPA`),
      999,
      `FECHAMENTO ETAPA`
    )),
    `EMPRESA (MOTIVO DECLÍNIO)` = ifelse(
      `EMPRESA (MOTIVO DECLÍNIO)` == 'NÃO INFORMADO' |
        is.na(`EMPRESA (MOTIVO DECLÍNIO)`),
      'EMPRESA NÃO INFORMADA',
      `EMPRESA (MOTIVO DECLÍNIO)`
    ),
    `MOTIVO DE DECLÍNIO DETALHE` = ifelse(
      `MOTIVO DE DECLÍNIO DETALHE` == 'NÃO INFORMADO' |
        is.na(`MOTIVO DE DECLÍNIO DETALHE`),
      'DETALHE NÃO INFORMADO',
      `MOTIVO DE DECLÍNIO DETALHE`
    ),
    `MOTIVO DE DECLÍNIO` = ifelse(
      `MOTIVO DE DECLÍNIO` == 'NÃO INFORMADO' |
        is.na(`MOTIVO DE DECLÍNIO`),
      'MOTIVO NÃO INFORMADO',
      `MOTIVO DE DECLÍNIO`
    ),
    `VALOR PARA PAGAMENTO` = converte(`VALOR PARA PAGAMENTO`),
    MES_PAGAMENTO = if_else(
      !is.na(`VALOR PARA PAGAMENTO`) &
        STATUS == 'FECHADA',
      floor_date(dmy(`DATA DE ADMISSÃO`), unit = 'month') %m+% months(3) ,
      as.Date('2099-12-31')
    ),
    CHAVE_AREA = paste(EMPRESA, ÁREA, sep = '_'),
    across(
      c(
        'DIAS ATÉ INÍCIO DO PROCESSO',
        'TEMPO TOTAL DA VAGA EM ABERTO',
        'TEMPO DA VAGA BLOQUEADA'
      ),
      as.numeric
    ),
    across(c(
      'DATA DE ABERTURA NA GUPY',  'DATA DE ADMISSÃO'
    ),
    dmy)
  ) %>%
  filter(!is.na(STATUS),
         year(`DATA DE FECHAMENTO`) > 2020) %>%
  mutate(id = rownames(.)) %>%
  select(
    c(
      'id',
      'STATUS',
      'NOME DA VAGA OCUPADA',
      'MOTIVO DA VAGA',
      'DATA DE RECEBIMENTO DO FORMS',
      'DATA DE ABERTURA NA GUPY',
      'DIAS ATÉ INÍCIO DO PROCESSO',
      'VAGA DIVULGADA',
      'EXPECTATIVA DE CONTRATAÇÃO',
      'CARGO',
      'LOCAL',
      'MODELO',
      'NÍVEL DE DIFICULDADE',
      'RESPONSÁVEL',
      'ÁREA',
      'PROJETO',
      'GESTOR(A) RESPONSÁVEL',
      'ETAPA',
      'ETAPA ATUAL TEC',
      'LINK DA VAGA DIVULGADA',
      'TEMPO DA VAGA BLOQUEADA',
      'TEMPO TOTAL DA VAGA EM ABERTO',
      'DATA DE FECHAMENTO',
      'DATA DE FECHAMENTO REAL',
      'DATA DE ADMISSÃO',
      'MÊS DE ADMISSÃO',
      'QUARTER VAGA FECHADA',
      'QUARTER DE INÍCIO',
      'NOME COMPLETO DO CANDIDATO APROVADO',
      'OBSERVAÇÕES GERAIS',
      'OBSERVAÇÕES PARA LÍDERES',
      'FECHAMENTO ETAPA',
      'FECHAMENTO ETAPA - NUM',
      'PROCESSO DE TECNOLOGIA',
      'N° DE CAND',
      'OBSERVAÇÕES TECNOLOGIA',
      'TEMPO FINAL VAGA',
      'DIVERSIDADE',
      'PARA SLA MÉDIO',
      'MOTIVO DE DECLÍNIO',
      'MOTIVO DE DECLÍNIO DETALHE',
      'EMPRESA (MOTIVO DECLÍNIO)',
      'ESPECIFICAR FONTE',
      'FONTE',
      'VALOR PARA PAGAMENTO',
      'É INDICAÇÃO DO RH, GESTOR OU CONSULTOR?',
      'MES_PAGAMENTO',
      'EMPRESA',
      'CHAVE_AREA',
      'flag_tech'
    )
  )


# listaNA <- c('MOTIVO DE DECLÍNIO',
#              'MOTIVO DE DECLÍNIO DETALHE',
#              'EMPRESA (MOTIVO DECLÍNIO)')
#
# df2[listaNA] <- lapply(df2[listaNA], function(x) replace(x, is.na(x), 'NÃO INFORMADO'))
#


declina <- df2 %>%
  filter(df2$STATUS == 'FECHADA (DECLINOU DA PROPOSTA)')


sankey2 <-   rbind(
  declina %>%
    group_by(`MOTIVO DE DECLÍNIO`, `EMPRESA (MOTIVO DECLÍNIO)`) %>%
    summarize('occurs' = n(),
              'Declinadas' = n()) %>%
    rename('Source' = `MOTIVO DE DECLÍNIO`,
           'Target' = `EMPRESA (MOTIVO DECLÍNIO)`) ,
  declina %>%
    group_by(`EMPRESA (MOTIVO DECLÍNIO)`, `MOTIVO DE DECLÍNIO DETALHE`) %>%
    summarize('occurs' = n(),
              'Declinadas' = n()) %>%
    rename('Source' = `EMPRESA (MOTIVO DECLÍNIO)`,
           'Target' = `MOTIVO DE DECLÍNIO DETALHE`)
)

sheet_write(df2, ss = arquivo_dataset, sheet = 'Vagas')

sheet_write(sankey2, ss = arquivo_dataset, sheet = 'Vagas_Sankey')

## Exclusão de Variáveis ####
rm(sankey2,declina)








# Ativos e Inativos ####

AtivoeInativomes =  'https://docs.google.com/spreadsheets/d/1A7JIMxzUThFdB4klamkT6Cm1dP-rQ91Dc0AhxT99bH0/edit#gid=55058477'



## Inativos ####


df <-
  googlesheets4::read_sheet(
    AtivoeInativomes,
    sheet = 'Inativos',
    col_types = 'c'
  )

#Renomear Colunas
nomes_antigos <- c('CLT/PJ/RPA', 'DN', 'TIPO')

nomes_novos <-
  c('TIPO COLABORADOR', 'DATA DE NASCIMENTO', 'TIPO DE DESLIGAMENTO')

#Transformação

df2 <- df %>%
  select(
    'NOME',
    'CLT/PJ/RPA',
    'ADMISSÃO PJ',
    'ADMISSÃO CLT',
    'DESLIGAMENTO',
    'TIPO',
    'Salário Bruto',
    'FUNÇÃO',
    'ÁREA',
    'CENTRO DE CUSTO',
    'CPF',
    'DN',
    'Idade',
    'SEXO',
    'COR/RAÇA',
    'GRAU DE ESCOLARIDADE'
  ) %>%
  padronizar() %>%
  mutate(
    DESLIGAMENTO = dmy(DESLIGAMENTO),
    `SALÁRIO BRUTO` = converte(`SALÁRIO BRUTO`),
    IDADE = converte(IDADE),
    LÍDER = NA,
    ADMISSAO = dmy(ifelse(
      is.na(`ADMISSÃO CLT`), `ADMISSÃO PJ`, `ADMISSÃO CLT`
    )),
    STATUS_COLAB = 'INATIVO',
    LIDERADOS = NA,
    TIPO = replace_na(TIPO, 'NÃO INFORMADO')
  ) %>%
  filter(DESLIGAMENTO >= as.Date('2021-01-01'),
         TIPO != 'MIGRAÇÃO') %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos) %>%
  select(-`ADMISSÃO PJ`,-`ADMISSÃO CLT`)


#Alterar tipo

listadt <- c('DATA DE NASCIMENTO')

df2[listadt] <- lapply(df2[listadt], dmy)





## Ativos ####

df_at <-
  googlesheets4::read_sheet(
    AtivoeInativomes,
    sheet = 'CLT ativos',
    skip = 1,
    col_types = 'c'
  )

#Renomear Colunas
nomes_antigos <- c('CLT/ESTAG', 'DN')

nomes_novos <- c('TIPO COLABORADOR', 'DATA DE NASCIMENTO')

#Tratamento

df2_at <- df_at %>%
  padronizar() %>%
  mutate(
    DESLIGAMENTO = '31/12/2099',
    ÁREA = ifelse(is.na(ÁREA), 'SEM ÁREA',      ÁREA),
    ADMISSAO = ifelse(is.na(`ADMISSÃO CLT`), `ADMISSÃO ESTAG`, `ADMISSÃO CLT`),
    STATUS_COLAB = 'ATIVO',
    `SALÁRIO BRUTO` = converte(`SALÁRIO BRUTO`)
  ) %>%
  rename_at(vars(all_of(nomes_antigos)), ~ nomes_novos) %>%
  select(-`ADMISSÃO ESTAG`, -`ADMISSÃO CLT`)

df2_at <- df2_at %>%
  left_join(
    y =
      df2_at %>%
      group_by(LÍDER) %>%
      summarise(LIDERADOS = n()),
    by = c('NOME' = 'LÍDER')
  )


# Alterar tipo de coluna

listadt <- c('DATA DE NASCIMENTO', 'ADMISSAO', 'DESLIGAMENTO')

df2_at[listadt] <- lapply(df2_at[listadt], dmy)




## Dados RPA ####

df_rpa <-
  googlesheets4::read_sheet(
    AtivoeInativomes,
    sheet = 'RPA ativos',
    col_types = 'c'
  )


#Transformação

df2_rpa <- df_rpa %>%
  padronizar() %>%
  select('NOME',
         'FUNÇÃO',
         'CENTRO DE CUSTO',
         'TIPO DE CONTRATO',
         'DESLIGAMENTO')

## Dados PJ ####

df_pj <-
  googlesheets4::read_sheet(
    AtivoeInativomes,
    sheet = 'PJ Ativos',
    col_types = 'c'
  )


#Transformação

df2_pj <- df_pj %>%
  padronizar() %>%
  select('NOME',
         'FUNÇÃO',
         'CENTRO DE CUSTO',
         'TIPO DE CONTRATO',
         'DESLIGAMENTO')



df_final_rpa_pj <-
  rbind(df2_rpa, df2_pj) %>%
  mutate(
    STATUS_COLAB = ifelse(is.na(DESLIGAMENTO), 'ATIVO', 'INATIVO'),
    DESLIGAMENTO = if_else(
      is.na(DESLIGAMENTO),
      as.Date('2099-12-31'),
      dmy(DESLIGAMENTO)
    )
  ) %>%
  rename('TIPO COLABORADOR' = 'TIPO DE CONTRATO',
         ÁREA = 'CENTRO DE CUSTO')








## Unir os dois dfs finais ####


df_final_colab <- plyr::rbind.fill(df2_at, df2) %>%
  mutate(NOME = rm_accent(NOME),
         TEMPO_DE_CASA_MESES = ifelse(ADMISSAO > today(), 0, round(
           ifelse(
             STATUS_COLAB == 'ATIVO',
             today() - ADMISSAO,
             DESLIGAMENTO - ADMISSAO
           ) / 30,
           2
         )))

df_final <- plyr::rbind.fill(df_final_colab, df_final_rpa_pj) %>%
  mutate(
    TEMPO_DE_CASA_MESES = ifelse(ADMISSAO > today(), 0, interval(
      ADMISSAO,
      if_else(STATUS_COLAB == 'ATIVO', today(), DESLIGAMENTO)
    ) / months(1)),
    TEMPO_DE_CASA_FAIXA = cut(
      TEMPO_DE_CASA_MESES,
      breaks = c(0, 6, 12, 24, 72, 9999),
      labels = c(
        '< 6 meses',
        '6 - 11 meses',
        '1 ano - < 2 anos',
        '2 anos - < 6 anos',
        '6+ anos'
      )
    ),
    TEMPO_DE_CASA_FAIXA_ORD = cut(
      TEMPO_DE_CASA_MESES,
      breaks = c(0, 6, 12, 24, 72, 9999),
      labels = c(1,
                 2,
                 3,
                 4,
                 5)
    ),
    RECEM_ADM = ifelse(TEMPO_DE_CASA_MESES < 7, 1, 0),
    CONTEUDISTA = ifelse(grepl('CONTEUDISTA', FUNÇÃO), 'CONTEUDISTA', NA),
    PART_TIME = ifelse(grepl('PART TIME', FUNÇÃO), 'PART TIME', NA),
    COLABORADOR = ifelse(
      `TIPO COLABORADOR` == 'RPA' |
        `TIPO COLABORADOR` == 'PJ',
      'NÃO COLABORADOR' ,
      'COLABORADOR'
    ),
    SEXO = ifelse(SEXO %in% c('FEMININO', 'MASCULINO'), SEXO, 'NÃO INFORMADO'),
    ADMISSAO_COR = replace_na(ADMISSAO, make_date(2099, 12, 31))
  )


#Repor os NAs

nna <- c('NÍVEL', 'COR/RAÇA')

df_final[nna][is.na(df_final[nna])] <- 'NÃO INFORMADO'


sheet_write(df_final, ss = arquivo_dataset, sheet = 'PEBMED_Ativos_e_Inativos')


df_final2 <- df_final %>%
  filter(STATUS_COLAB == 'ATIVO')


df_final2 <- df_final2[colSums(!is.na(df_final2)) > 0] %>%
  mutate(COMPETENCIA = as.Date(format(today(), '%Y-%m-01')))

# 
# df_final2 <- df_final2[colSums(!is.na(df_final2)) > 0] %>%
#   mutate(COMPETENCIA = make_date(2021,7,1))

## Histórico de Ativos ####

df_final_hist <- plyr::rbind.fill(
  df_final2,
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1vuT8i9Bzr6P9wYD7oAJNM-fXoDakg8LL65sBGPDcIfE/edit#gid=291428496',
    sheet = 'HIST_PEBMED',
    col_types = 'c'
  )  %>%
    mutate(across(
      c(
        'DATA DE NASCIMENTO',
        'DESLIGAMENTO',
        'ADMISSAO',
        'ADMISSAO_COR',
        'COMPETENCIA'
      ),
      as.Date
    ),
    across(
      c('SALÁRIO BRUTO', 'IDADE', 'TEMPO_DE_CASA_MESES'),
      converte
    )) %>%
    filter(COMPETENCIA < make_date(year(today(
    )), month(today(
    ), 1)))
)

# 
# df_final_hist <- 
#   df_final2



sheet_write(df_final_hist,
            ss = arquivo_dataset,
            sheet = 'HIST_PEBMED')



### Exclusão de Variáveis ####
rm(df_at,
   df2_at,
   df_rpa,
   df2_rpa,
   df_pj,
   df2_pj,
   df_final_rpa_pj,
   df_final_colab,
   df_final,
   df_final2,
   df_final_hist)



# Vagas Orçadas ####

df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1_hVFAQuQpRAfOhs0nwTc7WyZkwFC05jCMSrH-3ID5_U/edit#gid=30000926',
    sheet = 'Vagas Abertas',
    col_types = 'c',
    skip = 1
  ) %>%
  mutate(EMPRESA = 'PEBMED')



df2 <- df %>%
  padronizar() %>%
  left_join(compl_area, by = c('ÁREA' = 'Original', 'EMPRESA' = 'Empresas')) %>%
  separate(
    `PREVISÃO DE ENTRADA`,
    into = c('PREVISÃO DE ENTRADA MES', 'PREVISÃO DE ENTRADA 2'),
    sep = '\\/'
  ) %>%
  left_join(compl_pos, by = c('NOME DA VAGA OCUPADA' = 'N° da posição')) %>%
  left_join(mes, by = c('PREVISÃO DE ENTRADA MES' = 'mes_extenso')) %>%
  mutate(
    `PREVISÃO DE ENTRADA` = make_date(year = `PREVISÃO DE ENTRADA 2`, month = MES_NUM , day = 1),
    AREA_CORRETA = ifelse(!is.na(AREA), AREA,      ÁREA),
    na_if(., 'R$ 0,00'),
    CHAVE_AREA = paste(EMPRESA,AREA_CORRETA, sep = '_')
  ) %>%
  filter(!is.na(`NOME DA VAGA OCUPADA`)) %>%
  select(
    'CLASSIFICAÇÃO',
    'AREA_CORRETA',
    'NOME DO PROFISSIONAL',
    'STATUS',
    'CARGO',
    'FUNÇÃO',
    'PREVISÃO DE ENTRADA',
    'PREVISÃO DE ENTRADA MES',
    'CARGO NO ORÇAMENTO DA AFYA',
    'NOME DA VAGA OCUPADA',
    'SENIORIDADE',
    'CAPEX',
    'SALÁRIO REAL 2021',
    'SALÁRIO (ORÇADO 2021)
SEM DISSÍDIO',
    'CUSTO TOTAL',
    'MOTIVO DA ABERTURA DA VAGA',
    'PROJETO',
    'OBSERVAÇÃO',
    'EMPRESA',
    'OBSERVAÇÕES VAGAS N ABERTAS',
    'CHAVE_AREA',
    'JANEIRO/2021',
    'FEVEREIRO/2021',
    'MARÇO/2021',
    'ABRIL/2021',
    'MAIO/2021',
    'JUNHO/2021',
    'JULHO/2021',
    'AGOSTO/2021',
    'SETEMBRO/2021',
    'OUTUBRO/2021',
    'NOVEMBRO/2021',
    'DEZEMBRO/2021'
    
  ) %>% rename(ÁREA = AREA_CORRETA,
                'SALÁRIO (ORÇADO 2021) SEM DISSÍDIO' = 'SALÁRIO (ORÇADO 2021)
SEM DISSÍDIO')





df2_pivot <- df2 %>%
  pivot_longer(
    cols = c(
      'JANEIRO/2021',
      'FEVEREIRO/2021',
      'MARÇO/2021',
      'ABRIL/2021',
      'MAIO/2021',
      'JUNHO/2021',
      'JULHO/2021',
      'AGOSTO/2021',
      'SETEMBRO/2021',
      'OUTUBRO/2021',
      'NOVEMBRO/2021',
      'DEZEMBRO/2021'
    ),
    names_to = 'COMPETÊNCIA',
    values_to = 'VALOR',
    values_drop_na = T
  )  %>%
  separate(col = 'COMPETÊNCIA',
           into = c('C1', 'C2'),
           sep = '/') %>%
  left_join(mes, by = c('C1' = 'mes_extenso')) %>%
  mutate(VALOR = converte(VALOR),
         COMPETÊNCIA = make_date(C2, MES_NUM, 1)) %>%
  select(-C1,-C2,-MES_NUM)


sheet_write(df2, ss = arquivo_dataset, sheet = 'Vagas_Orcadas')

sheet_write(df2_pivot, ss = arquivo_dataset, sheet = 'Vagas_Orcadas_Pivot')


## Exclusão de Variáveis ####

rm(df2_pivot)

# Data Atualização ####

dt = data.frame(
  Data_Atualizacao = today() ,
  Data_Atualizacao_Completa  = today(),
  Mes =  month(today()),
  Ano = year(today()),
  Dia = day(today())
) %>%
  left_join(mes, by = c('Mes' = 'MES_NUM')) %>%
  mutate(mes_extenso = proper(mes_extenso)) %>%
  rename(Mes_extenso = mes_extenso)


sheet_write(dt, ss = arquivo_dataset, sheet = 'Data_Atualizacao')



# Movimentação PEBMED Salário e Função ####


df <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/13fX-0h5rCf_E7wY4DQq42JtuExQO5kwnAVCxWF9voIE/edit#gid=202423923',
    sheet = 'Movimentações - Função e Salário a partir de 2019',
    col_types = 'c'
  )

df2 <- df %>%
  padronizar() %>%
  left_join(y = mes, by = c('MÊS' = 'mes_extenso')) %>%
  mutate(
    COMPETENCIA = make_date(ANO, MES_NUM, 1),
    `REMUNERAÇÃO DE:` = converte(`REMUNERAÇÃO DE:`),
    `REMUNERAÇÃO PARA:` = converte(`REMUNERAÇÃO PARA:`),
    NOME = rm_accent(NOME),
    PERCENT_AUMENTO = round((`REMUNERAÇÃO PARA:` / `REMUNERAÇÃO DE:`) - 1, 2),
    MOTIVO = as.factor(MOTIVO),
    AUMENTO = round(`REMUNERAÇÃO PARA:` - `REMUNERAÇÃO DE:`, 2),
    FLAG_FUNCAO = ifelse(!is.na(`FUNÇÃO PARA:`), 1, 0),
    FLAG_REM = ifelse(!is.na(`REMUNERAÇÃO PARA:`), 1, 0)
  ) %>%
  filter(COMPETENCIA > as.Date('2020-12-31')) %>%
  select(
    'NOME',
    'FUNÇÃO DE:',
    'FUNÇÃO PARA:',
    'REMUNERAÇÃO DE:',
    'REMUNERAÇÃO PARA:',
    'MOTIVO',
    'COMPETENCIA',
    'PERCENT_AUMENTO',
    'AUMENTO',
    'FLAG_FUNCAO',
    'FLAG_REM'
  )

sheet_write(df2, ss = arquivo_dataset, sheet = 'PEBMED_SAL_FUNC')
