# 
# Authors: Caio Matteucci, Gabriel Gaze
# Last script update: 2026-01-25

# IMPORTA BASE DE DESONERAÇÃO (VARIÁVEL DE TRATAMENTO)
desoneracao <- readxl::read_excel("data/externals/desoneracao_staggered.xlsx", col_types = c("text", "text", "numeric"))

# BASE COM VARIÁVEIS DEPENDENTES POR CNAE

files <- base::list.files(path = 'data/raw_data', full.names = TRUE)

lista_de_bases <- purrr::map(.x = files, 
                             .f = clean_data)

colunas_by <- c("Ano", "CNAE_FORMATADO", "Classificação Nacional de Atividades Econômicas (CNAE 2.0)", "CNAE_CODIGO")

dados_juntos <- purrr::reduce(lista_de_bases, inner_join, by = colunas_by) %>% # APENAS JUNTANDO TODOS OS DADOS POR CÓDIGO CNAE
      dplyr::filter(Ano <= 2015)

########################

df_final <- dados_juntos %>%
  dplyr::group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  dplyr::arrange(CNAE_CODIGO, Ano) %>%
  dplyr::mutate(
    SAL_pertrabBC = SALBC / POEBC,
    SAL_pertrabWC = SALWC / POEWC,
    BEN_pertrab = GBE / (POEBC+POEWC),
    PREVSOC_pertrab = PREV_SOC / (POEBC+POEWC),
    POEWC_perfirm = POEWC /FRM,
    POEBC_perfirm = POEBC /FRM,
    cnae = stringi::stri_sub(str = CNAE_FORMATADO, from = 1, to = 5)) %>%
    #REC_pertrab = REC / (POEBC+POEWC),
    #SAL_per_trab = SAL / POE,
    #SAL_per_trabSOCIO = SALSOCIO / POESOCIO,
    #SAL_share = SAL / REC,
    #BEN_shr = GBE / CUSTOS,
    #BEN_shr_pessoal = GBE / CUSTOSPESSOAL,
    #PREVSOC_shr = PREV_SOC / CUSTOS,
    #PREVSOC_shr_pessoal = PREV_SOC / CUSTOSPESSOAL,
    #PREVSOC_per_trab = PREV_SOC / POE,
    #PREVPRI_shr = PREV_PRI / CUSTOS,
    #PREVPRI_shr_pessoal = PREV_PRI / CUSTOSPESSOAL,
    #INDENIZ_shr = INDENIZ / CUSTOS,
    #TER_shr = TER / CUSTOS,
    #TER_shr_pessoal = TER / CUSTOSPESSOAL,
    #TER_pertrab = TER / (POEBC+POEWC),
    #POE_perfirm = (POEBC+POEWC)/FRM,
  dplyr::ungroup() %>% 
  base::unique() %>% 
  dplyr::inner_join(desoneracao, by = c("cnae", "Ano")) %>% 
  dplyr::mutate(Ano = as.numeric(Ano)) %>% 
  tidyr::separate(cnae, into = c("twodigits", "trash"), sep = "\\.", remove = FALSE) %>% 
  dplyr::group_by(cnae) %>% 
  dplyr::mutate(cnaeidnum = cur_group_id()) %>%
  dplyr::ungroup()

sum_by <- df_final %>%
  dplyr::select(cnae, Ano, finalpolicy) %>%
  dplyr::group_by(cnae) %>%
  dplyr::summarise(sum_amount = sum(finalpolicy, na.rm = TRUE), .groups = "drop")

df_final <- df_final %>% 
  # JUNTA sum_by COM DF_FINAL
  dplyr::left_join(sum_by, by = "cnae") %>% 
  dplyr::mutate(
    Ano = as.integer(Ano),
    
    first_treat = ifelse(sum_amount == 4, 2012,
                         ifelse(sum_amount == 0, 0, 2013)),
    
    first.treat = ifelse(sum_amount == 4, 2012,
                         ifelse(sum_amount == 0, NA, 2013)),
    
    time_to_treat = ifelse(first_treat != 0, Ano - first.treat, 0),
    
    treat2012    = factor(ifelse(first.treat == 2012, 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    treat2013    = factor(ifelse(first.treat == 2013, 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    nevertreated = factor(ifelse(is.na(first.treat), 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    
    treated      = ifelse(first_treat != 0, 1, 0),
    
    year_treated = ifelse(treated == 0, 10000, first.treat),
    
    anotratado   = Ano * treated
  )



writexl::write_xlsx(df_final, path = 'data/model_data/model_data.xlsx')

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))

