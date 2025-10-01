# 
# Authors: 
# Last script update: 2024-08-05


files <- list.files(path = 'data/raw_data', full.names = TRUE)

lista_de_bases <- purrr::map(.x = files, 
                             .f = clean_data)

colunas_by <- c("Ano", "CNAE_FORMATADO", "Classificação Nacional de Atividades Econômicas (CNAE 2.0)", "CNAE_CODIGO")

dados_juntos <- reduce(lista_de_bases, inner_join, by = colunas_by)

###################################################################
############ Para ver os missings por variável ###################
#################################################################
colunas_para_checar <- c("PREV_SOC", "SALBC", "SALWC", "GBE", "POE", "REC")
dados_painel_balanceado <- dados_juntos %>%
  group_by(CNAE_CODIGO) %>%
  filter(
    all(across(all_of(colunas_para_checar), ~ !is.na(.)))
  ) %>%
  ungroup() 
# A saída deve ser zero para todas as colunas checadas.
sapply(dados_painel_balanceado[colunas_para_checar], function(x) sum(is.na(x)))
# Compare o número de setores únicos antes e depois
n_setores_antes <- n_distinct(dados_juntos$CNAE_CODIGO)
n_setores_depois <- n_distinct(dados_painel_balanceado$CNAE_CODIGO)
print(paste("Número de setores antes da filtragem:", n_setores_antes))
print(paste("Número de setores depois da filtragem:", n_setores_depois))
print(paste("Número de setores removidos:", n_setores_antes - n_setores_depois))

df_final <- dados_juntos %>%
  group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  arrange(CNAE_CODIGO, Ano) %>%
  mutate(
    SAL_per_trab = SAL / POE,
    SAL_per_trabBC = SALBC / POEBC,
    SAL_per_trabWC = SALWC / POEWC,
    SAL_per_trabSOCIO = SALSOCIO / POESOCIO,
    SAL_share = SAL / REC,
    BEN_per_trab = GBE / POE,
    BEN_shr = GBE / CUSTOS,
    BEN_shr_pessoal = GBE / CUSTOSPESSOAL,
    BEN_pertrab = GBE / (POEBC+POEWC),
    PREVSOC_shr = PREV_SOC / CUSTOS,
    PREVSOC_shr_pessoal = PREV_SOC / CUSTOSPESSOAL,
    PREVSOC_pertrab = PREV_SOC / (POEBC+POEWC),
    PREVSOC_per_trab = PREV_SOC / POE,
    PREVPRI_shr = PREV_PRI / CUSTOS,
    PREVPRI_shr_pessoal = PREV_PRI / CUSTOSPESSOAL,
    INDENIZ_shr = INDENIZ / CUSTOS,
    TER_shr = TER / CUSTOS,
    TER_shr_pessoal = TER / CUSTOSPESSOAL,
    TER_pertrab = TER / (POEBC+POEWC),
    LNPOEWC = log(POEWC),
    LNPOEBC = log(POEBC),
    POEWC_perfirm = POEWC /FRM,
    POEBC_perfirm = POEBC /FRM,
    cnae = stringi::stri_sub(str = CNAE_FORMATADO, from = 1, to = 5)) %>%
  ungroup()


# IMPORTA BASE DE DESONERAÇÃO
desoneracao <- readxl::read_excel("data/externals/desoneracao_staggered.xlsx", col_types = c("text", "text", "numeric"))

df <- inner_join(df_final, desoneracao, by = c("cnae", "Ano"))

df <- df %>%
  mutate(Ano = as.numeric(Ano)) %>%
  separate(cnae, into = c("twodigits", "trash"), sep = "\\.", remove = FALSE) %>%
  group_by(cnae) %>%
  mutate(cnaeidnum = cur_group_id()) %>%
  ungroup()

writexl::write_xlsx(df, path = 'data/model_data/model_data.xlsx')

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))

