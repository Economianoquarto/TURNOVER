# 
# Authors: 
# Last script update: 2024-08-05


files <- list.files(path = 'data/raw_data', full.names = TRUE)

lista_de_bases <- purrr::map(.x = files, 
                             .f = clean_data)

colunas_by <- c("Ano", "CNAE_FORMATADO", "Classificação Nacional de Atividades Econômicas (CNAE 2.0)", "CNAE_CODIGO")

dados_juntos <- reduce(lista_de_bases, inner_join, by = colunas_by)

colunas_para_checar <- c("PO", "TER", "TER_MAQ", "PROP", "GBE")

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
  mutate(productivity = VTI / PO)%>%
  ungroup() %>%
  group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  arrange(CNAE_CODIGO, Ano) %>%
  mutate(
    productivity = VTI / PO,
    flow_of_productivity = (productivity - dplyr::lag(productivity)) / dplyr::lag(productivity),
    flow_of_workers = (PO - dplyr::lag(PO)) / dplyr::lag(PO),
    job_variation = (PO - dplyr::lag(PO)),
    flow_of_ter = (TER - dplyr::lag(TER)) / dplyr::lag(TER),
    flow_of_ter_maq = (TER_MAQ - dplyr::lag(TER_MAQ)) / dplyr::lag(TER_MAQ),
    flow_of_prop = (PROP - dplyr::lag(PROP)) / dplyr::lag(PROP),
    CNAE_CORTADA = stringi::stri_sub(str = CNAE_FORMATADO, from = 1, to = 5)) %>%
  ungroup()

writexl::write_xlsx(x = df_final, path = 'data/model_data/model_data.xlsx')

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))

