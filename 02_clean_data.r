# 
# Authors: 
# Last script update: 2024-08-05

VTI <- readxl::read_excel(path = 'data/raw_data/VTI.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(VTI = Valor,
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))

PO <- readxl::read_excel(path = 'data/raw_data/PO.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(PO = Valor, 
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))

SPT <- readxl::read_excel(path = 'data/raw_data/SPT.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(SPT = Valor,
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))

dados_juntos <- inner_join(VTI, PO, by = c("Ano",
                                           "CNAE_FORMATADO",
                                           "Classificação Nacional de Atividades Econômicas (CNAE 2.0)",
                                           "CNAE_CODIGO")) %>%
  dplyr::inner_join(SPT, by = c("Ano",
                                "CNAE_FORMATADO",
                                "Classificação Nacional de Atividades Econômicas (CNAE 2.0)",
                                "CNAE_CODIGO")) %>%
  dplyr::mutate(productivity = VTI/PO ) %>%
  group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  arrange(CNAE_CODIGO, Ano)  %>%
  mutate(TURNOVER = (PO - dplyr::lag(PO, 1))/PO) %>%
  ungroup()

writexl::write_xlsx(x = dados_juntos, path = 'data/model_data/model_data.xlsx')

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))
