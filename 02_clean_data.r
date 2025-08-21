# Survey data cleansing manager
# Author: Gabriel Gaze Gonçalves Fontenele Gomes
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


dados_juntos <- inner_join(VTI, PO, by = c("Ano",
                                           "CNAE_FORMATADO",
                                           "Classificação Nacional de Atividades Econômicas (CNAE 2.0)",
                                           "CNAE_CODIGO")) %>%
  dplyr::mutate(productivity = VTI/PO ) %>%
  group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  arrange(CNAE_CODIGO, Ano) %>%
  mutate(flow_of_workers = (PO - lag(PO)) / lag(PO),
         CNAE_CORTADA = stringi::stri_sub(str = CNAE_FORMATADO, from = 1, to = 5)) %>%
  ungroup()
