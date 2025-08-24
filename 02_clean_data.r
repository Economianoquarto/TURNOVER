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

TER <- readxl::read_excel(path = 'data/raw_data/TER.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(TER = Valor, 
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))

TER_MAQ <- readxl::read_excel(path = 'data/raw_data/TER_MAQ.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(TER_MAQ = Valor, 
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))

PROP <- readxl::read_excel(path = 'data/raw_data/PROP.xlsx') %>%
  dplyr::select(Ano,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                Valor) %>%
  dplyr::rename(PROP = Valor, 
                CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  dplyr::filter(!is.na(CNAE_FORMATADO))


lista_de_bases <- list(VTI, PO, TER, TER_MAQ, PROP)
colunas_by <- c("Ano", "CNAE_FORMATADO", "Classificação Nacional de Atividades Econômicas (CNAE 2.0)", "CNAE_CODIGO")
dados_juntos <- reduce(lista_de_bases, inner_join, by = colunas_by)

colunas_para_checar <- c("PO", "TER", "TER_MAQ", "PROP")

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

df <- dados_juntos %>%
  group_by(CNAE_CODIGO, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, CNAE_FORMATADO) %>%
  arrange(CNAE_CODIGO, Ano) %>%
  mutate(productivity = VTI / PO)%>%
  ungroup()


df <- dados_juntos %>%
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

