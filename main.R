# Clean environment
rm(list = ls())

# Instalar o pacote se necessário
# install.packages("sidrar")

# Carregar o pacote
library(sidrar)
library(plm)
library(ivreg)

library(tidyverse)
VTI <- get_sidra(api = "/t/7238/n1/all/v/811/p/all/c12762/all") %>%
  select(Ano,`Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,Valor) %>%
  rename(VTI = Valor, CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  filter(!is.na(CNAE_FORMATADO))

PO <- get_sidra(api = "/t/7238/n1/all/v/631/p/all/c12762/all") %>%
  select(Ano,`Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,Valor) %>%
  rename(PO = Valor, CNAE_CODIGO = `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)` ) %>%
  mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
  filter(!is.na(CNAE_FORMATADO))

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


dados_eventos <- readxl::read_excel(path = 'TURNOVER/Tipicos.xlsx') %>%
  mutate(codigo_com_ponto = str_replace(CNAE, "(.{2})(.*)", "\\1.\\2"))
  
dados_juntos2 <- dplyr::inner_join(x = dados_juntos, y = dados_eventos, by = c(c("Ano" = "Ano"), c("CNAE_CORTADA" = "codigo_com_ponto")))


lm_model <- plm::plm(log(productivity) ~ flow_of_workers,
                     data = dados_juntos2, 
                     effect = "twoways", index = c("CNAE_CORTADA", "Ano"))


lm_model <- lm(flow_of_workers ~ lag(Eventos), data = dados_juntos2)

summary(lm_model)




