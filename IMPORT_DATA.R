rm(list = ls())


library(readxl)
library(tidyverse)


arquivos_presentes <- list.files(path = 'TURNOVER', full.names = TRUE, pattern = '.xlsx')


lista_de_arquivos <- base::list()

for (i in arquivos_presentes) {
  abas <- excel_sheets(i)
  
  for (j in abas) {
    
    print(paste0("Fazendo o arquivo ", j))
    
    dados1 <- readxl::read_excel(path = i, skip = 8, col_types = 'text', sheet = j)
    
    dados_editados <- dados1[, c(1, 8, 9, 10)] 
    
    
    
    nomes_colunas_editadas <- stringi::stri_replace_all_fixed(
      names(dados_editados)[2:length(names(dados_editados))],
      "...", "n"
    )
    
    names(dados_editados) <- c('CNAE', nomes_colunas_editadas)
    
    nomes_colunas <- names(dados_editados)[2:length(names(dados_editados))]
    
    dados_editados <- dados_editados %>%
      tidyr::pivot_longer(cols = nomes_colunas, values_to = 'Eventos', names_to = 'Ano')
    
    lista_de_arquivos[[j]] <- dados_editados
    
  }
    
}


dados_finais <- do.call(what = bind_rows, args = lista_de_arquivos) %>%
  dplyr::filter(!CNAE %in% c("TOTAL",
                             "Ignorado",
                             "Fonte: DATAPREV, CAT, SUB.",
                             "NOTA: Os dados são preliminares, estando sujeitos a correções.")) %>%
  dplyr::mutate(Ano = stringi::stri_sub(str = Ano, from = 1, to = 4),
                Eventos = as.numeric(ifelse(Eventos == "-", 0, Eventos))) %>%
  dplyr::group_by(CNAE, Ano) %>%
  dplyr::summarise(Eventos = sum(Eventos, na.rm = TRUE))

writexl::write_xlsx(x = dados_finais, path = 'TURNOVER/Tipicos.xlsx')
