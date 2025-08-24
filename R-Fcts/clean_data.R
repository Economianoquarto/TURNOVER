# A function that creates directories if they do not already exist and cleans up the files if so
# Author:
# Last script update: 2024-05-30

clean_data <- function(path) {

  # creates directories if they do not already exist and cleans up the files if so
  # ARGS:
  #   dir_path - the new directory path
  #   
  # Returns: 
  #   a new clean directory in the entered directory path
  
  nm_col <- tools::file_path_sans_ext(basename(path))
  
  temp_df <- readxl::read_excel(path = path) %>%
    dplyr::select(Ano,
                  `Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)`,
                  `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`,
                  Valor) %>%
    dplyr::rename(!!nm_col := Valor,
                  "CNAE_CODIGO" = "Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)") %>%
    dplyr::mutate(CNAE_FORMATADO = str_extract(`Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, "^\\d{2}\\.\\d{2}-\\d")) %>%
    dplyr::filter(!is.na(CNAE_FORMATADO))
  
  return(temp_df)
  
}
