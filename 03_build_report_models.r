

# IMPORT MODEL_DATA

data <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')

iv_reg <- plm::plm(productivity ~ TURNOVER | SPT, 
                   data = data, 
                   effect = "twoways", 
                   index = c("CNAE_FORMATADO", "Ano"))

summary(iv_reg)
