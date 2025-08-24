# 
# Authors: 
# Last script update: 2025-08-21

# Files available at https://sidra.ibge.gov.br/Tabela/7238
# 
#
# Last file update: 2024-08-05


# CREATING AND CLEANING A NEW DIR FOR RAW DATA --------------------
my_dir <- base::paste0(dirname(rstudioapi::getActiveDocumentContext()$path), '/data/raw_data') 

create_and_clean_dir(my_dir)

# COLOQUE NA ESQUERDA O NOME DA VAR E NA DIREITA A API KEY

apis <- c(
  VTI     = "/t/7238/n1/all/v/811/p/all/c12762/all", # Valor da transformação industrial
  PO      = "/t/7238/n1/all/v/631/p/all/c12762/all", # PESSOAL OCUPADO
  TER     = "/t/7245/n1/all/v/10428/p/all/c12762/all",  # Serviços industriais prestados por terceiros
  TER_MAQ = "/t/7245/n1/all/v/10429/p/all/c12762/all", # Serviços de manutenção e reparação de máquinas e equipamentos ligados à produção prestados por terceiros
  PROP    = "/t/7245/n1/all/v/1273/p/all/c12762/all", #Despesas com propaganda
  GBE     = "/t/7241/n1/all/v/862/p/all/c12762/all" # GASTOS COM BENEFÍCIOS A EMPREGADOS
)

args <- tibble::tibble(
  api = unname(apis),
  filename = names(apis),
  dir = my_dir  
)

pwalk(args, get_and_save)

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))



