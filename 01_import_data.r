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
  FRM  = "/t/7241/n1/all/v/630/p/all/c12762/all", # Empresa - Numero de empresas unidades (Tabela 7241)
  POE      = "/t/7241/n1/all/v/631/p/all/c12762/all", # Empresa - Pessoal ocupado (Tabela 7241) 
  POEBC =  "/t/7241/n1/all/v/813/p/all/c12762/all", # Empresa - Pessoal ocupado ligado a producao (Tabela 7241)
  POEWC =  "/t/7241/n1/all/v/815/p/all/c12762/all", # Empresa - Pessoal ocupado não ligado a producao (Tabela 7241)
  POESOCIO = "/t/7241/n1/all/v/817/p/all/c12762/all", # Empresa - Pessoal ocupado socio (Tabela 7241)
  SAL      = "/t/7241/n1/all/v/822/p/all/c12762/all", # Empresa - Salarios (Tabela 7241)
  SALBC      ="/t/7241/n1/all/v/819/p/all/c12762/all", # Empresa - Salarios pessoal ligado a producao (Tabela 7241)
  SALWC      ="/t/7241/n1/all/v/820/p/all/c12762/all", # Empresa - Salarios pessoal não ligado a producao (Tabela 7241)
  SALSOCIO = "/t/7241/n1/all/v/821/p/all/c12762/all", # Empresa - Retiradas socios e proprietarios (Tabela 7241)
  PREV_SOC  = "/t/7241/n1/all/v/859/p/all/c12762/all", # Empresa - Previdencia social (Tabela 7241) 
  PREV_PRI  = "/t/7241/n1/all/v/858/p/all/c12762/all", # Empresa - Previdencia privada (Tabela 7241)
  INDENIZ  = "/t/7241/n1/all/v/861/p/all/c12762/all", # Empresa - Indenizações (Tabela 7241)
  GBE     = "/t/7241/n1/all/v/862/p/all/c12762/all", # Empresa - Benefícios (Tabela 7241)
  CUSTOSPESSOAL = "/t/7245/n1/all/v/803/p/all/c12762/all", # Empresa - Custos e despesas - gastos de pessoal (Tabela 7245)
  CUSTOS = "/t/7245/n1/all/v/802/p/all/c12762/all", # Empresa - Custos e despesas - total (Tabela 7245)
  PROP = "/t/7245/n1/all/v/1273/p/all/c12762/all", # Empresa -Despesas com propaganda (Tabela 7245)  
  TER = "/t/7245/n1/all/v/10428/p/all/c12762/all", # Empresa - Serviços industriais prestados por terceiros (Tabela 7245)
  REC = "/t/7242/n1/all/v/863/p/all/c12762/all" # Empresa - Rec total (Tabela 7242)
)

args <- tibble::tibble(
  api = unname(apis),
  filename = names(apis),
  dir = my_dir  
)

pwalk(args, get_and_save)

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))



