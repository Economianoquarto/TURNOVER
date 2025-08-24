# 
# Author: 
# Last script update: 2025-08-21

# Files available at https://sidra.ibge.gov.br/Tabela/7238
# 
#
# Last file update: 2024-08-05


# CREATING AND CLEANING A NEW DIR FOR RAW DATA --------------------
my_dir <- base::paste0(dirname(rstudioapi::getActiveDocumentContext()$path), '/data/raw_data') 

create_and_clean_dir(my_dir)
#nivel: unidade local
VTI <- sidrar::get_sidra(api = "/t/7238/n1/all/v/811/p/all/c12762/all")

writexl::write_xlsx(x = VTI, paste0(my_dir, "/VTI.xlsx"))
#nivel: unidade local
PO <- sidrar::get_sidra(api = "/t/7238/n1/all/v/631/p/all/c12762/all")

writexl::write_xlsx(x = PO, paste0(my_dir, "/PO.xlsx"))
#nivel: empresa
TER <- sidrar::get_sidra(api = "/t/7245/n1/all/v/10428/p/all/c12762/all")

writexl::write_xlsx(x = TER, paste0(my_dir, "/TER.xlsx"))
#nivel: empresa
TER_MAQ <- sidrar::get_sidra(api = "/t/7245/n1/all/v/10429/p/all/c12762/all")

writexl::write_xlsx(x = TER_MAQ, paste0(my_dir, "/TER_MAQ.xlsx"))

#nivel: empresa
PROP <- sidrar::get_sidra(api = "/t/7245/n1/all/v/1273/p/all/c12762/all")

writexl::write_xlsx(x = PROP, paste0(my_dir, "/PROP.xlsx"))

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))


