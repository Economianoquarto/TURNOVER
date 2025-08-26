# IMPORT MODEL_DATA

model_data <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')


mod0 <- plm(productivity ~ GBE_per_worker, 
            data= model_data,
            model="within", 
            effect="twoways")

print(summary(mod0))

mod1 <- plm(GBE_per_worker ~ TER_per_worker + TER_MAQ_per_worker + PROP_per_worker, 
            data = model_data, 
            model="within", 
            effect="twoways")

summary(mod1)

mod2 <- plm(productivity ~ GBE_per_worker | # VARIAÇÃO NO TRABALHO EM T-1
              TER_per_worker + PROP_per_worker, # SERVIRÇOS PRESTADOS POR TERCEIROS EM T-1 + SERVIÇOS DE REPARO E MANUTENÇÃO PRESTADOR POR TERCEIROS EM T-1
            data = model_data, 
            model= "within", 
            effect="twoways")


print(summary(mod2))


