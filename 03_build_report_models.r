# IMPORT MODEL_DATA

model_data <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')


mod0 <- plm(productivity ~ lag(job_variation), 
            data= model_data,
            model="within", 
            effect="twoways")

print(summary(mod0))

mod1 <- plm(job_variation ~ TER + TER_MAQ, 
            data = model_data, 
            model="within", 
            effect="twoways")

summary(mod1)

mod2 <- plm(productivity ~ lag(job_variation) | # VARIAÇÃO NO TRABALHO EM T-1
            lag(TER) + lag(TER_MAQ) + GBE, # SERVIRÇOS PRESTADOS POR TERCEIROS EM T-1 + SERVIÇOS DE REPARO E MANUTENÇÃO PRESTADOR POR TERCEIROS EM T-1
            data = model_data, 
            model= "within", 
            effect="twoways")


print(summary(mod2))


