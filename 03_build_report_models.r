# IMPORT MODEL_DATA

model_data <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')

#productivitye
#SAL_per_trab
#BEN_per_trab
#PREVSOC_per_trab
#PREVPRI_per_trab
#INDENIZ_per_trab
#TER_per_trab
#TERMAQ_per_trab
#PROP_per_trab

mod0 <- plm(productivitye ~ BEN_per_trab + SAL_per_trab + PREVSOC_per_trab + PREVPRI_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab + PROP_per_trab, 
            data= model_data,
            model="within", 
            effect="twoways")

print(summary(mod0))

mod1 <- plm(INDENIZ_per_trab ~ PROP_per_trab, 
            data = model_data, 
            model="within", 
            effect="twoways")

summary(mod1)

mod2 <- plm(productivitye ~ BEN_per_trab + SAL_per_trab + PREVSOC_per_trab + PREVPRI_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab | # VARIAÇÃO NO TRABALHO EM T-1
            SAL_per_trab + PREVSOC_per_trab + PREVPRI_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab + PROP_per_trab, # SERVIRÇOS PRESTADOS POR TERCEIROS EM T-1 + SERVIÇOS DE REPARO E MANUTENÇÃO PRESTADOR POR TERCEIROS EM T-1
            data = model_data, 
            model= "within", 
            effect="twoways")


print(summary(mod2))


