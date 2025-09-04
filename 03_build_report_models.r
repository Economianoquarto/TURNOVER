# IMPORT MODEL_DATA

df <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')

tg <- df %>%
  select(cnae, Ano, finalpolicy) %>%
  group_by(cnae) %>%
  summarise(sum_amount = sum(finalpolicy), .groups = "drop") %>%
  mutate(
    group = case_when(
      sum_amount == 4 ~ "treated in 2012",
      sum_amount == 0 ~ "never treated",
      TRUE            ~ "treated in 2013"
    )
  ) %>%
  group_by(group) %>%
  summarise(cnae = paste(cnae, collapse = " , "), .groups = "drop")
write_xlsx(tg, path = "data/model_data/treatment_groups.xlsx")

#productivitye
#SAL_per_trab
#BEN_per_trab
#PREVSOC_per_trab
#PREVPRI_per_trab
#INDENIZ_per_trab
#TER_per_trab
#TERMAQ_per_trab
#PROP_per_trab

mod0 <- plm(productivitye ~ BEN_per_trab + SAL_per_trab + PREVSOC_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab, 
            data= df,
            model="within", 
            effect="twoways")

print(summary(mod0))

mod1 <- plm(BEN_per_trab ~ PROP_per_trab + PREVPRI_per_trab , 
            data = df, 
            model="within", 
            effect="twoways")

summary(mod1)

mod2 <- plm(productivitye ~ BEN_per_trab + SAL_per_trab + PREVSOC_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab | 
            SAL_per_trab + PREVSOC_per_trab + PREVPRI_per_trab + INDENIZ_per_trab + TER_per_trab +TERMAQ_per_trab + PROP_per_trab, 
            data = df, 
            model= "within", 
            effect="twoways")


print(summary(mod2))


