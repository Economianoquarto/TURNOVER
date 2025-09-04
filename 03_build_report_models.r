# IMPORT MODEL_DATA

df <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')

## 1) somatório por cnae
sum_by <- df %>%
  select(cnae, Ano, finalpolicy) %>%
  group_by(cnae) %>%
  summarise(sum_amount = sum(finalpolicy, na.rm = TRUE), .groups = "drop")

## 2) junta sum_amount de volta ao df
df2 <- df %>% left_join(sum_by, by = "cnae")

## 3) tabela de grupos (distinct cnae x grupo) e exporta
tg <- df2 %>%
  distinct(cnae, sum_amount) %>%
  mutate(
    group = case_when(
      sum_amount == 4 ~ "treated in 2012",
      sum_amount == 0 ~ "never treated",
      TRUE            ~ "treated in 2013"
    )
  ) %>%
  group_by(group) %>%
  summarise(cnae = paste(sort(unique(cnae)), collapse = " , "),
            .groups = "drop")

write_xlsx(tg, path = "data/model_data/treatment_groups.xlsx")

## 4) diddf com um único mutate (padronizando Ano -> inteiro)
diddf <- df2 %>%
  mutate(
    Ano = as.integer(Ano),
    
    first_treat = ifelse(sum_amount == 4, 2012,
                         ifelse(sum_amount == 0, 0, 2013)),
    
    first.treat = ifelse(sum_amount == 4, 2012,
                         ifelse(sum_amount == 0, NA, 2013)),
    
    time_to_treat = ifelse(first_treat != 0, Ano - first.treat, 0),
    
    treat2012    = factor(ifelse(first.treat == 2012, 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    treat2013    = factor(ifelse(first.treat == 2013, 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    nevertreated = factor(ifelse(is.na(first.treat), 1, 0),
                          levels = c(0,1), labels = c("no","yes")),
    
    treated      = ifelse(first_treat != 0, 1, 0),
    
    year_treated = ifelse(treated == 0, 10000, first.treat),
    
    anotratado   = Ano * treated
  )
#productivitye
#SAL_per_trab
#BEN_per_trab
#PREVSOC_per_trab
#PREVPRI_per_trab
#INDENIZ_per_trab
#TER_per_trab
#TERMAQ_per_trab
#PROP_per_trab

esdesc <-diddf %>% 
  group_by(treated) %>% 
  summarise(mean_iprevsoc = mean(PREVSOC_per_trab, na.rm = T),
            sd_iprevsoc = sd(PREVSOC_per_trab, na.rm = T),
            min_iprevsoc = min(PREVSOC_per_trab, na.rm = T),
            max_iprevsoc = max(PREVSOC_per_trab, na.rm = T)) %>% 
  ungroup()

esdesc <-esdesc %>%
  pivot_longer(!treated, names_to = "statistic", values_to = "value")

esdesc <- esdesc %>%
  separate_wider_delim(statistic, delim = "_", names = c("Statistic", "Variable"))%>%
  relocate("Variable", "Statistic", "treated", "value") %>%
  arrange(Variable, treated)

write_xlsx(esdesc, path = "data/model_data/esdesc.xlsx")

##########################
######### PREV.SOC########
##########################

# twfe model 
twfe_PREV.SOC <- lm(PREVSOC_per_trab ~ finalpolicy + 
                      factor(cnaeidnum) + factor(Ano),
                    data = diddf)

excel_twfe_PREV.SOC <- write_xlsx(tidy(twfe_PREV.SOC), path = "data/model_data/twfe_PREV.SOC.xlsx") 

##### Event Study ######

# run fe ols 
twfe_PREV.SOC = feols(PREVSOC_per_trab ~ i(time_to_treat, treated, ref = -1)|
                        factor(cnaeidnum) + factor(Ano),vcov_cluster, data = diddf)

tidy_twfe_PREV.SOC = tidy(twfe_PREV.SOC, conf.int = TRUE)

tidy_twfe_PREV.SOC <- tidy_twfe_PREV.SOC %>%
  mutate(term = ifelse(term == 'time_to_treat::-6:treated', -6, 
                       ifelse(term == 'time_to_treat::-5:treated', -5,
                              ifelse(term == 'time_to_treat::-4:treated', -4,
                                     ifelse(term == 'time_to_treat::-3:treated', -3,
                                            ifelse(term == 'time_to_treat::-2:treated', -2, 
                                                   ifelse(term == 'time_to_treat::-1:treated', -1,
                                                          ifelse(term == 'time_to_treat::0:treated', 0,
                                                                 ifelse(term == 'time_to_treat::1:treated', 1,
                                                                        ifelse(term == 'time_to_treat::2:treated', 2, 
                                                                               3)))))))))) 

tidy_twfe_PREV.SOC <- tidy_twfe_PREV.SOC %>% 
  dplyr::select(term,estimate,std.error,conf.low,conf.high) %>%
  mutate(approach = "TWFE") 

# now run Sun and Abraham method model 
sa_PREV.SOC = feols(PREVSOC_per_trab ~ sunab(year_treated, Ano,ref.p=-1) | factor(cnaeidnum) + factor(Ano),
                    vcov_cluster, 
                    data = diddf)

tidy_sa_PREV.SOC = tidy(sa_PREV.SOC,conf.int = TRUE)

tidy_sa_PREV.SOC <- tidy_sa_PREV.SOC %>%
  mutate(term = ifelse(term == 'Ano::-6', -6, 
                       ifelse(term == 'Ano::-5', -5,
                              ifelse(term == 'Ano::-4', -4,
                                     ifelse(term == 'Ano::-3', -3,
                                            ifelse(term == 'Ano::-2', -2, 
                                                   ifelse(term == 'Ano::-1', -1,
                                                          ifelse(term == 'Ano::0', 0,
                                                                 ifelse(term == 'Ano::1', 1,
                                                                        ifelse(term == 'Ano::2', 2, 
                                                                               3)))))))))) 

tidy_sa_PREV.SOC <- tidy_sa_PREV.SOC %>% 
  dplyr::select(term,estimate,std.error,conf.low,conf.high) %>%
  mutate(approach = "Sun & Abraham")    

# Santanna & Callaway plot with event-study
csa_PREV.SOC <- att_gt(yname = "PREVSOC_per_trab",
                       gname = "first_treat",
                       idname = "cnaeidnum",
                       tname = "Ano",
                       xformla = ~ 1,
                       data = diddf,
                       est_method = "dr",
                       control_group = "nevertreated",
                       bstrap=T,
                       base_period = "universal",
                       allow_unbalanced_panel = T
)

csa_es_PREV.SOC <- aggte(csa_PREV.SOC, type = "dynamic")
tidy_csaes_PREV.SOC = tidy(csa_es_PREV.SOC)

tidy_csaes_PREV.SOC <- tidy_csaes_PREV.SOC %>%
  mutate(term = ifelse(term == 'ATT(-6)', -6, 
                       ifelse(term == 'ATT(-5)', -5,
                              ifelse(term == 'ATT(-4)', -4,
                                     ifelse(term == 'ATT(-3)', -3,
                                            ifelse(term == 'ATT(-2)', -2, 
                                                   ifelse(term == 'ATT(-1)', -1,
                                                          ifelse(term == 'ATT(0)', 0,
                                                                 ifelse(term == 'ATT(1)', 1,
                                                                        ifelse(term == 'ATT(2)', 2, 
                                                                               3)))))))))) 

tidy_csaes_PREV.SOC <- tidy_csaes_PREV.SOC %>% 
  dplyr::select(term,estimate,std.error,conf.low,conf.high) %>%
  mutate(approach = "Callaway & Sant'anna")

# ETWFE plot with event-study
etwfe_PREV.SOC <- etwfe(fml  = PREVSOC_per_trab ~1, # outcome ~ controls
                        tvar = Ano,        # time variable
                        gvar = first_treat, # group variable
                        data = diddf,       # dataset
                        vcov = ~cnaeidnum,  # vcov adjustment
                        cgroup = "never"
                        #cgroup = "notyet"
)
etwfees_PREV.SOC <- emfx(etwfe_PREV.SOC, type = "event")
tidy_etwfees_PREV.SOC = tidy(etwfees_PREV.SOC)

tidy_etwfees_PREV.SOC <- tidy_etwfees_PREV.SOC %>%
  select(-term) %>%
  rename(term = event)

tidy_etwfees_PREV.SOC <- tidy_etwfees_PREV.SOC %>% 
  dplyr::select(term,estimate,std.error,conf.low,conf.high) %>%
  mutate(approach = "ETWFE")

############
res_PREV.SOC <- rbind(tidy_twfe_PREV.SOC, tidy_sa_PREV.SOC,tidy_csaes_PREV.SOC,tidy_etwfees_PREV.SOC)

dodge <- position_dodge(width=0.3)

# Criando o gráfico com facets para cada abordagem
ggplot(res_PREV.SOC, aes(x = term,
                         y = estimate,
                         ymin = conf.low,
                         ymax = conf.high,
                         color = approach,
                         shape = approach)) +
  geom_pointrange(position = dodge) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Time to treatment",
       y = "ATT",
       title = "Group-Time ATTs por abordagem") +
  facet_wrap(~approach) +  
  theme_minimal()

# Salvando a figura
ggsave("res_PREV.SOC.png", width = 10, height = 6)
