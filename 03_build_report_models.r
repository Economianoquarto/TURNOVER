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
# vER COMANDO LATEX

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

#SAL_per_trab
#SAL_per_trabBC
#SAL_per_trabWC
#SAL_per_trabSOCIO 
#SAL_share 
#BEN_per_trab 
#BEN_shr
#BEN_shr_pessoal 
#PREVSOC_shr 
#PREVSOC_shr_pessoal 
#PREVPRI_shr
#PREVPRI_shr_pessoal 
#INDENIZ_shr
#TER_shr 
#PROP_shr

esdesc <-diddf %>% 
  group_by(treated) %>% 
  summarise(mean_iprevsoc = mean(PREVSOC_shr_pessoal, na.rm = T),
            sd_iprevsoc = sd(PREVSOC_shr_pessoal, na.rm = T),
            min_iprevsoc = min(PREVSOC_shr_pessoal, na.rm = T),
            max_iprevsoc = max(PREVSOC_shr_pessoal, na.rm = T)) %>% 
  ungroup()

esdesc <-esdesc %>%
  pivot_longer(!treated, names_to = "statistic", values_to = "value")

esdesc <- esdesc %>%
  separate_wider_delim(statistic, delim = "_", names = c("Statistic", "Variable"))%>%
  relocate("Variable", "Statistic", "treated", "value") %>%
  arrange(Variable, treated)

write_xlsx(esdesc, path = "data/model_data/esdesc.xlsx")
# VER COMANDO PARA GERAR TEX

# Lista das variáveis dependentes
dependent_vars <- c("BEN_per_trab", 
                    "PREVSOC_per_trab",
                    "SAL_per_trabBC",
                    "SAL_per_trabWC",
                    "SAL_per_trabSOCIO"
                    )

# Lista para armazenar todos os resultados
all_results <- list()

# Loop para cada variável dependente
for (var in dependent_vars) {
#var <- "PREVSOC_per_trab"
  cat("Processando variável:", var, "\n")
  
  # Criar fórmula dinâmica
  formula_twfe <- as.formula(paste(var, "~ finalpolicy + factor(cnaeidnum) + factor(Ano)"))
  formula_feols <- as.formula(paste(var, "~ i(time_to_treat, treated, ref = -1) | factor(cnaeidnum) + factor(Ano)"))
  formula_sa <- as.formula(paste(var, "~ sunab(year_treated, Ano, ref.p=-1) | factor(cnaeidnum) + factor(Ano)"))
  
  # 1. TWFE model 
  twfe_model <- lm(formula_twfe, data = diddf)
  
  # Salvar resultado TWFE
  write_xlsx(tidy(twfe_model), path = paste0("data/model_data/twfe_", var, ".xlsx"))
  
  # 2. Event Study TWFE
  twfe_es <- feols(formula_feols, vcov_cluster, data = diddf)
  tidy_twfe <- tidy(twfe_es, conf.int = TRUE)
  
  tidy_twfe <- tidy_twfe %>%
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
  
  tidy_twfe <- tidy_twfe %>% 
    dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
    mutate(approach = "TWFE", variable = var)
  
  # 3. Sun and Abraham method
  sa_model <- feols(formula_sa, vcov_cluster, data = diddf)
  tidy_sa <- tidy(sa_model, conf.int = TRUE)
  
  tidy_sa <- tidy_sa %>%
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
  
  tidy_sa <- tidy_sa %>% 
    dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
    mutate(approach = "Sun & Abraham", variable = var)
  
  # 4. Callaway & Sant'anna
  csa_model <- att_gt(yname = var,
                      gname = "first_treat",
                      idname = "cnaeidnum",
                      tname = "Ano",
                      xformla = ~ 1,
                      data = diddf,
                      est_method = "dr",
                      control_group = "nevertreated",
                      bstrap = T,
                      base_period = "universal",
                      allow_unbalanced_panel = T)
  
  csa_es <- aggte(csa_model, type = "dynamic")
  tidy_csa <- tidy(csa_es)
  
  tidy_csa <- tidy_csa %>%
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
  
  tidy_csa <- tidy_csa %>% 
    dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
    mutate(approach = "Callaway & Sant'anna", variable = var)
  
  # 5. ETWFE
  etwfe_model <- etwfe(fml = as.formula(paste(var, "~ 1")),
                       tvar = Ano,
                       gvar = first_treat,
                       data = diddf,
                       vcov = ~cnaeidnum,
                       cgroup = "never")
  
  etwfe_es <- emfx(etwfe_model, type = "event")
  tidy_etwfe <- tidy(etwfe_es)
  
  tidy_etwfe <- tidy_etwfe %>%
    select(-term) %>%
    rename(term = event)
  
  tidy_etwfe <- tidy_etwfe %>% 
    dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
    mutate(approach = "ETWFE", variable = var)
  
  # Combinar todos os resultados para esta variável
  var_results <- rbind(tidy_twfe, tidy_sa, tidy_csa, tidy_etwfe)
  
  # Adicionar à lista de todos os resultados
  all_results[[var]] <- var_results
  
  # Criar gráfico para esta variável
  dodge <- position_dodge(width = 0.3)
  
  p <- ggplot(var_results, aes(x = term,
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
         title = paste("Group-Time ATTs por abordagem -", var)) +
    facet_wrap(~approach) +  
    theme_minimal()
  
  # Salvar gráfico
  
  ggsave(file.path("figs", paste0("res_", var, ".png")), plot = p, width = 10, height = 6)
  
   
  cat("Concluído para variável:", var, "\n\n")
}

# Combinar todos os resultados em um único dataframe
final_results <- do.call(rbind, all_results)

# Salvar resultados combinados
write_xlsx(final_results, path = "data/model_data/all_estimators_results.xlsx")

# Criar gráfico comparativo com todas as variáveis
comparison_plot <- ggplot(final_results, aes(x = term,
                                             y = estimate,
                                             ymin = conf.low,
                                             ymax = conf.high,
                                             color = approach,
                                             shape = approach)) +
  geom_pointrange(position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Time to treatment",
       y = "ATT",
       title = "Comparação de ATTs por variável e abordagem") +
  facet_grid(variable ~ approach, scales = "free_y") +  
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar gráfico comparativo
ggsave(file.path("figs", "comparison_all_variables.png"), plot = comparison_plot, width = 16, height = 20, bg = "white")

cat("Análise completa! Todos os resultados foram salvos.\n")
