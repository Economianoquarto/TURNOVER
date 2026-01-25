# 
# Authors: Caio Matteucci, Gabriel Gaze
# Last script update: 2025-08-21
# Files available at https://sidra.ibge.gov.br/Tabela/7238


# IMPORT MODEL_DATA

diddf2 <- readxl::read_excel(path = 'data/model_data/model_data.xlsx')

dependent_vars <- c("BEN_pertrab",
                  "PREVSOC_pertrab",
                  "SAL_pertrabBC",
                  "SAL_pertrabWC",
                  "POEBC_perfirm",
                  "POEWC_perfirm")

# ESTATÍSTICA DESCRITIVA ------------------

groups_plot <- diddf %>%  
  dplyr::select(cnae, sum_amount) %>% 
  base::unique() %>% 
  dplyr::mutate(
    sum_amount = dplyr::case_when(
      sum_amount == 4 ~ "Tratado 2011",
      sum_amount == 3 ~ "Tratado 2012",
      sum_amount == 0 ~ "Não tratado",
      TRUE ~ as.character(sum_amount)
    )
  ) %>% 
  dplyr::group_by(sum_amount) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>% 
  dplyr::arrange(desc(total)) %>%
  ggplot2::ggplot(aes(x = reorder(sum_amount, total), y = total)) +
  ggplot2::geom_col(fill = "steelblue", width = 0.6) +
  ggplot2::geom_text(aes(label = total), hjust = -0.2, size = 4) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text(
      face = "bold",   # negrito
      size = 14        # aumenta o tamanho
    )
  )

ggplot2::ggsave(file.path("figs", 'NÚMERO DE SETORES POR ANO DE TRATAMENTO.png'), 
                plot = groups_plot,
                width = 10, height = 6)


## TABELA DOS SETORES POR GRUPO DE TRATAMENTO
tg <- diddf %>%
  dplyr::distinct(cnae, sum_amount) %>%
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

writexl::write_xlsx(tg, path = "tables/treatment_groups.xlsx")



total_tratados <- base::sum(diddf$finalpolicy, na.rm = TRUE)
media_geral <- base::mean(diddf$finalpolicy, na.rm = TRUE)
sd_geral <- stats::sd(diddf$finalpolicy, na.rm = TRUE)
n_tratados <- base::sum(diddf$finalpolicy == 0, na.rm = TRUE)

esdesc <- tibble::tibble(
  Ano = c('Total', '< 2012', '2012', '>= 2013'),
  `Média` = c(media_geral,
            base::mean(diddf$finalpolicy[diddf$Ano == 2011], na.rm = TRUE),
            base::mean(diddf$finalpolicy[diddf$Ano == 2012], na.rm = TRUE),
            base::mean(diddf$finalpolicy[diddf$Ano == 2013], na.rm = TRUE)),
  `Desvio padrão` = c(sd_geral,
             stats::sd(diddf$finalpolicy[diddf$Ano == 2011], na.rm = TRUE),
             stats::sd(diddf$finalpolicy[diddf$Ano == 2012], na.rm = TRUE),
             stats::sd(diddf$finalpolicy[diddf$Ano == 2013], na.rm = TRUE)),
  `Total de tratados` = c(total_tratados,
                          base::sum(diddf$finalpolicy[diddf$Ano == 2011], na.rm = TRUE),
                          base::sum(diddf$finalpolicy[diddf$Ano == 2012], na.rm = TRUE),
                          base::sum(diddf$finalpolicy[diddf$Ano == 2013], na.rm = TRUE)),
  `Total de não tratados` = c(n_tratados,
                              base::sum(diddf$finalpolicy[diddf$Ano == 2011] == 0, na.rm = TRUE),
                              base::sum(diddf$finalpolicy[diddf$Ano == 2012] == 0, na.rm = TRUE),
                              base::sum(diddf$finalpolicy[diddf$Ano == 2013] == 0, na.rm = TRUE))
)


writexl::write_xlsx(x = esdesc, path = 'tables/Estatística descritiva.xlsx')


# BOXPLOT
bxplot <- diddf %>%
  dplyr::filter(Ano <= 2011) %>% 
  select(all_of(dependent_vars), year_treated) %>% 
  pivot_longer(
    cols = all_of(dependent_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # substituindo 1000 por "Não tratado" e mantendo o restante como fator
    treated = ifelse(year_treated == 10000, "Não tratado", as.character(year_treated)),
  ) %>% 
  ggplot(aes(x = treated, y = value, fill = treated)) +
  geom_boxplot(outlier.color = "red", outliers = FALSE, varwidth = TRUE) +
  facet_wrap(~variable, scales = "free_y") +  
  labs(x = NULL, y = "Valor", fill = "Grupo de tratamento", title = "Variáveis dependentes por grupo tratado no período pré política") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Variáveis dependentes por grupo de tratamento \n no período pré tratamento") +
  theme(
    axis.text.x = element_blank(),  # remove textos do eixo x
    axis.ticks.x = element_blank(), # remove ticks do eixo x
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top",        # legenda no topo
    plot.title = element_text(hjust = 0.5)  
    )

ggplot2::ggsave(file.path("figs", 'BOXPLOT.png'), 
                plot = bxplot,
                width = 10, height = 6)


line_plot <- diddf %>%
  dplyr::select(Ano, year_treated, all_of(dependent_vars)) %>%
  tidyr::pivot_longer(
    cols = all_of(dependent_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::group_by(Ano, year_treated, variable) %>%
  dplyr::summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    treated = ifelse(year_treated == 10000, "Não tratado", as.character(year_treated))
    ) %>% 
  ggplot(aes(x = Ano, y = mean_value, color = treated)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Ano",
    y = "Média",
    color = "Grupo",
    title = "Tendências médias por grupo tratado",
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.ticks.x = element_blank(), # remove ticks do eixo x
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5)  # legenda no topo
  )

ggplot2::ggsave(file.path("figs", 'TENDÊNCIAS.png'), 
                plot = line_plot,
                width = 10, height = 6)

# MODELOS --------------


final_results <- purrr::map_dfr(
  dependent_vars,
  ~ run_did_models(diddf = diddf, var = .x)
) %>% 
  dplyr::mutate(Iteracao = 'Original') 


comparison_plot <- final_results %>% 
  ggplot2::ggplot(aes(x = term,
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high,
                      color = approach,
                      shape = approach)) +
  ggplot2::geom_pointrange(position = position_dodge(width = 0.3)) +
  ggplot2::geom_hline(yintercept = 0, color = "black") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  ggplot2::labs(x = "Time to treatment",
       y = "ATT",
       title = "Comparação de ATTs por variável e abordagem") +
  ggplot2::facet_grid(variable ~ approach, scales = "free_y") +  
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggplot2::theme(
    legend.position = "none",   # <<< remove a legenda
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  )
 
ggplot2::ggsave(file.path("figs", "comparison_all_variables.png"), plot = comparison_plot, width = 16, height = 20, bg = "white")


# PLACEBO TEST -------------------------

vars_to_test <- c("BEN_pertrab", 'PREVSOC_pertrab')

placebo_df <- diddf %>% 
  dplyr::select(-sum_amount, 
                -first_treat,
                -first.treat,
                -time_to_treat,
                -treat2012,
                -treat2013,
                -nevertreated,
                -treated,
                -year_treated,
                -anotratado)

final_results_placebo <- run_placebo_tests(placebo_df = placebo_df, 
                                           vars_to_test = vars_to_test, 
                                           iter = 1000, 
                                           seed = 80) %>% 
  dplyr::mutate(Iteracao = as.character(Iteracao)) %>% 
  dplyr::bind_rows(final_results) %>% 
  dplyr::mutate(
    is_placebo = Iteracao != "Original"
  ) %>% 
  dplyr::filter(variable %in% vars_to_test)

writexl::write_xlsx(x = final_results_placebo, path = 'data/model_data/PLACEBOS.xlsx')


original <- final_results_placebo %>%
  dplyr::filter(Iteracao == "Original", term >= 0) %>%
  dplyr::select(approach, variable, term, estimate) %>%
  dplyr::rename(orig_estimate = estimate)

placebo_pval <- final_results_placebo %>%
  dplyr::filter(Iteracao != "Original", term >= 0) %>% 
  dplyr::left_join(original, by = c("approach", "variable", "term")) %>%
  dplyr::mutate(larger_than_orig = abs(estimate) >= abs(orig_estimate)) %>%
  dplyr::group_by(approach, variable, term) %>%
  dplyr::summarise(
    n_placebos = n(),
    n_larger = sum(larger_than_orig),
    prop_larger = n_larger / n_placebos
  ) %>%
  ungroup()

writexl::write_xlsx(x = placebo_pval, path = 'tables/placebo_summary.xlsx')

post_effects <- final_results_placebo %>%
  dplyr::filter(term >= 0) %>%
  dplyr::group_by(variable, approach, Iteracao, is_placebo) %>%
  dplyr::summarise(
    post_ATT = mean(abs(estimate)),
    .groups = "drop"
  )

original_effect <- post_effects %>%
  dplyr::filter(!is_placebo)

placebo_effects <- post_effects %>%
  dplyr::filter(is_placebo)

placebo_hist <- ggplot2::ggplot(placebo_effects, aes(x = post_ATT)) +
  ggplot2::geom_histogram(bins = 20, fill = "grey80", color = "black") +
  ggplot2::geom_vline(
    data = original_effect,
    aes(xintercept = post_ATT),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  ggplot2::facet_grid(variable ~ approach, scales = "free") +
  ggplot2::labs(
    title = "Distribuição dos efeitos placebo em módulo",
    subtitle = "Linha vermelha: efeito médio do tratamento verdadeiro em módulo",
    x = NULL,
    y = NULL
  ) +
  theme_bw()

ggplot2::ggsave(file.path("figs", "DIST_PLACEBO.png"), plot = placebo_hist, width = 16, height = 20, bg = "white")

  
placebo_bands <- final_results_placebo  %>%
  dplyr::filter(is_placebo) %>%
  dplyr::group_by(variable, approach, term) %>%
  dplyr::summarise(
    p5  = quantile(estimate, 0.05),
    p95 = quantile(estimate, 0.95),
    .groups = "drop"
  )

original_es <- final_results_placebo  %>%
  dplyr::filter(!is_placebo)

envelopamento <- placebo_bands %>% 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    aes(x = term, ymin = p5, ymax = p95),
    fill = "grey80"
  ) +
  ggplot2::geom_line(
    data = original_es,
    aes(x = term, y = estimate),
    color = "red"
  ) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::facet_grid(variable ~ approach, scales = "free_y") +
  labs(
    title = "Gráfico de envelopamento",
    subtitle = "Área cinza: 5-95° percentil dos placebos",
    x = NULL,
    y = NULL
  ) +
  theme_bw()

ggplot2::ggsave(file.path("figs", "LINHA_PLACEBO.png"), plot = envelopamento, width = 16, height = 20, bg = "white")


placebo_significance <- final_results_placebo %>%
  dplyr::filter(term >= 0, Iteracao != 'Original') %>% 
  dplyr::mutate(significant = ifelse(conf.low > 0 | conf.high < 0, 1, 0)) %>%
  dplyr::group_by(variable, approach) %>%
  dplyr::summarise(
    n_placebos = n(),
    n_significant = sum(significant),
    pct_significant = 100 * n_significant / n_placebos,
    .groups = "drop"
  ) 

writexl::write_xlsx(x = placebo_significance, path = 'tables/placebo_significance.xlsx')


# TESTANDO PRE TRENDS  ------------------------
# TESTE DE WALD
wald_results <- purrr::map_dfr(
  vars_to_test,
  ~ run_wald_tests(diddf = diddf, var = .x)
) %>% 
  tidyr::pivot_wider(names_from = 'Variavel', values_from = 'p_valor')

writexl::write_xlsx(x = wald_results, path = 'tables/wald_results.xlsx')

# CLEAN ENVIRONMENT (EXCEPT FOR FUNCTIONS SOURCED) ------------------------
base::rm(list = setdiff(ls(), lsf.str()))

