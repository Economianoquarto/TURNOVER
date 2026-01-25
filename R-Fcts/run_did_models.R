# A function that creates directories if they do not already exist and cleans up the files if so
# Author:
# Last script update: 2024-05-30

run_did_models <- function(diddf, var){
  # creates directories if they do not already exist and cleans up the files if so
  # ARGS:
  #   dir_path - the new directory path
  #   
  # Returns: 
  #   a new clean directory in the entered directory path
  
  # FÓRMULAS GERAIS DOS MODELOS
  formula_feols <- stats::as.formula(base::paste(var, "~ i(time_to_treat, treated, ref = -1) | factor(cnaeidnum) + factor(Ano)"))
  formula_sa <- stats::as.formula(base::paste(var, "~ sunab(year_treated, Ano, ref.p=-1) | factor(cnaeidnum) + factor(Ano)"))
  
  # EVENT STUDY TWFE
  
  twfe_es <- fixest::feols(formula_feols, vcov_cluster, data = diddf)
  tidy_twfe <- broom::tidy(twfe_es, conf.int = TRUE)
  
  tidy_twfe <- tidy_twfe %>%
    dplyr::mutate(term = ifelse(term == 'time_to_treat::-6:treated', -6, 
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
    dplyr::mutate(approach = "TWFE", variable = var)
  
  # SUN AND ABRAHAM
  sa_model <- fixest::feols(formula_sa, vcov_cluster, data = diddf)
  tidy_sa <- broom::tidy(sa_model, conf.int = TRUE)
  
  tidy_sa <- tidy_sa %>%
    dplyr::mutate(term = ifelse(term == 'Ano::-6', -6, 
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
    dplyr::mutate(approach = "Sun & Abraham", variable = var)
  
  # Callaway & Sant'anna
  csa_model <- did::att_gt(yname = var,
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
  
  csa_es <- did::aggte(csa_model, type = "dynamic")
  tidy_csa <- broom::tidy(csa_es)
  
  tidy_csa <- tidy_csa %>%
    dplyr::mutate(term = ifelse(term == 'ATT(-6)', -6, 
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
    dplyr::mutate(approach = "Callaway & Sant'anna", variable = var)
  
  # 5. ETWFE
  etwfe_model <- etwfe::etwfe(fml = stats::as.formula(paste(var, "~ 1")),
                       tvar = Ano,
                       gvar = first_treat,
                       data = diddf,
                       vcov = ~cnaeidnum,
                       cgroup = "never")
  
  etwfe_es <- etwfe::emfx(etwfe_model, type = "event")
  tidy_etwfe <- broom::tidy(etwfe_es)
  
  tidy_etwfe <- tidy_etwfe %>%
    dplyr::select(-term) %>%
    dplyr::rename(term = event)
  
  tidy_etwfe <- tidy_etwfe %>% 
    dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
    dplyr::mutate(approach = "ETWFE", variable = var)
  
  # COMBINANDO TODOS OS RESULTADOS
  var_results <- base::rbind(tidy_twfe, tidy_sa, tidy_csa, tidy_etwfe)
  
  return(var_results)
  
}
