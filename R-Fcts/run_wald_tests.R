run_wald_tests <- function(diddf, var){
  # creates directories if they do not already exist and cleans up the files if so
  # ARGS:
  #   dir_path - the new directory path
  #   
  # Returns: 
  #   a new clean directory in the entered directory path

  # teste de Wald PARA TWFE
  
  twfe_es <- fixest::feols(stats::as.formula(paste0(var,'~ i(time_to_treat, treated, ref = -1) | factor(cnaeidnum) + factor(Ano)')), vcov_cluster, data = diddf)
  
  coef_names <- base::names(coef(twfe_es))
  
  pre_coef_names <- coef_names[grep("-", coef_names)]
  
  # Teste Wald conjunto
  twfe_p <- fixest::wald(twfe_es, pre_coef_names)$p
  
  # TESTE DE WALD PARA ETWFE
  
  etwfe_model <- etwfe::etwfe(
    fml = as.formula(paste(var, "~ 1")),
    tvar = Ano,
    gvar = first_treat,
    data = diddf,
    vcov = ~cnaeidnum,
    cgroup = "never"
  )
  
  etwfe_es <- etwfe::emfx(etwfe_model, type = "event")
  
  pre_att <- etwfe_es$estimate[etwfe_es$event  < -1] 
  
  pre_se  <- etwfe_es$std.error[etwfe_es$event < -1]
  
  wald_stat <- base::sum((pre_att / pre_se)^2, na.rm = TRUE)
  
  etwfe_p <- 1 - stats::pchisq(wald_stat, df = length(pre_att))
  
  
  
  # teste Wald para SA
  sa_model <- fixest::feols(
    stats::as.formula(
      base::paste0(
        var, '~ sunab(year_treated, Ano, ref.p = -1) | ',
        'factor(cnaeidnum) + factor(Ano)'
      )
    ),
    cluster = ~ cnaeidnum,
    data = diddf
  )
  
  coef_names <- base::names(coef(sa_model))
  
  # Filtrar apenas os pré-trends (leads < 0)
  pre_coef_names <- coef_names[grep("-", coef_names)]
  
  # Teste Wald conjunto
  sa_p <- fixest::wald(sa_model, pre_coef_names)$p
  
  
  # TESTE DE WALD PARA CS
  # 4. Callaway & Sant'anna
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
  
  pre_att <- csa_es$att[csa_es$egt < 0] 
  
  pre_se  <- csa_es$se[csa_es$egt < 0]
  
  wald_stat <- base::sum((pre_att / pre_se)^2, na.rm = TRUE)
  cs_p   <- 1 - stats::pchisq(wald_stat, df = base::length(pre_att))
  
  wald_df <- base::data.frame(
    Modelo = c("TWFE", "ETWFE", "SA", "CS"),
    Variavel = var,
    p_valor = c(twfe_p, etwfe_p, sa_p, cs_p)
  )
  
  return(wald_df)

}
