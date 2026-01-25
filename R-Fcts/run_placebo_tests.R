# A function that creates directories if they do not already exist and cleans up the files if so
# Author:
# Last script update: 2024-05-30

run_placebo_tests <- function(placebo_df, vars_to_test, iter = 5, seed = 123){
  # creates directories if they do not already exist and cleans up the files if so
  # ARGS:
  #   dir_path - the new directory path
  #   
  # Returns: 
  #   a new clean directory in the entered directory path
  
  base::set.seed(seed)
  
  placebo_results <- list()
  
  for (i in 1:iter){
    
    base::print(base::paste0('Iteração: ', i, ' (', round((i/iter)*100, 2), '%)'))
    
    setores <- placebo_df %>%
      dplyr::distinct(cnae)
    
    n <- 237 # NÚMERO TOTAL DE SETORES
    
    n_2012 <- 98 # SETORES TRATADOS EM 2012
    n_2013 <- 84 # SETORES TRATADOS EM 2013
    
    setores_placebo <- setores %>%
      dplyr::mutate(
        u = stats::runif(n)
      ) %>%
      dplyr::arrange(u) %>%
      dplyr::mutate(
        first_treat_placebo = case_when(
          row_number() <= n_2012 ~ 2012,
          row_number() <= (n_2012 + n_2013) ~ 2013,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(-u)
    
    df_placebo <- placebo_df %>%
      dplyr::left_join(setores_placebo, by = "cnae") %>%
      dplyr::mutate(
        finalpolicy = ifelse(
          !is.na(first_treat_placebo) & Ano >= first_treat_placebo,
          1, 0
        )
      )
    
    sum_by <- df_placebo %>%
      dplyr::select(cnae, Ano, finalpolicy) %>%
      dplyr::group_by(cnae) %>%
      dplyr::summarise(sum_amount = sum(finalpolicy, na.rm = TRUE), .groups = "drop")
    
    
    df_placebo <- df_placebo %>% 
      dplyr::left_join(sum_by, by = "cnae") %>% 
      dplyr::mutate(
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
    
    placebo_i <- purrr::map_dfr(
      vars_to_test,
      ~ run_did_models(diddf = df_placebo, var = .x)
    ) %>% 
      dplyr::mutate(Iteracao = i) 
    
    placebo_results[[paste0("Iter", i)]] <- placebo_i
    
    
  }
  
  final_results_placebo <- base::do.call(what = dplyr::bind_rows, args = placebo_results)
  
  return(final_results_placebo)
}
