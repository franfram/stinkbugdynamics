#' @title Model averaging weights
#' @description get model averaging weights using the following methods:
#'   stacking, 
#'   pseudo-BMA
#'   pseudo-BMA with bootstrap
#' @return `tibble` with model weights. 
#' @param data_list, a named list containing loo scores coming from 
#'   loo_model_*_2017 targets. Example:
#'     data_list = list(
#'       "M1_b0" = loo_model_M1_b0_2017, 
#'       "M2" = loo_model_M2_2017,
#'       "M3" = loo_model_M3_2017
#'     ) 
compute_model_weights_2017 <- function(
  data_list
) {
  
  lpd_point <- data_list %>% 
    ## extract pointwise loglik values for each model
    map(~ pluck(.x, 'pointwise')[, 'elpd_loo']) %>% 
    ## reshape for loo:: functions
    as_tibble %>% as.matrix
  
  
  
  ## Merge model averaging results and round
  results <- cbind(
    # Bayesian stacking weights. 
    'stacking_wts' = loo::stacking_weights(lpd_point),
    # Pseudo-BMA weights without Bayesian bootstrap
    'pbma_wts' = loo::pseudobma_weights(lpd_point, BB = FALSE),
    # Pseudo-BMA+ weights (with bootstrap)
    'pbma_BB_wts' = loo::pseudobma_weights(lpd_point)
  ) %>% 
    round(digits = 3)
  
  
  
  ## add rownames
  names <- colnames(lpd_point)
  rownames(results) <- names
  
  ## rownames to column
  results <- results %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'models') %>% 
    as_tibble()
  
  
  ## return
  return(results)
  
}
