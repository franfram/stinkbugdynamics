#' @title Compute Leave-One-Out Cross-Validation
#' @description Computes Leave-One-Out Cross-Validation for a given model 
#'   variant
#' @return `list`, output of 'loo' package 
#' @param jags_draws
compute_loo <- function(jags_draws){
  # select Log Likelihood (ll) samples from model draws
  jags_draws_loglik <- jags_draws %>%
    select(starts_with("loglik")) %>%
    as.matrix()
  
  # compute LOO
  loo_jags <- loo::loo(jags_draws_loglik)
  
  
  return(loo_jags)  
  
  
}
