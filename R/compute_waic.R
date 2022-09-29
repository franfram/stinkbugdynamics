#' @title Compute Widely Applicable Information Criterion
#' @description Computes Widely Applicable Information Criterion for a given model 
#'   variant
#' @return `list`, output of 'loo' package 
#' @param jags_draws
compute_waic <- function(jags_draws){
  # select Log Likelihood (ll) samples from model draws
  jags_draws_loglik <- jags_draws %>%
    select(starts_with("loglik")) %>%
    as.matrix()
  
  # compute WAIC
  waic_jags <- loo::waic(
    jags_draws_loglik#,
    # save_psis = TRUE
  )
  
  return(waic_jags)  
  
  
}
