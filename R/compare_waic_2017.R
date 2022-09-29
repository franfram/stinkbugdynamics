#' @title Compare models using WAIC scores.
#' @description compare predictive scores using loo_compare from 'loo' package 
#' @return `tibble` with loo scores. 
#' @param waic_list named `list` with waic scores targets waic_model_*_2017  
compare_waic_2017 <- function(
  waic_list
){
  
  
  model_comp <- waic_list %>% 
    loo_compare %>% 
    as.data.frame %>% 
    rownames_to_column(var = 'Model') %>% 
    mutate(
      across(
        where(is.numeric),
        round, 
        2
      )
    )
  
  
  return(model_comp) 
  
  
}  
  