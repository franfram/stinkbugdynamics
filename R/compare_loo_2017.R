#' @title Compare models using loo scores.
#' @description compare predictive scores using loo_compare from 'loo' package 
#' @return `tibble` with loo scores. 
#' @param loo_list named `list` with loo scores targets loo_model_*_2017  
compare_loo_2017 <- function(
  loo_list
){
  
  
  model_comp <- loo_list %>%
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

