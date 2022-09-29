#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param waic_model_comparison_2017
render_waic_comparison_table <- function(waic_model_comparison_2017) {

 
  
  
  # Customize table
  ## We will remove rows such that we only show M1_b0, M2 and M3 models
  keep <- c("M1_b0", "M2", "M3")

  waic_model_comparison_2017_modified <- waic_model_comparison_2017 %>% 
    # Filter rows 
    filter(!Model == "M1")  %>% 
    filter(!Model == "M2_b0") %>% 
    # Rename row for paper publication
    mutate(Model = recode(
      Model, 
      'M1_b0' = 'M1'
    )
    ) 







  # Render table
  table_waic_comparison  <- waic_model_comparison_2017_modified %>% 
    kable() %>% 
    kable_styling()




  return(
    table_waic_comparison
  )
  


}
