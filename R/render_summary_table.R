#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title Render the summary table for the posterior parameters of the model
#' @param jags_draws
#' @param useful_objects_2017
#' @param model_name
render_summary_table <- function(
  jags_summary, 
  useful_objects_2017, 
  model_name = "M1_b0"
) {
  

  # read argument functions as tar_read
  useful_objects_2017 <- tar_read(useful_objects_2017)
  
  if (model_name == "M1_b0") {
    jags_summary <- tar_read(jags_2017_summary_model_M1_b0) 
  } else if (model_name == "M2") {
    jags_summary <- tar_read(jags_2017_summary_model_M2) 
  } else if (model_name == "M3") {
    jags_summary <- tar_read(jags_2017_summary_model_M3) 
  } else {
    print("Model supplied not included in code. ")
  }



  ## Customize jags_summary output
  # parameters we don't want for the table
  remove <- c(
    "loglik",
    "X.A.",
    "lambda",
    'deviance'
  )


  jags_summary_clean <- jags_summary %>% 
   # Remove parameters
   filter(!str_detect(variable, paste(remove, collapse = '|'))) %>% 
   # Rename columns
   rename(
     'parameter' = 'variable'
   ) %>% 
   # Select cols
   #select(-c('.join_data', 'mad', 'ess_bulk', 'ess_tail', 'median'))
   select(-c('.join_data', 'mad', 'median'))



  # htmlTable(jags_summary_clean) %>%
  # save_kable(file = "test.png")
  # Kable
  jags_summary_kable <- jags_summary_clean %>% 
    kable() 



  # Save kable
  # save_kable(
  #   jags_summary_kable, 
  #   file = here("output", "tables", "jags_summary_kable.png")
  # )


  return(
    jags_summary_kable
  )



}
