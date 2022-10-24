
# Renv setup
renv::activate()

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

# fnmate setup
options(fnmate_searcher = "git_grep")
options(fnmate_quote_jump_regex = TRUE) 

## Options
options(tidyverse.quiet = TRUE)



tar_plan(

  # Input files

  ## Stinkbug 2017 data
  tar_target(
    raw_stinkbug_data_file_2017, 
    "data/raw_stinkbug_data_2017.csv",
    format = "file"
  ),

  ## ECMWF 2017 data
  tar_target(
    raw_ecmwf_data_file_2017, 
    "data/raw_ecmwf_data_2017.csv", 
    format = "file"
  ),

  ## NOAA 2017 data
  tar_target(
    raw_noaa_data_file_2017, 
    "data/raw_NOAA_data_2017.csv", 
    format = "file"
  ), 

  # Read and wrangle data
  ## Stinkbug 2017 
  wrangled_stinkbug_data_2017 = read_and_wrangle_stinkbug_2017(raw_stinkbug_data_file_2017), 

  ## NOAA 2017
  wrangled_noaa_data_2017 = read_and_wrangle_noaa_2017(raw_noaa_data_file_2017), 
  
  ## ECMWF 2017
  wrangled_ecmwf_data_2017 = read_and_wrangle_ecmwf_2017(raw_ecmwf_data_file_2017), 
  
  # Merge data-sets
  merged_sb_noaa_ecmwf_data_2017 = merge_sb_noaa_ecmwf_data_2017(
      wrangled_stinkbug_data_2017, 
      wrangled_noaa_data_2017,
      wrangled_ecmwf_data_2017
  ),

  
  # Prepare data for JAGS run
  ## Useful JAGS objects
  useful_objects_2017 = create_useful_objects_2017(merged_sb_noaa_ecmwf_data_2017),
  

  ## Create JAGS input data 
  model_input_data_2017 = create_jags_input_data_2017(
      merged_sb_noaa_ecmwf_data_2017,
      useful_objects_2017
  ),




  # JAGS run
  jagstargets::tar_jags(
    jags_2017,
    jags_files = c(
      here("code", "models", "model_M1.jags"),
      here("code", "models", "model_M2.jags"),
      # here("code", "models", "model_M2_N.jags"),
      here("code", "models", "model_M3.jags"),
      here("code", "models", "model_M3v2.jags"),
      here("code", "models", "model_M3v3.jags"),
      # here("code", "models", "model_M3_N.jags"),
      here("code", "models", "model_M1_b0.jags"),
      # here("code", "models", "model_M1_b0_N.jags"),
      here("code", "models", "model_M2_b0.jags")
    ),
    parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
    data = model_input_data_2017,
    n.chains = 3,
    n.iter = 5e4,
    n.burnin = 2e4,
    n.thin = 4,
    #inits = tar_read(useful_objects_2017)[['for_jags']][['model_inits']][['other']]
    inits = function(model_input_data_2017) {
      list(
        bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
        bTP.A.EqNv.dat = stats::rnorm(8, 0, 1),
        bDP.A.EqNv.dat = stats::rnorm(8, 0, 1),
        #I.EqNv.dat = stats::runif(8, 0, 10),
        pobs.EqNv.dat = stats::rbeta(1, 8, 2.5),
        #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
        X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
      )
    }
  ),

  # Posterior Predictive Checks (2017)
  ## M1 model 
  post_check_M1_2017 = compute_posterior_predictive_check(
      jags_draws = jags_2017_draws_model_M1,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      model_name = "M1"
  ), 

  ## M2 model
  post_check_M2_2017 = compute_posterior_predictive_check(
      jags_draws = jags_2017_draws_model_M2,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      model_name = "M2"
  ),

  ## M2_N model
  # post_check_M2_N_2017 = compute_posterior_predictive_check(
  #     jags_draws = jags_2017_draws_model_M2,
  #     useful_objects_2017,
  #     merged_sb_noaa_ecmwf_data_2017,
  #     model_name = "M2_N"
  # ), 

  ## M3 model
  post_check_M3_2017 = compute_posterior_predictive_check(
      jags_draws = jags_2017_draws_model_M3,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      model_name = "M3"
  ), 
  # ## M3v2 model
  # post_check_M3v2_2017 = compute_posterior_predictive_check(
  #     jags_draws = jags_2017_draws_model_M3v2,
  #     useful_objects_2017,
  #     merged_sb_noaa_ecmwf_data_2017,
  #     model_name = "M3v2"
  # ), 
  # ## M3v3 model
  # post_check_M3v3_2017 = compute_posterior_predictive_check(
  #     jags_draws = jags_2017_draws_model_M3v3,
  #     useful_objects_2017,
  #     merged_sb_noaa_ecmwf_data_2017,
  #     model_name = "M3v3"
  # ), 
  # ## M3_N model
  # post_check_M3_N_2017 = compute_posterior_predictive_check(
  #     jags_draws = jags_2017_draws_model_M3,
  #     useful_objects_2017,
  #     merged_sb_noaa_ecmwf_data_2017,
  #     model_name = "M3_N"
  # ), 

  ## M1_b0 model
  post_check_M1_b0_2017 = compute_posterior_predictive_check(
      jags_draws = jags_2017_draws_model_M1_b0,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      model_name = "M1_b0"
  ), 

  ## M1_b0_N model
  # post_check_M1_b0_N_2017 = compute_posterior_predictive_check(
  #     jags_draws = jags_2017_draws_model_M1_b0,
  #     useful_objects_2017,
  #     merged_sb_noaa_ecmwf_data_2017,
  #     model_name = "M1_b0_N"
  # ), 

  # M2_b0 model
  post_check_M2_b0_2017 = compute_posterior_predictive_check(
      jags_draws = jags_2017_draws_model_M2_b0,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      model_name = "M2_b0"
  ), 

  # Plot posterior parameters 
  ## M1_b0
  # plots_posterior_params_M1_b0 = plot_post_params(
  #   jags_draws = jags_2017_draws_model_M1_b0,
  #   useful_objects_2017,
  #   merged_sb_noaa_ecmwf_data_2017, 
  #   model_name = "M1_b0"
  # ), 
  ## M3
  plots_posterior_params_M3 = plot_post_params(
    jags_draws = jags_2017_draws_model_M3,
    useful_objects_2017,
    merged_sb_noaa_ecmwf_data_2017, 
    model_name = "M3"
  ), 

  # Summary tables
  ## M1 model
  table_summary_M1_b0 = render_summary_table(
    jags_summary = jags_2017_summary_model_M1_b0,
    useful_objects_2017,
    model_name = "M1_b0"
  ),

  ## M2 model
  table_summary_M2 = render_summary_table(
    jags_summary = jags_2017_summary_model_M2,
    useful_objects_2017,
    model_name = "M2"
  ),
  
  ## M3 model
  table_summary_M3 = render_summary_table(
    jags_summary = jags_2017_summary_model_M3,
    useful_objects_2017,
    model_name = "M3"
  ),

  # LOO-CV (2017)
  ## M1 model
  loo_model_M1_2017 = compute_loo(jags_2017_draws_model_M1), 

  ## M2 model
  loo_model_M2_2017 = compute_loo(jags_2017_draws_model_M2),

  ## M3 model
  loo_model_M3_2017 = compute_loo(jags_2017_draws_model_M3),

  # ## M3v2 model
  # loo_model_M3v2_2017 = compute_loo(jags_2017_draws_model_M3v2),

  # ## M3v3 model
  # loo_model_M3v3_2017 = compute_loo(jags_2017_draws_model_M3v3),
  
  ## M1_b0 model
  loo_model_M1_b0_2017 = compute_loo(jags_2017_draws_model_M1_b0), 

  ## M2_b0 model 
  loo_model_M2_b0_2017 = compute_loo(jags_2017_draws_model_M2_b0), 
  
  ## M1_b0_N model
  # loo_model_M1_b0_N_2017 = compute_loo(jags_2017_draws_model_M1_b0_N), 
  
  ## M2_N model
  # loo_model_M2_N_2017 = compute_loo(jags_2017_draws_model_M2_N), 
  
  ## M3_N model
  # loo_model_M3_N_2017 = compute_loo(jags_2017_draws_model_M3_N), 

 # LOO-CV Compare
  loo_model_comparison_2017 = compare_loo_2017(
      list(
        # 'M1' = loo_model_M1_2017,
        'M2' = loo_model_M2_2017,
        'M3' = loo_model_M3_2017,
        # 'M3v2' = loo_model_M3v2_2017,
        # 'M3v3' = loo_model_M3v3_2017,
        'M1_b0' = loo_model_M1_b0_2017#,
        # 'M2_b0' = loo_model_M2_b0_2017#,
        #'M1_b0_N' = loo_model_M1_b0_N_2017,
        #'M2_N' = loo_model_M2_N_2017,
        #'M3_N' = loo_model_M3_N_2017
        
      )
  ), 
  ## Render LOO-CV Compare table
  table_loo_comparison_2017 = render_loo_comparison_table(
    loo_model_comparison_2017
  ),
  
 
  # Model averaging using LOO weights
  loo_model_weights_2017 = compute_model_weights_2017(
      data_list = list(
        "M1_b0" = loo_model_M1_b0_2017,
        #"M1_b0_N" = loo_model_M1_b0_N_2017,
        "M2" = loo_model_M2_2017,
        #"M2_N" = loo_model_M2_N_2017,
        "M3" = loo_model_M3_2017#,
        # "M3v2" = loo_model_M3v2_2017,
        # "M3v3" = loo_model_M3v3_2017#,
        #"M3_N" = loo_model_M3_N_2017
      )
  ), 

  # # WAIC 
  # ## M1 model
  # waic_model_M1_2017 = compute_waic(jags_2017_draws_model_M1), 

  # ## M2 model
  # waic_model_M2_2017 = compute_waic(jags_2017_draws_model_M2), 
  
  # ## M3 model
  # waic_model_M3_2017 = compute_waic(jags_2017_draws_model_M3), 

  # ## M1_b0 model
  # waic_model_M1_b0_2017 = compute_waic(jags_2017_draws_model_M1_b0), 
  
  # ## M2_b0 model
  # waic_model_M2_b0_2017 = compute_waic(jags_2017_draws_model_M2_b0), 
  
  # # WAIC Compare
  # waic_model_comparison_2017 = compare_waic_2017(
  #   list(
  #     "M1" = waic_model_M1_2017,
  #     "M2" = waic_model_M2_2017,
  #     "M3" = waic_model_M3_2017,
  #     "M1_b0" = waic_model_M1_b0_2017,
  #     "M2_b0" = waic_model_M2_b0_2017
  #   )
  # ), 


  # ## Render waic-CV Compare table
  # table_waic_comparison_2017 = render_waic_comparison_table(
  #   waic_model_comparison_2017
  # ),

  # Last week forecast
  ## Create Input
  model_input_lastweek_forecast_data_2017 = create_lastweeks_forecast_input_data_2017(
      raw_stinkbug_data_file_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      merged_sb_noaa_ecmwf_data_2017,
      weeks_forecasted = 1
  ), 

  # Fit models
  ## M1 model
  lastweek_forecast_jagsfit_M1_2017 = fit_lastweeks_forecast_jags_2017(
      forecast_data = model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_name = "M1",
      model_file = here("code", "models", "model_M1.jags"),
      n_chains = 3,
      iter_increment = 5e4,
      n_burnin = 2e4,
      n_thin = 4,
      max_iter = 3e5
  ), 

  ## M2 model
  lastweek_forecast_jagsfit_M2_2017 = fit_lastweeks_forecast_jags_2017(
      forecast_data = model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_name = "M2",
      model = here("code", "models", "model_M2.jags"),
      n_chains = 3,
      iter_increment = 5e4,
      n_burnin = 2e4,
      n_thin = 4,
      max_iter = 3e5
  ), 

  ### M2 N model
  # tar_target(
  #   lastweek_forecast_jagsfit_M2_N_2017,
  #   fit_lastweeks_forecast_jags_2017(
  #     forecast_data = model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M2_N",
  #     model = here("code", "models", "model_M2_N.jags"),
  #     n_chains = 3,
  #     iter_increment = 5e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5
  #   )
  # ),
  
  ## M3 model
  lastweek_forecast_jagsfit_M3_2017 = fit_lastweeks_forecast_jags_2017(
      forecast_data = model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_name = "M3",
      model = here("code", "models", "model_M3.jags"),
      n_chains = 3,
      iter_increment = 5e4,
      n_burnin = 2e4,
      n_thin = 4,
      max_iter = 3e5
  ), 
  # ## M3v2 model
  # lastweek_forecast_jagsfit_M3v2_2017 = fit_lastweeks_forecast_jags_2017(
  #     forecast_data = model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M3v2",
  #     model = here("code", "models", "model_M3v2.jags"),
  #     n_chains = 3,
  #     iter_increment = 5e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5
  # ), 
  # ## M3v3 model
  # lastweek_forecast_jagsfit_M3v3_2017 = fit_lastweeks_forecast_jags_2017(
  #     forecast_data = model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M3v3",
  #     model = here("code", "models", "model_M3v3.jags"),
  #     n_chains = 3,
  #     iter_increment = 5e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5
  # ), 

  ## M3_N model ############ callr subprocess failed: 3 nodes produced errors; first error: Error in node loglik.EqNv.dat[8, Invalid parent values
  # lastweek_forecast_jagsfit_M3_N_2017 = fit_lastweeks_forecast_jags_2017(
  #     forecast_data = model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M3_N",
  #     model = here("code", "models", "model_M3_N.jags"),
  #     n_chains = 3,
  #     iter_increment = 5e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5
  # ), 

  ## M1 b0 model
  lastweek_forecast_jagsfit_M1_b0_2017 = fit_lastweeks_forecast_jags_2017(
      forecast_data = model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_name = "M1_b0",
      model = here("code", "models", "model_M1_b0.jags"),
      n_chains = 3,
      iter_increment = 5e4,
      n_burnin = 2e4,
      n_thin = 4,
      max_iter = 4e5
  ), 

  # # M1 b0 N model
  # lastweek_forecast_jagsfit_M1_b0_N_2017 = fit_lastweeks_forecast_jags_2017(
  #     forecast_data = model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M1_b0_N",
  #     model = here("code", "models", "model_M1_b0_N_noll.jags"),
  #     n_chains = 3,
  #     iter_increment = 5e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e4#4e5
  # ),

  ## M2 b0 model
  lastweek_forecast_jagsfit_M2_b0_2017 = fit_lastweeks_forecast_jags_2017(
      forecast_data = model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_name = "M2_b0",
      model = here("code", "models", "model_M2_b0.jags"),
      n_chains = 3,
      iter_increment = 5e4,
      n_burnin = 2e4,
      n_thin = 4,
      max_iter = 3e5
  ), 

  # Compute Forecasts
  ## M1 model
  lastweek_forecast_M1_2017 = compute_lastweeks_forecast_2017(
      model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_fit = lastweek_forecast_jagsfit_M1_2017,
      weeks_forecasted = 1
  ), 

  ## M2 model
  lastweek_forecast_M2_2017 = compute_lastweeks_forecast_2017(
      model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_fit = lastweek_forecast_jagsfit_M2_2017,
      weeks_forecasted = 1
  ), 

  ## M3 model
  lastweek_forecast_M3_2017 = compute_lastweeks_forecast_2017(
      model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_fit = lastweek_forecast_jagsfit_M3_2017,
      weeks_forecasted = 1
  ),

  # ## M3v2 model
  # lastweek_forecast_M3v2_2017 = compute_lastweeks_forecast_2017(
  #     model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_fit = lastweek_forecast_jagsfit_M3v2_2017,
  #     weeks_forecasted = 1
  # ),

  # ## M3 model
  # lastweek_forecast_M3v3_2017 = compute_lastweeks_forecast_2017(
  #     model_input_lastweek_forecast_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_fit = lastweek_forecast_jagsfit_M3v3_2017,
  #     weeks_forecasted = 1
  # ),

  # M1_b0 model
  lastweek_forecast_M1_b0_2017 = compute_lastweeks_forecast_2017(
      model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_fit = lastweek_forecast_jagsfit_M1_b0_2017,
      weeks_forecasted = 1
  ), 

  ## M2_b0 model
  lastweek_forecast_M2_b0_2017 = compute_lastweeks_forecast_2017(
      model_input_lastweek_forecast_data_2017,
      useful_objects_2017,
      wrangled_stinkbug_data_2017,
      model_fit = lastweek_forecast_jagsfit_M2_b0_2017,
      weeks_forecasted = 1
  ), 
 
  #Plot Forecasts
  ## M1 model
  plots_lastweek_forecast_M1_2017 = plot_lastweeks_forecast_2017(
      forecast_data = lastweek_forecast_M1_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      weeks_forecasted = 1,
      model_name = "M1"
  ),

  ## M2 model
  plots_lastweek_forecast_M2_2017 = plot_lastweeks_forecast_2017(
      forecast_data = lastweek_forecast_M2_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      weeks_forecasted = 1,
      model_name = "M2"
  ), 

  ## M3 model
  plots_lastweek_forecast_M3_2017 = plot_lastweeks_forecast_2017(
      forecast_data = lastweek_forecast_M3_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      weeks_forecasted = 1,
      model_name = "M3"
  ), 

  # ## M3v2 model
  # plots_lastweek_forecast_M3v2_2017 = plot_lastweeks_forecast_2017(
  #     forecast_data = lastweek_forecast_M3v2_2017,
  #     wrangled_stinkbug_data_2017,
  #     useful_objects_2017,
  #     weeks_forecasted = 1,
  #     model_name = "M3v2"
  # ), 

  # ## M3v3 model
  # plots_lastweek_forecast_M3v3_2017 = plot_lastweeks_forecast_2017(
  #     forecast_data = lastweek_forecast_M3v3_2017,
  #     wrangled_stinkbug_data_2017,
  #     useful_objects_2017,
  #     weeks_forecasted = 1,
  #     model_name = "M3v3"
  # ), 
  
  ## M1_b0 model
  plots_lastweek_forecast_M1_b0_2017 = plot_lastweeks_forecast_2017(
      forecast_data = lastweek_forecast_M1_b0_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      weeks_forecasted = 1,
      model_name = "M1_b0"
  ),

  ## M2_b0 model
  plots_lastweek_forecast_M2_b0_2017 = plot_lastweeks_forecast_2017(
      forecast_data = lastweek_forecast_M2_b0_2017,
      wrangled_stinkbug_data_2017,
      useful_objects_2017,
      weeks_forecasted = 1,
      model_name = "M2_b0"
  ),

  
  ## Compare forecasts made by different models. 
  plots_lastweek_forecast_allmodels_2017 = plot_lastweeks_forecast_modelcompare_2017(
      data_list = list(
        'M1_b0' = plots_lastweek_forecast_M1_b0_2017, 
        'M2' = plots_lastweek_forecast_M2_2017, 
        'M3' = plots_lastweek_forecast_M3_2017#, 
        #'M3v2' = plots_lastweek_forecast_M3v2_2017#, 
        # 'M3v3' = plots_lastweek_forecast_M3v3_2017 

      ),
      useful_objects_2017
  ), 

  
  # uncertainty_analysis_input_data_2017 = create_uncertainty_analysis_input_data_2017_nf(
  #     merged_sb_noaa_ecmwf_data_2017,
  #     raw_stinkbug_data_file_2017,
  #     useful_objects_2017, 
  #     wrangled_stinkbug_data = wrangled_stinkbug_data_2017,
  #     sample_size = c(180, 140, 120, 100, 80, 60, 40, 20, 10),
  #     n_repl = 1:9
  # ), 
  
  # uncertainty_analysis_input_data_2017 = create_uncertainty_analysis_input_data_2017(
  #     merged_sb_noaa_ecmwf_data_2017,
  #     raw_stinkbug_data_file_2017,
  #     useful_objects_2017, 
  #     wrangled_stinkbug_data = wrangled_stinkbug_data_2017,
  #     sample_size = c(180, 140, 120, 100, 80, 60, 40, 20, 10),
  #     n_repl = 1:9
  # ), 

  # uncertainty_analysis_2017 = compute_uncertainty_analysis_2017(
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017
  # ),

  results_export = export_results(
      plots_lastweek_forecast_M1_2017,  
      plots_lastweek_forecast_M2_2017,  
      plots_lastweek_forecast_M3_2017,  
      plots_lastweek_forecast_M1_b0_2017,  
      plots_lastweek_forecast_M2_b0_2017
  ) 

)


















  # uncertainty_analysis_jagsfit_M1_b0_2017 = fit_uncertainty_analysis_jags_2017(
  #     ua_data = uncertainty_analysis_input_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M3",#"M1_b0",
  #     model = here("code", "models", "model_M3_ua.jags"),#,here("code", "models", "model_M1_b0_ua.jags"),
  #     n_chains = 4,
  #     iter_increment = 6e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5,
  #     n_localities = 8,
  #     sample_size = c(180, 140, 120, 100, 80, 60, 40, 20, 10),
  #     n_repl = 1:9
  # )




# TARGET = compute_uncertainty_analysis_2017()





  # uncertainty_analysis_jagsfit_M2_2017 = fit_uncertainty_analysis_jags_2017(
  #     ua_data = uncertainty_analysis_input_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M2",
  #     model = here("code", "models", "model_M2_ua.jags"),
  #     n_chains = 3,
  #     iter_increment = 6e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5,
  #     n_localities = 2,
  #     sample_size = c(180, 140, 120, 100, 80, 6, 40, 20, 10),
  #     n_repl = 1:2
  #   ), 

  # uncertainty_analysis_jagsfit_M3_2017 = fit_uncertainty_analysis_jags_2017(
  #     ua_data = uncertainty_analysis_input_data_2017,
  #     useful_objects_2017,
  #     wrangled_stinkbug_data_2017,
  #     model_name = "M3",
  #     model = here("code", "models", "model_M3_ua.jags"),
  #     n_chains = 3,
  #     iter_increment = 6e4,
  #     n_burnin = 2e4,
  #     n_thin = 4,
  #     max_iter = 3e5,
  #     n_localities = 2,
  #     sample_size = c(180, 140, 120, 100, 80, 6, 40, 20, 10),
  #     n_repl = 1:2
  # ), 
   
# 
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #
# #
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #       
# #   # tar_target(
# #   #   model_input_last2weeks_forecast_data_2017,
# #   #   create_lastweeks_forecast_input_data_2017(
# #   #     raw_stinkbug_data_file_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017,
# #   #     merged_sb_noaa_ecmwf_data_2017,
# #   #     weeks_forecasted = 2
# #   #   )
# #   # ),
# #   # tar_target(
# #   #   lastweek_forecast_2017,
# #   #   compute_lastweek_forecast_2017(
# #   #     model_input_forecastvsdata_data_2017,
# #   #     useful_objects_2017,
# #   #     wrangled_stinkbug_data_2017
# #   #   )
# #   # ),
# #   # tar_target(
# #   #   lastweek_forecast_jagsfit_M2_2017,
# #   #   fit_lastweeks_forecast_jags_2017(
# #   #     forecast_data = model_input_lastweek_forecast_data_2017,
# #   #     useful_objects_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     model_name = here("code", "models", "model_M2.jags"),
# #   #     n_chains = 3,
# #   #     iter_increment = 5e4,
# #   #     n_burnin = 2e4,
# #   #     n_thin = 4,
# #   #     max_iter = 3e5
# #   #   )
# #   # ),
# #   # tar_target(
# #   #   last2weeks_forecast_jagsfit_2017,
# #   #   fit_lastweeks_forecast_jags_2017(
# #   #     forecast_data = model_input_last2weeks_forecast_data_2017,
# #   #     useful_objects_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     model_name = "model_mod1_ECMWF_full.jags",
# #   #     n_chains = 3,
# #   #     iter_increment = 5e4,
# #   #     n_burnin = 2e4,
# #   #     n_thin = 4,
# #   #     max_iter = 3e5
# #   #   )
# #   # ),
# # 
# #   # tar_target(
# #   #   lastweek_forecast_M2_2017,
# #   #   compute_lastweeks_forecast_2017(
# #   #     model_input_lastweek_forecast_data_2017,
# #   #     useful_objects_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     lastweek_forecast_jagsfit_M2_2017,
# #   #     weeks_forecasted = 1
# #   #   )
# #   # ),
# #   # # tar_target(
# #   # #   last2weeks_forecast_2017,
# #   # #   compute_lastweeks_forecast_2017(
# #   # #     model_input_last2weeks_forecast_data_2017,
# #   # #     useful_objects_2017,
# #   # #     wrangled_stinkbug_data_2017,
# #   # #     last2weeks_forecast_jagsfit_2017,
# #   # #     weeks_forecasted = 2
# #   # #   )
# #   # # ),
# #   # tar_target(
# #   #   plots_lastweek_forecast_M2_2017,
# #   #   plot_lastweeks_forecast_2017(
# #   #     forecast_data = lastweek_forecast_M2_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017,
# #   #     weeks_forecasted = 1
# #   # 
# #   #   )
# #   # )
# #   # tar_target(
# #   #   plots_last2weeks_forecast_2017,
# # 
# #   
# #   
# #     #   plot_lastweeks_forecast_2017(
# #   #     forecast_data = last2weeks_forecast_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017,
# #   #     weeks_forecasted = 2
# #   # 
# #   #   )
# #   # )
# # 
# #   
# #   # tar_target(
# #   #   lastweek_forecast_plots_2017,
# #   #   plot_lastweeks_forecast_2017(
# #   #     forecast_data = lastweek_forecast_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017
# #   #   )
# #   # ),
# #   # tar_target(
# #   #   last2weeks_forecast_plots_2017,
# #   #   plot_lastweeks_forecast_2017(
# #   #     forecast_data = last2weeks_forecast_2017, ## add tar_read?
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017
# #   #   )
# #   # ),
# #   
# #   
# # 
# #   
# #   
# #   ############## forecastvsdata many targets approach fail. 
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_1,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 1)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_2,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 2)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_3,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 3)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_4,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 4)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_5,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 5)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_6,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 6)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_7,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 7)
# #   # ),
# #   # tar_target(
# #   #   model_input_forecastvsdata_data_2017_8,
# #   #   extract_forecastvsdata_k(model_input_forecastvsdata_data_2017, k = 8)
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_1,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_1,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_2,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_2,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_3,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_3,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_4,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_4,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_5,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_5,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_6,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_6,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_7,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_7,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # ),
# #   # jagstargets::tar_jags(
# #   #   jags_forecastvsdata_2017_8,
# #   #   jags_files = c(
# #   #     "model_mod1_ECMWF_full.jags"
# #   #   ),
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = model_input_forecastvsdata_data_2017_8,
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # )
# #   # 
# #   
# #   
# #   
# #   
# #   
# #   
# #   # jagstargets::tar_jags_rep_draws( ########## forecastvsdata branching approach NOT WORKING ATM
# #   #   jags_forecastvsdata_2017,
# #   #   jags_files = "model_mod1_ECMWF_full.jags",
# #   #   parameters.to.save = tar_read(model_input_data_2017)$pars,  #c("X.A.EqNv.dat"), #test the tar_read(model_input_data) thing
# #   #   data = create_forecastvsdata_input_data_2017(
# #   #     raw_stinkbug_data_file_2017,
# #   #     wrangled_stinkbug_data_2017,
# #   #     useful_objects_2017,
# #   #     merged_sb_noaa_ecmwf_data_2017
# #   #   ),
# #   #   n.chains = 3,
# #   #   n.iter = 5e3,
# #   #   n.burnin = 2e3,
# #   #   n.thin = 4,
# #   #   inits = function(model_input_data_2017) {
# #   #     list(
# #   #       bT.A.EqNv.dat = stats::rnorm(8, 0, 1),
# #   #       I.EqNv.dat = stats::runif(8, 0, 10),
# #   #       pobs.EqNv.dat = stats::runif(1),
# #   #       #X.A.EqNv.dat = tar_read(model_input_data_2017)$Y.A.EqNv.dat[1:12, ] ######## check later how to fix this should work but outputs Error: callr subprocess failed: missing files: _targets/meta/meta   Visit https://books.ropensci.org/targets/debugging.html for debugging advice.
# #   #       X.A.EqNv.dat = matrix(c(6, 1, 3, 3, 33, 85, 346, 184, NA, NA, NA, NA, 0, 0, 15, 6, 6, 13, 14, 20, 84, 98, 79, 78, 2, 1, 5, 3, 5, 3, 4, 8, 26, 25, 78, NA, 4, 1, 5, 13, 30, 64, 68, 298, NA, NA, NA, NA, 8, 15, 7, 13, 3, 7, 50, 37, NA, NA, NA, NA, 4, 5, 3, 6, 2, 5, 23, 13, 10, 15, 36, NA, 4, 4, 16, 23, 18, 52, 69, 83, 101, 112, NA, NA, 4, 2, 2, 2, 7, 31, 88, 52, 97, 122, NA, NA), nrow = 12, ncol = 8, byrow = FALSE)
# #   #     )
# #   #   }
# #   # )
# #   
# #   #tar_combine()
# # 
# #   
# #   # loo_model3 = loo_model_ECMWF_2017_2021,
# #   # loo_model5 = loo_model_ECMWF_full_2017_2021,
# #   # loo_model8 = loo_model_mod1_ECMWF_2017_2021,
# #   # loo_model10 = loo_model_mod1_ECMWF_full_2017_2021
# #   
# # 
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   # tar_target(
# #   #   
# #   # )
# #   # tar_target(
# #   #   custom_summary_mod1_NOAA,
# #   #   create_custom_jags_summary(jags_draws_model_mod1_NOAA)
# #   # )
# #   # tar_target(
# #     # custom_summary,
# #     # posterior::summarize_draws(
# #     #   dplyr::select(jags_draws_model_mod1_ECMWF, -.draw),
# #     #   mean,
# #     #   median,
# #     #   ~posterior::quantile2(.x, probs = c(0.2, 0.8)),
# #     #   ~posterior::quantile2(.x, probs = c(0.4, 0.6)),
# #     #   ~posterior::quantile2(.x, probs = c(0.05, 0.95))
# #     #)
# #   #)
# # 
# #   
# #   
# #   
#  
# 
