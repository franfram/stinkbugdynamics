#' @title Fit jags model with 2017 data for making last-weeks forecasts
#' @description Use datasets created with create_forecastvsdata_input_data_2017
#' to fit 'length(Localidades)' models, where the missing observation (from last
#' week) of each locality will be estimated by JAGS (due to JAGS' automatic 
#' missing data imputation) 
#' @return `list` of size 'length(Localidades)' with the posterior samples
#' from each model fit. 
#' @param forecast_data Data to be used for model fitting. 
#`   e.g., model_input_forecastvsdata_data_2017 
#' @param useful_objects_2017
#' @param wrangled_stinkbug_data_2017
#' @param model_name possible arguments: M1, M2, M3, M1_b0, M2_b0
#' @param model_file path to model file. jagsUI::autojags() function argument 
#' @param n_chains jagsUI::autojags() function argument
#' @param iter_increment jagsUI::autojags() function argument
#' @param n_burnin jagsUI::autojags() function argument
#' @param n_thin jagsUI::autojags() function argument
#' @param max_iter jagsUI::autojags() function argument

fit_lastweeks_forecast_jags_2017 <- function(
  forecast_data,
  useful_objects_2017,
  wrangled_stinkbug_data_2017,
  model_name,
  model_file, 
  n_chains,
  iter_increment,
  n_burnin,
  n_thin,
  max_iter
) {
  Localidades <- unique(wrangled_stinkbug_data_2017$Localidad)
  
  
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  #time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  
  model_input_forecastvsdata_data_2017 <- forecast_data
  
  ## save ECMWF and phen vars. Since they are all the same across the list
  ## list elements, it doesn't matter which list we grab (here '1')
  # T.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['T.s.ECMWF']]
  # DP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['DP.s.ECMWF']]
  # TP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['TP.s.ECMWF']]
  # VR1.2.3 <- model_input_forecastvsdata_data_2017[[1]][['VR1.2.3']]
  # R4.5.6.7 <- model_input_forecastvsdata_data_2017[[1]][['R4.5.6.7']]
  # R8 <- model_input_forecastvsdata_data_2017[[1]][['R8']]
  # nFenologia <- model_input_forecastvsdata_data_2017[[1]][['nFenologia']]
  # # 
  pars <- useful_objects_2017[['for_jags']][['pars_jags']]
  
   
  # sem_menos <- 1
  # 
  # semanasLoc <- time_series_length
  # loc_name <- list()
  # 
  # datos.forecast.EqNv.dat <- list()
  # 
  # lista.datos.forecast.EqNv.dat <- tibble()
  # 
  # 
  # l.X.A.F1.EqNv.dat <- list()
  # l.Y.A.F1.EqNv.dat <- list()
  # X.A.F1.EqNv.dat.planta <- list()
  # Y.A.F1.EqNv.dat.planta <- list()

  
  postdata_mod1_list <- list()
  
  for (k in seq_along(Localidades)){ 
    
    data.V1.EqNv.dat <- model_input_forecastvsdata_data_2017[[k]]
    init_X.A.EqNv.dat <- as.matrix(model_input_forecastvsdata_data_2017[[k]][['Y.A.EqNv.dat']][1:12, ]) ####### removed the as.matrix(), try adding it back if there's a bug
    init_Y.N.EqNv.dat <- as.matrix(model_input_forecastvsdata_data_2017[[k]][['Y.N.EqNv.dat']][1:12, ])
    
    ## Will provide custom inits for each model to avoid dimension mismatches
    ## and to improve model fitting (mainly to avoid the re-running of a model
    ## when the convergece check fails for a given parameter). 
    
    "
    todo esto lo podes pasar al useful_objects_2017 target
    "
    
    
    
    if (model_name == "M1") {
      inits.V1.EqNv.dat <- function() list(
        bF.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        I.EqNv.dat = runif(nLocalidades, 0, 5),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )
    } else if (model_name == "M2") {
      inits.V1.EqNv.dat <- function() list(
        bF.VR1.2.3.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bF.R4.5.6.7.EqNv.dat = rnorm(nLocalidades, 0.5, 1),
        bF.R8.EqNv.dat = rnorm(nLocalidades, -0.5, 1), 
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        I.EqNv.dat = runif(nLocalidades, 0, 5),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )
    } else if (model_name == "M3") {
      inits.V1.EqNv.dat <- function() list(
        bF.VR1.2.3.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bF.R4.5.6.7.EqNv.dat = rnorm(nLocalidades, 0.5, 1),
        bF.R8.EqNv.dat = rnorm(nLocalidades, -0.5, 1), 
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        # missing I.EqNv.dat init to avoid dimension mismatch. 
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )
    } else if (model_name == "M1_b0") {
      inits.V1.EqNv.dat <- function() list(
        b0.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bF.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        I.EqNv.dat = runif(nLocalidades, 0, 5),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )  
    } else if (model_name == "M1_b0_N") {
      inits.V1.EqNv.dat <- function() list(
        b0.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bF.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        I.EqNv.dat = runif(nLocalidades, 0, 5),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat, ############################ removed the as.matrix(), try adding it back if there's a bug  
        bN.EqNv.dat = rnorm(nLocalidades, 0, 1)
      )  
    } else if (model_name == "M2_b0") {
      inits.V1.EqNv.dat <- function() list(
        b0.EqNv.dat = rnorm(nLocalidades, 0, 1), 
        bF.VR1.2.3.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bF.R4.5.6.7.EqNv.dat = rnorm(nLocalidades, 0.5, 1),
        bF.R8.EqNv.dat = rnorm(nLocalidades, -0.5, 1), 
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        I.EqNv.dat = runif(nLocalidades, 0, 5),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )  
    } else {
      inits.V1.EqNv.dat <- function() list(
        bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
        pobs.EqNv.dat = rbeta(1, 8, 2.5),
        X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
      )  
    }
    

    
    
    jags_fit <- jagsUI::autojags(
      data = data.V1.EqNv.dat,
      inits = inits.V1.EqNv.dat,
      parameters.to.save = pars,
      model.file = model_file,
      n.chains = n_chains, #3,
      iter.increment = iter_increment, #50000,
      n.burnin = n_burnin,# 20000,
      n.thin = n_thin, #4,
      max.iter = max_iter, # 300000, # antes max.iter = 300000
      parallel = TRUE
    )
    
    
    
    samples_mod1 <- jags_fit$samples
    
    
    
    postdata_mod1 <- bind_rows(as_tibble(samples_mod1[[1]]),
                               as_tibble(samples_mod1[[2]]),
                               as_tibble(samples_mod1[[3]]))
    
    
    
    postdata_mod1_list[[k]] <- postdata_mod1
    
    
  
    
  }
  
  return(postdata_mod1_list)
  
}
