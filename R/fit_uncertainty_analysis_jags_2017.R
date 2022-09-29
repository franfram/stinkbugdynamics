#' @title Fit jags model with 2017 data for making uncertainty analysis
#' @description Use datasets created with 
#'   create_uncertainty_analysis_input_data_2017() and fit a model to each one
#'   of them, using jags missing data imputation to forecast the last-week of
#'   each locality. Retrieve results.
#' @return `list` of size n_localities with the posterior samples
#' from each model fit. 
#' @param ua_data target data to be used for fitting the models
#' @param useful_objects_2017
#' @param wrangled_stinkbug_data_2017
#' @param forecast_data
#' @param model_name possible arguments: M1, M2, M3, M1_b0, M2_b0
#' @param model_file path to model file. jagsUI::autojags() function argument 
#' @param n_chains jagsUI::autojags() function argument
#' @param iter_increment jagsUI::autojags() function argument
#' @param n_burnin jagsUI::autojags() function argument
#' @param n_thin jagsUI::autojags() function argument
#' @param max_iter jagsUI::autojags() function argument
#' @param n_localities scalar, amount of localities to evalute
#' @param sample_size vector with sample sizes to be evaluated
#' @param n_repl vector, 1:amount_of_replicates 

fit_uncertainty_analysis_jags_2017 <-  function(
  ua_data,
  useful_objects_2017,
  wrangled_stinkbug_data_2017,
  model_name,
  model_file, 
  n_chains,
  iter_increment,
  n_burnin,
  n_thin,
  max_iter,
  n_localities,
  sample_size,
  n_repl
) {
  
  
  
  #time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  #time_series_length_forecast <- useful_objects_2017[['for_jags']][['time_series_length_forecast']]
  
  #Localidades <- unique(wrangled_stinkbug_data_2017$Localidad)
  #localities <- Localidades
  
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  
  
 # model_input_forecastvsdata_data_2017 <- forecast_data
  
  ## save ECMWF and phen vars. Since they are all the same across the list
  ## list elements, it doesn't matter which list we grab (here '1')
  # T.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['T.s.ECMWF']]
  # DP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['DP.s.ECMWF']]
  # TP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['TP.s.ECMWF']]
  # VR1.2.3 <- model_input_forecastvsdata_data_2017[[1]][['VR1.2.3']]
  # R4.5.6.7 <- model_input_forecastvsdata_data_2017[[1]][['R4.5.6.7']]
  # R8 <- model_input_forecastvsdata_data_2017[[1]][['R8']]


  pars <- useful_objects_2017[['for_jags']][['pars_jags']]
  
  #sem_menos <- 1
  
  #semanasLoc <- time_series_length
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
  
  
  
  
  # Inits according to model used. 
      
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
 
  
 
  
  
  
  
   postdata_list <- list() 
  
  
  
    
   for (k in 1:n_localities) {
    postdata_list[[k]] <- list()
      for (s in seq_along(sample_size)) {
        postdata_list[[k]][[s]] <- list() 
         
        for (r in seq_along(n_repl)) {
          postdata_list[[k]][[s]][[r]] <- list()
       
        }
      }
    }
   
      
  # model fitting  
  for (k in 1:n_localities) { 
    for (s in seq_along(sample_size)) {
      for (r in seq_along(n_repl)) {
       
        
        
        data.V1.EqNv.dat <- ua_data[[k]][[s]][[r]]
       
        
        init_X.A.EqNv.dat <- as.matrix(
          ua_data[[k]][[s]][[r]][['Y.A.EqNv.dat']][1:12, ]
        ) 
     
    
    
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
        
        
        
        samples <- jags_fit$samples
        
        
        
        postdata <- bind_rows(
          as_tibble(samples[[1]]),
          as_tibble(samples[[2]]),
          as_tibble(samples[[3]])
        )
        
        
        # Grab a small amout of posterior samples to avoid using too much RAM
        samples <- 5e3
        
        
        # we samlpe "samplesplot" rows from the posterior
        postdata_samples <- postdata[sample(nrow(postdata), samples), ]
        

        
        postdata_list[[k]][[s]][[r]] <- postdata_samples
        
         
      }
    }
    
    
    
   
    
   
      
    
    
  }
  
  return(postdata_list)
  
}













