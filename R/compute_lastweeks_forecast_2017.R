#' @title Compute forecast for last weeks of the time-series
#' @description Compute the forecast for the last 'weeks_forecasted' weeks of
#'   the time-series. The function works for every model variant, as long as
#'   the observation function is the same. Have to provide a value to the
#'   'weeks_forecasted' argument to specify how many weeks, starting from the
#'   end of the time series, you want to forecast. 
#' @return `list` of length 2, containing the posterior + forecasted observed
#'   values as 'abundance' and 'density'. The elements 'abundance' and 'density'
#'   contains lists with the respective values for each of the localities. 
#' @param model_input_forecastvsdata_data_2017
#' @param useful_objects_2017 
#' @param wrangled_stinkbug_data_2017
#' @param model_fit Pass as argument the different targets built with the 
#'   fit_lastweeks_forecast_jags_2017 function.  
#' @param weeks_forecasted This argument needs a value to be specified     
compute_lastweeks_forecast_2017 <- function(
  model_input_forecastvsdata_data_2017,
  useful_objects_2017,
  wrangled_stinkbug_data_2017,
  model_fit,
  weeks_forecasted
) {
  
  
  ## we will make a different approach to computing the forecasts. Since we pass to jags a dataset of target values with NAs at the datapoints that we want to forecast, jags will automatically impute the missing value (i.e., will take the NA as a parameter to be estimated). We will include the target values as params to save, and this will output a target dataframe where the previously NA value has now a posterior distribution. The thing here is that in the target output dataframe, only the datapoint that contained a NA will have a posterior, whereas the other values (the datapoints with observed values) will have the observed value repeated as many times as post samples (because these datapoints aren't estimated because we provided values). So, in order to have the plots that we want (posterior + forecast), we will do 2 things: 1. Extract the needed params to compute the posterior predictive check (i.e., X.A, the latent variable); 2. Extract the posterior samples of the forecasted data-point. Then (in another function), we will merge these two to make the plots.     
  
  
  # load some useful objects to the function environment
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  #time_series_length_forecast <- useful_objects_2017[['for_jags']][['time_series_length_forecast']]
  #semanasLoc <- time_series_length
  Localidades <- unique(wrangled_stinkbug_data_2017$Localidad)
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  loc.suffix <- str_c(",", 1:nLocalidades, "]")
  #loc.preffix <- str_c("[", 1:nLocalidades, ",")
  sample.size <- useful_objects_2017[['other']][['n_samples_per_location']] %>% 
    max()
  
  # check that all localities have the same sample size. 
  ifelse(
    length(sample.size) == 1, 
    'sample.size has the correct length (length = 1)', 
    stop('sample.size has more than 1 value')
  )
  
  # save ECMWF and phen vars. Since they are all the same across the list
  # list elements, it doesn't matter which list we grab (here we grab the first
  # element)
  # T.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['T.s.ECMWF']]
  # DP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['DP.s.ECMWF']]
  # TP.s.ECMWF <- model_input_forecastvsdata_data_2017[[1]][['TP.s.ECMWF']]
  # VR1.2.3 <- model_input_forecastvsdata_data_2017[[1]][['VR1.2.3']]
  # R4.5.6.7 <- model_input_forecastvsdata_data_2017[[1]][['R4.5.6.7']]
  # R8 <- model_input_forecastvsdata_data_2017[[1]][['R8']]
   
  output_postdata_mod1_ECMWF_Y.A <- list()
  
  
  
  
  
  for (k in seq_along(Localidades)){   
    
    ## save post samples
    postdata_mod1 <- model_fit[[k]]
    
    
    ##################################################################### esto se llama postdata_mod1_ECMWF pero en realidad deberia andar con todo modelo (porque todos tienen la misma funcion de observacion)
    ## extract those containing 'EqNv.dat'
    postdata_mod1_ECMWF <- postdata_mod1 %>%
      select(contains("EqNv.dat"))
    
    
    # define amount of samples to be used for plots 
    # samplesplot <- model_input_forecastvsdata_data_2017 %>%    #before 1e4
    #   map(nrow) %>% 
    #   unique() %>% 
    #   unlist()
    samplesplot <- 3e4
    
    # we samlpe "samplesplot" rows from the posterior
    postdata_mod1_ECMWF <- postdata_mod1_ECMWF[
      sample(nrow(postdata_mod1_ECMWF), samplesplot),]
    
    # Posteriors to be used for posterior predictive check
    ## 'Pobs' param posterior
    postdata_mod1_ECMWF_pobs <- data.frame(postdata_mod1_ECMWF %>%
                                             select(starts_with("pobs")))
    
    
    ## for 'X.A' and 'Y.A' we will only extract the posteriors corresponding
    ## to the locality being forecasted (i.e., 'k').
    
    
    ### 'X.A' param (latent values) posterior. We will only keep the values
    ### corresponding to the part of the time-series that is NOT being 
    ### forecasted 
    postdata_mod1_ECMWF_X.A <- postdata_mod1_ECMWF %>%
      select(starts_with("X.A.EqNv.dat")) %>%
      #### select params corresponding to locality being forecasted
      select(ends_with(loc.suffix[k])) %>% 
      #### select part of time-series that's NOT being forecasted 
      select(1:(time_series_length[k] - weeks_forecasted))
    
    ### 'Y.A' param posterior. We will only keep the values
    ### corresponding to the part of the time-series that IS being 
    ### forecasted 
    postdata_mod1_ECMWF_Y.A <- postdata_mod1_ECMWF %>%
      select(starts_with("Y.A.EqNv.dat")) %>%
      #### select params corresponding to locality being forecasted
      select(ends_with(loc.suffix[k])) #%>% 
    #### select part of time-series that IS being forecasted
    # select(
    #   time_series_length[k]: (time_series_length[k] - weeks_forecasted + 1) 
    #   )
    
    
    
    # Now we will replace postdata_mod1_ECMWF_Y.A columns where there's no 
    # posterior (weeks not being forecasted) with the posterior predictive check
    # This will yield what we want, a data frame with posterior check + forecast
    ## t is the week index, k is the locality index
    for (t in 1:(time_series_length[k] - weeks_forecasted)){
      ## posterior predictive check
      post_vec <- rbinom(
        n = samplesplot, 
        size = pull(postdata_mod1_ECMWF_X.A[, t]),
        prob = pull(postdata_mod1_ECMWF_pobs)
      )
      
      postdata_mod1_ECMWF_Y.A[, t] <- post_vec
      
    }
    
    ## there's no NAs on postdata_mod1_ECMWF_Y.A. 
    ## test with: map(postdata_mod1_ECMWF_Y.A, is.na) %>% map(sum) 
    
    
    # now we compute the per 'muestra' postY.A densities. 
    postdata_mod1_ECMWF_Y.A_sample <- postdata_mod1_ECMWF_Y.A / sample.size
    
    
    output_postdata_mod1_ECMWF_Y.A[['abundance']][[k]] <-
      postdata_mod1_ECMWF_Y.A
    output_postdata_mod1_ECMWF_Y.A[['density']][[k]] <- 
      as_tibble(postdata_mod1_ECMWF_Y.A_sample)
  }

  return(output_postdata_mod1_ECMWF_Y.A)
  
}
