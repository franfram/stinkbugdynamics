#' @title Create JAGS input data for last-weeks forecast. 
#' @description Create a list containing datasets, where each dataset has the
#' last week observation of only one locality missing (NA)
#' @return `list` of size 'length(Localidades)', containing the followin vars:
#'  Scaled 2m-temperature (T.s.ECMWF)
#'  Scaled 2m-dewpoint ()
#'  Scaled total precipitation
#'  VR1.2.3 Crop phen dummy
#'  R4.5.6.7 Crop phen dummy
#'  R8 Crop phen dummy
#'  nLocalidades (amount of localities)
#'  semanasLoc (time series length of each locality)
#' 
#' @param raw_stinkbug_data_file_2017
#' @param wrangled_stinkbug_data_2017
#' @param useful_objects_2017 
#' @param merged_sb_noaa_ecmwf_data_2017   
#' @param weeks_forecasted n of weeks being forecasted, starting from the end
#'   of each time-series. Pass 1 or 2 as arguments. 

create_lastweeks_forecast_input_data_2017 <- function(
  raw_stinkbug_data_file_2017,
  wrangled_stinkbug_data_2017,
  useful_objects_2017,
  merged_sb_noaa_ecmwf_data_2017,
  weeks_forecasted
) {
  
  Localidades <- unique(wrangled_stinkbug_data_2017$Localidad)

  
  time_series_length_per_loc <- wrangled_stinkbug_data_2017 %>%
    group_by(Localidad) %>%
    tally() %>%
    rename("time_series_length" = n) %>% 
    pull(time_series_length)
  
  
  Y.A.EqNv.dat <- list()
  
  for (k in seq_along(Localidades)) {
    
    ## return the localities that will have the full dataset
    locs_with_full_data <- setdiff(Localidades, Localidades[k])  ############## replace '1' for k
    
    
    
    l <- list()
    for (j in seq_along(Localidades)) {
      if(Localidades[j] %in% locs_with_full_data) {
        ## if the locality is one with the full dataset, 
        ## then just return EqNv.adulto.redondeado
        l[[j]] <- wrangled_stinkbug_data_2017[
          wrangled_stinkbug_data_2017$nLocalidad == j, ]$EqNv.adulto.redondeado
        ## if the locality is not one with the full dataset, that is, 
        ## if it's the one we are going to remove the last data point from the 
        ## time series, then return a vector full of 0s, which will be filled 
        ## downstream.  #################################################
      } else {
        #l[[j]] <- rep(0, time_series_length_per_loc[j])  
        l[[j]] <- wrangled_stinkbug_data_2017[
          wrangled_stinkbug_data_2017$nLocalidad == j,] %>% 
          ## remove weeks-forecasted datapoints from the timeseries, starting
          ## from the end of the time-series
          slice(1:(time_series_length_per_loc[j] - weeks_forecasted)) %>% 
          select(EqNv.adulto.redondeado) %>% 
          pull()
      }
    }
    
    ## Juntamos los datos de todas las localidades en un dataframe. 
    n <- 12  # antes max(sapply(l, length)), lo cambiamos para evitar que  tenga 11 rows en vez de 12 en la localidad 2 ############### revisar luego
    
    ll <- lapply(l, function(X) {
      c(as.character(X), rep("NA", times = n - length(X)))
    })
    
    Y.A.Eq <- do.call(cbind, ll) 
    Y.A.Eq <- apply(Y.A.Eq, 2, as.integer)
    Y.A.EqNv.dat[[k]] <- Y.A.Eq
    
  }
  
  # now that we have the 8 new datasets where in each one, one locality has
  # the last data point removed, we add the rest of the data for jags
  
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]

  
  ## Y.N.EqNv
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$EqNv.joven.redondeado
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })

  Y.N.Eq <- do.call(cbind, ll)
  Y.N.Eq <- apply(Y.N.Eq, 2, as.integer)
  Y.N.EqNv.dat <- Y.N.Eq
    
  
  
  
  ## scaled 2m temperature ECMWF
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$t2m_wmean
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  T.ECMWF <- do.call(cbind, ll)
  T.ECMWF <- apply(T.ECMWF, 2, as.numeric)
  ### scale it
  T.s.ECMWF <- scale(T.ECMWF) %>%
    ### remove scale() attributes
    as.data.frame()
  
  
  ## scaled total precipitation
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$tp_wmean
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  TP.ECMWF <- do.call(cbind, ll)
  TP.ECMWF <- apply(TP.ECMWF, 2, as.numeric)
  ### scale it
  TP.s.ECMWF <- scale(TP.ECMWF) %>%
    ### remove scale() attributes
    as.data.frame()
  
  
  
  ## scaled 2-m dewpoint
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$d2m_wmean
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  DP.ECMWF <- do.call(cbind, ll)
  DP.ECMWF <- apply(DP.ECMWF, 2, as.numeric)
  ### scale it
  DP.s.ECMWF <- scale(DP.ECMWF) %>%
    ### remove scale() attributes
    as.data.frame()
  
  
  ## Fenología
  ### Categórica
  
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$nFenologia
  }
  
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  nFenologia <- do.call(cbind, ll)
  nFenologia <- apply(nFenologia, 2, as.numeric)
  
  
  
  ### Dummies
  #### VR1.2.3
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$VR1.2.3
  }
  
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  VR1.2.3 <- do.call(cbind, ll)
  VR1.2.3 <- apply(VR1.2.3, 2, as.numeric)
  
  
  #### R4.5.6.7
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$R4.5.6.7
  }
  
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  R4.5.6.7 <- do.call(cbind, ll)
  R4.5.6.7 <- apply(R4.5.6.7, 2, as.numeric)
  
  
  #### R8
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$R8
  }
  
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  R8 <- do.call(cbind, ll)
  R8 <- apply(R8, 2, as.numeric)
  
  
  localities <- useful_objects_2017[['for_jags']][['localities']]
  n_localities <- useful_objects_2017[['for_jags']][['n_localities']]
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  
  #####################################################  # faltan priors
  # faltan inits
  # faltan pars
  pars_mod1 <- useful_objects_2017[['for_jags']][['pars_jags']]
  
  
  
  ############## making it a function didnt work when trying to embed it into a list
  # inits_mod1 <- function() {
  #   nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  #   
  #   list(
  #     bT.A.EqNv.dat = stats::rnorm(nLocalidades, 0, 1),
  #     I.EqNv.dat = stats::runif(nLocalidades, 0, 10),
  #     pobs.EqNv.dat = stats::runif(1),
  #     X.A.EqNv.dat = Y.A.EqNv[1:12, ]
  #   )
  # }
  # 
  
  # inits_mod1 <- list(
  #     bT.A.EqNv.dat = stats::rnorm(nLocalidades, 0, 1),
  #     I.EqNv.dat = stats::runif(nLocalidades, 0, 10),
  #     pobs.EqNv.dat = stats::runif(1),
  #     X.A.EqNv.dat = Y.A.EqNv[1:12, ] 
  #   )
  
  
  ##### from R2documentation
  # "a list with n.chains elements; each element of the list is itself a list of starting values for the BUGS model, or a function creating (possibly random) initial values. If inits is NULL, JAGS will generate initial values for parameters."
  # so it can be a list or a function. Then we good on the list. but it says that is has to have n.chains elements, thus should i repeat the 
  # list i have so far n.chains times and embed it into a new list? 
  
  # compute new time_series_length vectors for the forecast. In each vector,
  # one locality will have a time_series_length - 1 value.
  # time_series_length_forecast <- list()  
  # for (j in seq_along(Localidades)) {
  #   t <- time_series_length
  #   
  #   t[j] = t[j] - 1
  #   time_series_length_forecast[[j]] <- t
  # }
  
  
  jags_input_data <- list()
  for (j in seq_along(Localidades)){
    jags_input_data[[j]] <- list(
      'Y.A.EqNv.dat' = Y.A.EqNv.dat[[j]],
      'Y.N.EqNv.dat' = Y.N.EqNv.dat,
      #'T.s.NOAA' = T.s.NOAA,
      'T.s.ECMWF' = T.s.ECMWF,
      'DP.s.ECMWF' = DP.s.ECMWF,
      'TP.s.ECMWF' = TP.s.ECMWF,
      'nFenologia' = nFenologia,
      'VR1.2.3' = VR1.2.3,
      'R4.5.6.7' = R4.5.6.7,
      'R8' = R8,
      'nLocalidades' = n_localities,
      'semanasLoc' = time_series_length ############ not sure if it has to be time_series_length or time_series_length_forecast[[j]]
      #'pars' = pars_mod1,
      #'inits' = inits_mod1
    )    
  }

  return(jags_input_data)
}

