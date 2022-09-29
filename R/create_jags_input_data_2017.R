#' @title Create JAGS input data, 2017
#' @description Creates a `list` with all data needed for fitting all the models
#'   with jags, but only using data from 2017
#' @return  `list` with everything needed for a JAGS run
#' @param merged_sb_noaa_ecmwf_data_2017
#' @param useful_objects_2017
create_jags_input_data_2017 <- function(
  merged_sb_noaa_ecmwf_data_2017, 
  useful_objects_2017
) {
  

  
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  
  # Creamos variables
  
  ## EqNv adulto observado  (Y.A.EqNv)
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$EqNv.adulto.redondeado
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  Y.A.EqNv <- do.call(cbind, ll)
  Y.A.EqNv.dat <- apply(Y.A.EqNv, 2, as.integer)
  
  ## EqNv ninfa observado  (Y.N.EqNv)
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$EqNv.joven.redondeado
  }
  
  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })
  
  Y.N.EqNv <- do.call(cbind, ll)
  Y.N.EqNv.dat <- apply(Y.N.EqNv, 2, as.integer)
  
  ## Temperatura escalada (T.s) NOAA
  l <- list()
  for (i in 1:nLocalidades) {
    l[[i]] <- merged_sb_noaa_ecmwf_data_2017[merged_sb_noaa_ecmwf_data_2017$nLocalidad == i, ]$NOAA.air.temp.weekly.avg_2017
  }

  n <- max(sapply(l, length))
  ll <- lapply(l, function(X) {
    c(as.character(X), rep("NA", times = n - length(X)))
  })

  T.NOAA <- do.call(cbind, ll)
  T.NOAA <- apply(T.NOAA, 2, as.numeric)
  T.s.NOAA <- scale(T.NOAA) %>%
    ## remove scale() attributes
    as.data.frame()

  
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
  
  
  
  # this vector contains all parameters for all model variants. 
  pars_mod1 <- c(
    "I.EqNv.dat",
    # "r.A.EqNv.dat",
    "X.A.EqNv.dat",
    "pobs.EqNv.dat",
    "loglik.EqNv.dat",
    "lambda.ini.EqNv.dat",
    "I.EqNv.dat", 
    "I.ini", 
    "I.decay",
    
    "bT.A.EqNv.dat",
    "bTP.A.EqNv.dat",
    "bDP.A.EqNv.dat",
    "bF.EqNv.dat",
    "bF.VR1.2.3.EqNv.dat", 
    "bF.R4.5.6.7.EqNv.dat", 
    "bF.R8.EqNv.dat",
    
    
    "muF.EqNv.dat",
    "muF.VR1.2.3.EqNv.dat", 
    "muF.R4.5.6.7.EqNv.dat", 
    "muF.R8.EqNv.dat",
    "muT.EqNv.dat", 
    "muTP.EqNv.dat", 
    "muDP.EqNv.dat", 
    "muI.EqNv.dat",
    "muI.ini.EqNv.dat",
    "muI.decay.EqNv.dat",
    
    
    "sigmaF.EqNv.dat",
    "sigmaF.VR1.2.3.EqNv.dat", 
    "sigmaF.R4.5.6.7.EqNv.dat", 
    "sigmaF.R8.EqNv.dat",
    "sigmaT.EqNv.dat", 
    "sigmaTP.EqNv.dat", 
    "sigmaDP.EqNv.dat", 
    "sigmaI.EqNv.dat",
    
    "sigmaI.ini.EqNv.dat", 
    "sigmaI.decay.EqNv.dat"
  )

  
  
  ############## making it a function didnt work when trying to embed it into a list
  inits_mod1 <- function() {
    nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]

    list(
      bT.A.EqNv.dat = stats::rnorm(nLocalidades, 0, 1),
      I.EqNv.dat = stats::runif(nLocalidades, 0, 10),
      pobs.EqNv.dat = stats::runif(1),
      X.A.EqNv.dat = Y.A.EqNv[1:12, ]
    )
  }
  

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
  
  
  
  jags_input_data <- list(
    'Y.A.EqNv.dat' = Y.A.EqNv.dat,
    'Y.N.EqNv.dat' = Y.N.EqNv.dat,
    'T.s.NOAA' = T.s.NOAA,
    'T.s.ECMWF' = T.s.ECMWF,
    'DP.s.ECMWF' = DP.s.ECMWF,
    'TP.s.ECMWF' = TP.s.ECMWF,
    'nFenologia' = nFenologia,
    'VR1.2.3' = VR1.2.3,
    'R4.5.6.7' = R4.5.6.7,
    'R8' = R8,
    'nLocalidades' = n_localities,
    'semanasLoc' = time_series_length,
     'pars' = pars_mod1,
     'inits' = inits_mod1
  )




  jags_input_data


  
  
}
