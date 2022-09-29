#' @title Create JAGS input data for Uncertainty analysis without forecast. 
#' @description Create a list containing datasets, where each dataset
#'  has the last week observation of only one locality missing (NA).
#'  Replicate this n_replicates times
#' @return `list` with target data for jags model fits. 
#'   list structure: [[locality]][[sample_size]][[n_repl]]
#' @param raw_stinkbug_data_file_2017 Data for sampling 
#' @param wrangled_stinkbug_data Pass wrangled_stinkbug_data_2017 as argument
#' @param useful_objects_2017 
#' @param sample_size vector with sample sizes to be evaluated
#' @param n_repl vector, 1:amount_of_replicates 

create_uncertainty_analysis_input_data_2017 <- function(
  merged_sb_noaa_ecmwf_data_2017, 
  raw_stinkbug_data_file_2017,
  wrangled_stinkbug_data,
  useful_objects_2017,
  sample_size,
  n_repl
  
) {

  
  
 ## save covariates that will be merged to the sampled data in a list, 
 ## which will be passed to jags in another function. 
  
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
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
  
  
  n_localities <- useful_objects_2017[['for_jags']][['n_localities']]
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]

  
 
  
   
  
  # useful stuff
  localities <- useful_objects_2017[['for_jags']][['localities']]
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  

  
  # Create nested list for saving input data for uncertainty analysis.
  #Y.A.EqNv.dat <- list()
  jags_input_data <- list()
  for (k in seq_along(localities)) {
    #Y.A.EqNv.dat[[k]] <- list()
    jags_input_data[[k]] <- list()
    
    for (s in seq_along(sample_size)) {
      #Y.A.EqNv.dat[[k]][[s]] <- list()
      jags_input_data[[k]][[s]] <- list() 
       
      for (r in seq_along(n_repl)) {
        #Y.A.EqNv.dat[[k]][[s]][[r]] <- list()
        jags_input_data[[k]][[s]][[r]] <- list()
     
           
      }
    }
  }
  
  
  
  
  
  ## Prep data for sampling. We can't use the fully wrangled data for sampling,
  ## thus we'll create a mildly wrangled data-set called data_for_sampling 
  
  data_raw <- raw_stinkbug_data_file_2017 %>% 
    read.csv2( 
      skip = 4,
      header = TRUE,
      dec = "."
    ) %>%
    as_tibble() %>%
    filter(!is.na(Temp)) %>%
    mutate("Fecha" = as.Date(Fecha, format = "%d/%m/%Y"))    
  
  
  
  
  ## Factors for computing Equivalente Nezara (EqNv) from each stinkbug 
  ## species (only adults)  
  factor.Nv <- 1
  factor.Pg <- 1.6
  factor.Em <- 0.6
  factor.Df <- 0.3
  
  
  
  data_for_sampling <- data_raw %>%
    mutate("EqNv..Grande" = Nezara.viri..Grande * factor.Nv +
             Piezod..Guil..Grande * factor.Pg +
             Edessa.med..Grande. * factor.Em +
             Dichelops.f..Grande * factor.Df) %>%
    select(
      "Localidad", "Fecha", "Fenologia", "Muestra", "Nezara.viri..Grande",
      "Piezod..Guil..Grande", "Edessa.med..Grande.", "Dichelops.f..Grande",
      "EqNv..Grande"
    )
  
  
  
  
  for (k in seq_along(localities) ) { #seq_along(localities) 
    for (s in seq_along(sample_size)) { # seq_along(sample_size)
      for (r in seq_along(n_repl)) { # seq_along(n_repl)
        ## We will compute a dataset, where all localities except one has 180 
        ## samples, and the remaining locality (the k locality) will have
        ## sample_size[s] samples. 
        
        
        
        
        l <- list()
        ## save localities that won't be sampled in this k iteration (i.e., 
        ## all except the k locality)
        locs_not_sampled <- setdiff(localities, localities[k])
        
        ## Fill list `l` in the following way: If a locality is included in 
        ## locs_not_sampled, then grab the normal observed data. Else, fill the
        ## list element with 0s (to be filled with sampled data downstream)
        for (i in seq_along(localities)) {
          if (localities[i] %in% locs_not_sampled) {
            l[[i]] <- wrangled_stinkbug_data %>%
              filter(nLocalidad == i) %>% 
              select(EqNv.adulto.redondeado) %>% 
              pull()
          } else {
            l[[i]] <- rep(0, time_series_length[k])
          }
        }
        
        
        ## Now select the element of the `l` list that's full with 0s 
        ## (i.e., the k element) and fill it with the sampled observed values
        l[[k]] <- data_for_sampling %>%
          filter(Localidad == localities[k]) %>%
          group_by(Fecha) %>%
          sample_n(size = sample_size[s], replace = FALSE) %>%
          summarise("EqNv_adulto_round" = round(sum(EqNv..Grande))) %>%
          ### we select every week of the time-series except the last 
          ### one (which we will forecast later) 
          slice(
            1:(time_series_length[k] - 1)
          ) %>% 
          pull(EqNv_adulto_round)
        
        ## Bind data from each locality into 1 dataframe.
        n <- 12 # antes max(sapply(l, length)), lo cambiamos para evitar que
        # tenga 11 rows en vez de 12 en la localidad 2
        ll <- lapply(l, function(X) {
          c(as.character(X), rep("NA", times = n - length(X)))
        })
        
        
        
        Y.A.EqNv <- do.call(cbind, ll)
        Y.A.EqNv <- apply(Y.A.EqNv, 2, as.integer)
        
        # `Y.A.EqNv.dat` list structure: [[locality]][[sample_size]][[n_repl]]
        #Y.A.EqNv.dat[[k]][[s]][[r]] <- Y.A.EqNv
       
        jags_input_data[[k]][[s]][[r]] <- list(
          'Y.A.EqNv.dat' = Y.A.EqNv,
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
        )         
             
      }
    }
  }
  
  
  return(jags_input_data)
  


  
  
  
}

 