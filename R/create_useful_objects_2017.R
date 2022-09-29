#' @title Create useful R objects for the whole project, 2017
#' @description Compute some R objects using only 2017 data that will be useful 
#'   for jags fit and posterior checks
#' @return  `list` of length 3, list-elements: 'for_jags', 'for_plots', 'other'
#' @param merged_sb_noaa_ecmwf_data_2017
create_useful_objects_2017 <- function(merged_sb_noaa_ecmwf_data_2017){ ########## later when u add all of the datasets u should do a function for all years using the full dataset

  # Interactive development
  #merged_sb_noaa_ecmwf_data_2017 <- tar_read(merged_sb_noaa_ecmwf_data_2017)



  locs <- unique(merged_sb_noaa_ecmwf_data_2017$Localidad)

  Localidades <- locs
  
  nLocalidades <- length(locs)
    
  time_series_length = merged_sb_noaa_ecmwf_data_2017 %>% 
    group_by(Localidad) %>% 
    summarise(
      'time_series_length' = unique(time_series_length)
    ) %>% 
    pull('time_series_length')
  



  time_series_length_forecast <- list()  
  for (j in seq_along(Localidades)) {
    t <- time_series_length
    
    t[j] = t[j] - 1
    time_series_length_forecast[[j]] <- t
  }



  dates <- list()
  for (l in seq_along(locs)) {
   dates[[l]] <- merged_sb_noaa_ecmwf_data_2017 %>% filter(Localidad == locs[[l]]) %>% pluck("Fecha")
  }
  names(dates) <- locs 


  useful_objects <- list(
    'for_jags' = list(
      ## Character vector of 2017 localities 
      'localities' = locs,
      ## Total amount of 2017 localities
      'n_localities' = length(locs),
      ## full time series length
      'time_series_length' = time_series_length,
      
      ## time series length for forecast (last week removed)
      'time_series_length_forecast' = time_series_length_forecast,
      
      
      ## parameters vector that includes all parameters used in any model.
     'pars_jags' = c(
       "Y.A.EqNv.dat", 
       "Y.N.EqNv.dat", 
       "I.EqNv.dat",
       "r.A.EqNv.dat",
       "X.A.EqNv.dat",
       "pobs.EqNv.dat",
       "loglik.EqNv.dat",
       "lambda.ini.EqNv.dat",
       "I.EqNv.dat", 
       "I.ini", 
       "I.decay",
       
       "b0.EqNv.dat",
       "bT.A.EqNv.dat",
       "bTP.A.EqNv.dat",
       "bDP.A.EqNv.dat",
       "bF.EqNv.dat",
       "bF.VR1.2.3.EqNv.dat", 
       "bF.R4.5.6.7.EqNv.dat", 
       "bF.R8.EqNv.dat",
       
       "mu0.EqNv.dat",  
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
       
       "sigma0.EqNv.dat", 
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
       
     ),
     
     # All covariates to be used in any model. 
     
     'covars' = c(
        
       
     ),
     
      
            
      ## inits for all models
      'model_inits' = list(
        'M1' = function() list(
          bF.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          I.EqNv.dat = runif(nLocalidades, 0, 5),
          pobs.EqNv.dat = rbeta(1, 8, 2.5),
          X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
          ), 
        'M2' = function() list(
          bF.VR1.2.3.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bF.R4.5.6.7.EqNv.dat = rnorm(nLocalidades, 0.5, 1),
          bF.R8.EqNv.dat = rnorm(nLocalidades, -0.5, 1), 
          bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          I.EqNv.dat = runif(nLocalidades, 0, 5),
          pobs.EqNv.dat = rbeta(1, 8, 2.5),
          X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
        ), 
        'M3' = function() list(
          bF.VR1.2.3.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bF.R4.5.6.7.EqNv.dat = rnorm(nLocalidades, 0.5, 1),
          bF.R8.EqNv.dat = rnorm(nLocalidades, -0.5, 1), 
          bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          # missing I.EqNv.dat init to avoid dimension mismatch. 
          pobs.EqNv.dat = rbeta(1, 8, 2.5),
          X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
        ), 
        'M1_b0' = function() list(
          b0.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bF.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          I.EqNv.dat = runif(nLocalidades, 0, 5),
          pobs.EqNv.dat = rbeta(1, 8, 2.5),
          X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
        ),   
        'M2_b0' = function() list(
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
        ),  
        'other' = function() list(
          bT.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bTP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          bDP.A.EqNv.dat = rnorm(nLocalidades, 0, 1),
          pobs.EqNv.dat = rbeta(1, 8, 2.5),
          X.A.EqNv.dat = init_X.A.EqNv.dat ############################ removed the as.matrix(), try adding it back if there's a bug  
        )  
        
        
      )
      
      
    ),
    
    'for_plots' = NA
      
    #   plot_labeling_data <- metdata_2017_weekly %>%
    #   # select variables of interest for plotting 
    #   select(station, lat, lon, Date, week, t2m_wmean, tp_wmean, d2m_wmean) %>%
    #   # store data only from each sunday of each week 
    #   group_by(station, week) %>%
    #   slice_head() 
    # 
    # # pull a dates vector from a location (does't matter which location)
    # plot_dates <- plot_labeling_data %>% 
    #   filter(station == "GENERAL_PICO") %>% 
    #   pull(Date) %>% 
    #   as.character()
    # 
      
      
      ,
    'other' = list(
      'dates' = dates,
      ## amount of samples per locality
      'n_samples_per_location' = merged_sb_noaa_ecmwf_data_2017 %>% 
        group_by(Localidad) %>% 
        summarise(
          'n_samples_per_location' = mean(Total.Muestras) ### le puse un mean() para promediar en los casos donde los paños tirados son distintos por cada fecha, como en el dataset de 2021
        ) %>% 
        pull('n_samples_per_location') ######## esto anda solo porque las muestras son las mismas 
    )
  )
  
  
  # nsemana <- CCsemana_2017 %>%
  #   group_by(Localidad) %>%
  #   tally() %>%
  #   rename("Cantidad de Semanas" = n)
  # 
  # Localidades <- as.character(nsemana$Localidad)
  
  ## return list
  useful_objects
  
}
