#' @title Plot forecast against data
#' @description plot the EqNv abundance and EqNn density forecasts and compare
#'   with observed values.
#'   Plot ribbons indicate 60, 80 and 95% HPDIs, line indicates posterior median,  
#'   points indicate observed values.
#' @return `list` of length 3, containing (1) plots of the 
#'   posterior + forecasted observed values as 'abundance' and 'density', 
#'  (2) abundance data and (3) density data. 
#' @param forecast_data Pass lastweek_forecast_2017 or 
#'   last2weeks_forecast_2017 as arguments. 
#' @param wrangled_stinkbug_data_2017
#' @param useful_objects_2017     
#' @param weeks_forecasted Provide value of 1 or 2 for lastweek and last2weeks
#'   forecast.
#' @param model_name Model used to generate the data to be plotted. Will be used
#'    

############################################################## this function can be "week-invariant", thus removing the "lastweeks" part of the name. Have to see how I adapt this for the 
#"replace lastweek_forecast_2017 argument for 'forecast_data', and then define the value?"

plot_lastweeks_forecast_2017 <- function(
  forecast_data, 
  wrangled_stinkbug_data_2017,
  useful_objects_2017,
  weeks_forecasted = 25, # give a default that will never be used, useful for control flow downstream
  model_name 
) {
  
  # "
  # use weeks_forecasted argument to plot the vline. vline value will be something 
  # like (time_series_length[k] - weeks_forecasted + 1)
  # 
  # "
  # 

  # Interactive development 
  # forecast_data <- tar_read(lastweek_forecast_M3_2017)
  # wrangled_stinkbug_data_2017 <- tar_read(wrangled_stinkbug_data_2017) 
  # useful_objects_2017 <- tar_read(useful_objects_2017)
  # model_name <- "M3"
  # weeks_forecasted <- 1

  
  Y_A_EqNv_mean_density <- list()
  Y_A_EqNv_median_density <- list() 
  Y_A_EqNv_HPDI60_density <- list()
  Y_A_EqNv_HPDI80_density <- list()
  Y_A_EqNv_HPDI95_density <- list()
  
  Y_A_EqNv_mean_abundance <- list()
  Y_A_EqNv_median_abundance <- list()
  Y_A_EqNv_HPDI60_abundance <- list()
  Y_A_EqNv_HPDI80_abundance <- list()
  Y_A_EqNv_HPDI95_abundance <- list()
  
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  Localities <- unique(wrangled_stinkbug_data_2017$Localidad) 
  
  for (j in  seq_along(Localities)){
    "this may not be needed because there's no NAs i think"
    #forecast_data[['density']][[j]][is.na(forecast_data[['density']][[j]])] <- 0
    
    

    
    # Density values
    Y_A_EqNv_mean_density[[j]] <- forecast_data[['density']][[j]] %>%  
      apply(2, mean, na.rm = TRUE) 
    "removed comment: #-1 to remove post.sample col"
    Y_A_EqNv_median_density[[j]] <- forecast_data[['density']][[j]] %>%
      apply(2, median, na.rm = TRUE)
    
    ## first test is with col selection to avoid applying HPDI to cols with NAs, because it gives an error. But this selection makes the vector transform of the data_plot_density_post[[j]] of different length, which gives an other error. We will test replacing NAs for 0s to see if we can avoid this problem
    # ## the col selection for computing the HPDI is due to error occurring when there are columns with NAs
    # Y_A_EqNv_HPDI60_density[[j]] <- forecast_data[['density']][[j]][, 1:time_series_length[j]] %>%
    #   apply(2, rethinking::HPDI, prob = 0.6)
    # 
    # Y_A_EqNv_HPDI80_density[[j]] <- forecast_data[['density']][[j]][, 1:time_series_length[j]] %>%
    #   apply(2, rethinking::HPDI, prob = 0.80)
    # 
    # Y_A_EqNv_HPDI95_density[[j]] <- forecast_data[['density']][[j]][, 1:time_series_length[j]] %>%
    #   apply(2, rethinking::HPDI, prob = 0.95)
    
    
    Y_A_EqNv_HPDI60_density[[j]] <- forecast_data[['density']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.6)
    
    Y_A_EqNv_HPDI80_density[[j]] <- forecast_data[['density']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.80)
    
    Y_A_EqNv_HPDI95_density[[j]] <- forecast_data[['density']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.95)
    
    
    
    # Abundance values
    
    Y_A_EqNv_mean_abundance[[j]] <- forecast_data[['abundance']][[j]] %>%  
      apply(2, mean, na.rm = TRUE) 
    "removed comment: #-1 to remove post.sample col"
    Y_A_EqNv_median_abundance[[j]] <- forecast_data[['abundance']][[j]] %>%
      apply(2, median, na.rm = TRUE)
    
    Y_A_EqNv_HPDI60_abundance[[j]] <- forecast_data[['abundance']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.6)
    
    Y_A_EqNv_HPDI80_abundance[[j]] <- forecast_data[['abundance']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.80)
    
    Y_A_EqNv_HPDI95_abundance[[j]] <- forecast_data[['abundance']][[j]] %>%
      apply(2, rethinking::HPDI, prob = 0.95)
    
  }
  
  # Merge data for ggplot loop
  data_plot_density_full <- list()
  data_plot_density_post <- list()
  data_plot_density_obs <- list()
  
  data_plot_abundance_full <- list()
  data_plot_abundance_post <- list()
  data_plot_abundance_obs <- list()
  
  
  n_localities <- useful_objects_2017[['for_jags']][['n_localities']]
  
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  
  

  
  for (j in seq_along(Localities)){ # seq_along(Localities)
    # Density values
    
    data_plot_density_post[[j]] <- 
      bind_cols(
        # Add model name
        rep(model_name, time_series_length[[j]]), 
        
        # Add locality name 
        rep(Localities[j], time_series_length[[j]]), 
        
        as.vector(Y_A_EqNv_HPDI95_density[[j]]["|0.95", ]),
        as.vector(Y_A_EqNv_HPDI95_density[[j]]["0.95|", ]),
        
        as.vector(Y_A_EqNv_HPDI80_density[[j]]["|0.8", ]),
        as.vector(Y_A_EqNv_HPDI80_density[[j]]["0.8|", ]),
        
        as.vector(Y_A_EqNv_HPDI60_density[[j]]["|0.6", ]),
        as.vector(Y_A_EqNv_HPDI60_density[[j]]["0.6|", ]),
        
        as.vector(Y_A_EqNv_median_density[[j]]),
        
        c(1:time_series_length[[j]]), # before  c(1:max(time_series_length))# before semanasplotF[[j]],
      ) %>% 
      rowid_to_column()
    
    names(data_plot_density_post[[j]]) <- c(
      "rowid",
      "model_name",
      "locality",
      "hpdi_min95", "hpdi_max95",
      "hpdi_min80", "hpdi_max80",
      "hpdi_min60", "hpdi_max60",
      "post_median", "semanas_forecast"
    )
    
    
    data_plot_density_obs[[j]] <-
      bind_cols(
        1:time_series_length[[j]],
        (pull(wrangled_stinkbug_data_2017[wrangled_stinkbug_data_2017$nLocalidad == j, "EqNv.adulto.planta"] )),
        #pull(CCsemana[CCsemana$nLocalidad == j, "threshold_EqNv.adulto.m2"]),
        pull(wrangled_stinkbug_data_2017[wrangled_stinkbug_data_2017$nLocalidad == j, "Fenologia"])
      ) %>%
      rowid_to_column()
    
    names(data_plot_density_obs[[j]]) <- c(
      "rowid", 
      "semanas_obs", 
      "EqNv_obs", 
      "phenology"
    ) # , "threshold"
    
    data_plot_density_full[[j]] <- left_join(
      data_plot_density_post[[j]], 
      data_plot_density_obs[[j]], 
      by = "rowid"
    )
    
    
    
    
    # Abundance values    
    
    data_plot_abundance_post[[j]] <- 
      bind_cols(
        # Add model name
        rep(model_name, time_series_length[[j]]),
        # Add locality name 
        rep(Localities[j], time_series_length[[j]]), 
        
        as.vector(Y_A_EqNv_HPDI95_abundance[[j]]["|0.95", ]),
        as.vector(Y_A_EqNv_HPDI95_abundance[[j]]["0.95|", ]),
        
        as.vector(Y_A_EqNv_HPDI80_abundance[[j]]["|0.8", ]),
        as.vector(Y_A_EqNv_HPDI80_abundance[[j]]["0.8|", ]),
        
        as.vector(Y_A_EqNv_HPDI60_abundance[[j]]["|0.6", ]),
        as.vector(Y_A_EqNv_HPDI60_abundance[[j]]["0.6|", ]),
        
        as.vector(Y_A_EqNv_median_abundance[[j]]),
        
        c(1:time_series_length[[j]]), # before  c(1:max(time_series_length))# before semanasplotF[[j]],
        
      ) %>% 
      rowid_to_column()
    
    names(data_plot_abundance_post[[j]]) <- c(
      "rowid",
      "model_name", 
      "locality", 
      "hpdi_min95", "hpdi_max95",
      "hpdi_min80", "hpdi_max80",
      "hpdi_min60", "hpdi_max60",
      "post_median", "semanas_forecast"
    )
    
    
    data_plot_abundance_obs[[j]] <-
      bind_cols(
        1:time_series_length[[j]],
        (pull(wrangled_stinkbug_data_2017[wrangled_stinkbug_data_2017$nLocalidad == j, "EqNv.adulto.redondeado"] )),
        #pull(CCsemana[CCsemana$nLocalidad == j, "threshold_EqNv.adulto.m2"]),
        pull(wrangled_stinkbug_data_2017[wrangled_stinkbug_data_2017$nLocalidad == j, "Fenologia"])
      ) %>%
      rowid_to_column()
    
    names(data_plot_abundance_obs[[j]]) <- c(
      "rowid", 
      "semanas_obs", 
      "EqNv_obs", 
      "phenology"
    ) # , "threshold"
    
    data_plot_abundance_full[[j]] <- left_join(
      data_plot_abundance_post[[j]], 
      data_plot_abundance_obs[[j]], 
      by = "rowid"
    )
    
    
    
  }
  
  
  
  
  # Esto no va a andar cuando haya mas locs, vas a tener que meter un subsetting que haga que en la localidad 9 arranque a usar los colores del comienzo de la lista. 
  hpdi_fill_data <- matrix(
    c(
      "#e5f5e0", "#a1d99b", "#31a354", "#deebf7", "#9ecae1",
      "#3182bd", "#fff7bc", "#fec44f", "#db6d23", "#e7d4e8",
      "#c2a5cf", "#762a83", "#f6e8c3", "#dfc27d", "#bf812d",
      "#c7eae5", "#80cdc1", "#35978f", "#fde0dd", "#fcc5c0",
      "#e86f9f", "#fff7bc", "#ffdf70", "#fec44f"
    ),
    ncol = n_localities
  )
  
  
  
  
  plots_forecastvsdata <- list()

  
  
  
  for (j in seq_along(Localities)){ 
    
    # Density plots
    plots_forecastvsdata[['density']][[j]] <- ggplot(
      data = data_plot_density_full[[j]][1:time_series_length[j], ]
    ) +
      
      # Posterior Values
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min95,
          ymax = hpdi_max95
        ),
        fill = hpdi_fill_data[[1, j]]
      ) +
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min80,
          ymax = hpdi_max80
        ),
        fill = hpdi_fill_data[[2, j]]
      ) +
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min60,
          ymax = hpdi_max60
        ),
        fill = hpdi_fill_data[[3, j]]
      ) +
      geom_line(
        aes(
          x = semanas_obs,
          y = post_median
        ),
        lwd = 1,
        color = "grey0"
      ) +
      # Init Forecast
      geom_vline(
        xintercept = if (weeks_forecasted == 1 || weeks_forecasted == 2) {
          (time_series_length[j] - weeks_forecasted)
        } else if (missing(weeks_forecasted)) {
          NA_integer_  ######################################################## check if this works
        } else {
          stop("not clear where to put the ggplot vline")
        }, 
        linetype = "dashed"
      ) +
      # Observed Values
      geom_point(
        aes(
          x = semanas_obs,
          y = EqNv_obs
        ),
        size = 2.5,
        color = "grey7"
      ) +
      # geom_line(aes(x = semanas_obs,
      #               y = EqNv_obs),
      #           lwd = 0.5,
      #           color = "grey20"
      #           ) +
      
      # Etc
      #coord_cartesian(ylim = c(0, max(data_plot_density_full[[j]]$threshold, na.rm = TRUE))) +
      scale_x_continuous(
        breaks = seq(
          0, 
          max(data_plot_density_full[[j]]$semanas_obs, na.rm=TRUE),
          1
        )
      ) + 
      labs(
        x = "Week",
        y = "E density",#expression(paste("EqNv / ", "muestra")), #expression(paste("EqNv / ", m^2))
        subtitle = paste0(
          model_name,
          ", ", 
          Localities[j]
        ),
        caption =  if (weeks_forecasted == 1 || weeks_forecasted == 2) {
          "Vertical dashed line indicates beggining of forecast"
        } else if (missing(weeks_forecasted)) {
          "Every week is being forecasted, using data from previous time-steps, starting at L = ... NEED TO FINISH THIS CAPTION"
        } else {
          stop("not clear where to put the ggplot vline")
        }
      ) +
      theme(
        axis.title.y = element_text(
          angle = 90, 
          vjust = 0.5
        ), 
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20)
      ) #+
    #theme_few(base_size = 20) 
    
    
    #plots_forecastvsdata_names[[j]] <- paste0(Localities[j], "_planta") 
    
    
    
    
    
    # Abundance plots
    plots_forecastvsdata[['abundance']][[j]] <- ggplot(
      data = data_plot_abundance_full[[j]][1:time_series_length[j], ]
    ) +
      
      # Posterior Values
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min95,
          ymax = hpdi_max95
        ),
        fill = hpdi_fill_data[[1, j]]
      ) +
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min80,
          ymax = hpdi_max80
        ),
        fill = hpdi_fill_data[[2, j]]
      ) +
      geom_ribbon(
        aes(
          x= semanas_obs,
          ymin = hpdi_min60,
          ymax = hpdi_max60
        ),
        fill = hpdi_fill_data[[3, j]]
      ) +
      geom_line(
        aes(
          x = semanas_obs,
          y = post_median
        ),
        lwd = 1,
        color = "grey0"
      ) +
      
      # Init Forecast
      geom_vline(
        xintercept = if (weeks_forecasted == 1 || weeks_forecasted == 2) {
          (time_series_length[j] - weeks_forecasted)
        } else if (missing(weeks_forecasted)) {
          NA_integer_  ######################################################## check if this works
        } else {
          stop("not clear where to put the ggplot vline")
        }, 
        linetype = "dashed"
      ) +
      
      
      # Observed Values
      geom_point(
        aes(
          x = semanas_obs,
          y = EqNv_obs
        ),
        size = 2.5,
        color = "grey7"
      ) +
      # geom_line(aes(x = semanas_obs,
      #               y = EqNv_obs),
      #           lwd = 0.5,
      #           color = "grey20"
      #           ) +
      
      # Etc
      #coord_cartesian(ylim = c(0, max(data_plot_abundance_full[[j]]$threshold, na.rm = TRUE))) +
      scale_x_continuous(
        breaks = seq(
          0, 
          max(data_plot_abundance_full[[j]]$semanas_obs, na.rm=TRUE),
          1
        )
      ) + 
      labs(
        x = "Week",
        y = "E abundance",#expression(paste("EqNv / ", "muestra")), #expression(paste("EqNv / ", m^2))
        subtitle = paste0(
          model_name, 
          ", ", 
          Localities[j]
        ),
        caption =  if (weeks_forecasted == 1 || weeks_forecasted == 2) {
          "Vertical dashed line indicates beggining of forecast"
        } else if (missing(weeks_forecasted)) {
          "Every week is being forecasted, using data from previous time-steps, starting at L = ... NEED TO FINISH THIS CAPTION"
        } else {
          stop("not clear where to put the ggplot vline")
        }
      ) +
      theme(
        axis.title.y = element_text(
          angle = 90, 
          vjust = 0.5
        ), 
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20)
      ) #+
    #theme_few(base_size = 20) 
    
    
    #plots_forecastvsdata_names[[j]] <- paste0(Localities[j], "_planta") 
    

    


    
    
  }
  
  









  data_plot_abundance_full[[j]][1:time_series_length[j], ]


  # Add crop phenology labels and save plots
  
  
  for (j in seq_along(Localities)) {
    # prep data for plotting
    ## Density data
    data_density_plot <-  data_plot_density_full[[j]] %>% 
      filter(locality == Localities[j])
    ## Abundance data
    data_abundance_plot <- data_plot_abundance_full[[j]] %>% 
      filter(locality == Localities[j])

    
    for (i in 1:time_series_length[j]) {
      # Add crop phenology label
      ## density plots
      plots_forecastvsdata[['density']][[j]] <- plots_forecastvsdata[['density']][[j]] +
        annotate("text",
                 x = i,
                 y = max(data_density_plot[, "hpdi_max95"]) * 0.9, # add labels at top
                 label = data_density_plot$phenology[i]
        )
      
      ## abundance plots 
      plots_forecastvsdata[['abundance']][[j]] <- plots_forecastvsdata[['abundance']][[j]] +
        annotate("text",
                 x = i,
                 y = max(data_abundance_plot[, "hpdi_max95"]) * 0.9, # add labels at top
                 label = data_abundance_plot$phenology[i]
        )
      
    }
    
    # print plots and save them
    ## density plots
    print(plots_forecastvsdata[['density']][[j]])
    ggsave(
      filename = paste0(
        "plot_forecastvsdata_density_", 
        model_name,
        "_",
        Localities[[j]], 
        ".png"
      ),
      path = here::here("output", "plots")
    )
  
    ## need to print for ggsave
    print(plots_forecastvsdata[['abundance']][[j]])
    ggsave(
      filename = paste0(
        "plot_forecastvsdata_abundance_", 
        model_name,
        "_", 
        Localities[[j]], 
        ".png"
      ),
      path = here::here("output", "plots")
    )


  }







































  
  
  
  
  
  #########
  
  return(
    list(
      'plots' = plots_forecastvsdata,
      'data_density' = data_plot_density_full,
      'data_abundance' = data_plot_abundance_full
    )
  )  
  
  
}
