#' @title Compute Posterior Predictive Check
#' @description computes posterior predictive check regardless of model variant
#' @return `list` with posterior predictive check plots. 
#' @param jags_draws
#' @param useful_objects_2017
#' @param merged_sb_noaa_ecmwf_data_2017  
#' @param model_name

compute_posterior_predictive_check <- function(
  jags_draws,
  useful_objects_2017,
  merged_sb_noaa_ecmwf_data_2017,
  model_name
) {
  

  ## posterior draws
  n_sims <- nrow(jags_draws) 
  loc_names <- useful_objects_2017[['for_jags']][['localities']]
  nLocalidades <- useful_objects_2017[['for_jags']][['n_localities']]
  semanasLoc <- useful_objects_2017[['for_jags']][['time_series_length']]
  loc.suffix <- str_c(",", 1:nLocalidades, "]")
  data_merged <- merged_sb_noaa_ecmwf_data_2017 
  
  
  ### extract pobs
  post.pobs.EqNv.dat <- jags_draws %>%
    select(contains("pobs.EqNv.dat")) %>% 
    pull()
  
  
  post.X.A.EqNv.dat <- list()
  for (j in 1:nLocalidades) {
    post.X.A.EqNv.dat[[j]] <- jags_draws %>%
      select(contains("X.A.EqNv.dat")) %>%
      select(ends_with(loc.suffix[j])) %>% 
      data.frame()
  }
  
  
  
  # create array to store simulated data
  pp.Y.A.EqNv.dat <- list()
  for (j in 1:nLocalidades) {
    pp.Y.A.EqNv.dat[[j]] <- matrix(
      0, 
      nrow = n_sims, 
      ncol = useful_objects_2017[['for_jags']][['time_series_length']][j]
    )
  }
  
  # simulate observed data (Y.A)
  ## Data Model
  
  for (j in 1:nLocalidades) {
    for (t in 1:semanasLoc[j]) {
      pp.Y.A.EqNv.dat[[j]][, t] <- (
        rbinom( 
          n_sims, 
          post.X.A.EqNv.dat[[j]][, t],
          post.pobs.EqNv.dat
        )
      ) / useful_objects_2017[['other']][['n_samples_per_location']][j] 
    }
  }
  
  pp.Y.A.EqNv.dat.mean <- list()
  pp.Y.A.EqNv.dat.median <- list()
  pp.Y.A.EqNv.dat.HPDI60 <- list()
  pp.Y.A.EqNv.dat.HPDI80 <- list()
  pp.Y.A.EqNv.dat.HPDI95 <- list()
  
  
  
  ##### .dat now means per sample
  for (j in 1:nLocalidades) {
    pp.Y.A.EqNv.dat.mean[[j]] <- apply(pp.Y.A.EqNv.dat[[j]], 2, mean, na.rm = TRUE)
    pp.Y.A.EqNv.dat.median[[j]] <- apply(pp.Y.A.EqNv.dat[[j]], 2, median, na.rm = TRUE)
    pp.Y.A.EqNv.dat.HPDI60[[j]] <- apply(pp.Y.A.EqNv.dat[[j]], 2, rethinking::HPDI, prob = 0.60)
    pp.Y.A.EqNv.dat.HPDI80[[j]] <- apply(pp.Y.A.EqNv.dat[[j]], 2, rethinking::HPDI, prob = 0.80)
    pp.Y.A.EqNv.dat.HPDI95[[j]] <- apply(pp.Y.A.EqNv.dat[[j]], 2, rethinking::HPDI, prob = 0.95)
  }
  
  
  
  # lista de vectores con las semanas (eje X de cada plot)
  semanasplot <- list()
  for (i in 1:nLocalidades) {
    semanasplot[[i]] <- c(1:semanasLoc[i])
  }
  
  
  # Datos EqNv.round / n.muestras para el posterior predictive check
  
  
  ######## esto al final no lo estas usando
  # Y.A.EqNv.dens <- tar_read(model_input_data)[['Y.A.EqNv.dat']] / 
  #   unique(tar_read(useful_objects_2017)[['other']][['n_samples_per_location']]) ############ estoy hay que cambiarlo a aplicarle el / unique(n_samples_per_location) a aplicarleselo por cols porque cuando meta los otros datasets ya no va a ser un unico valor el unique(n_samples_per_location)
  
  fenologias <- list()
  for (j in 1:nLocalidades){
    fenologias[j] <- merged_sb_noaa_ecmwf_data_2017 %>% 
      filter(Localidad == loc_names[j]) %>% 
      select(Fenologia) %>% 
      as.vector()
  }
  
  
  # lista de vectores con las semanas (eje X de cada plot)
  semanasplot <- list()
  for (i in 1:nLocalidades) {
    semanasplot[[i]] <- c(1:semanasLoc[i])
  }
  
  
  # Merge .planta data for ggplot loop
  data_post_check <- list()
  data_plot_post_planta <- list()
  datos_post_plot_full <- list()
  data_plot_obs_planta <- list()
  
  for (j in seq_along(loc_names)) {
    data_post_check[[j]] <-
      bind_cols(
        as.vector(pp.Y.A.EqNv.dat.HPDI95[[j]]["|0.95", ]),
        as.vector(pp.Y.A.EqNv.dat.HPDI95[[j]]["0.95|", ]),
        as.vector(pp.Y.A.EqNv.dat.HPDI80[[j]]["|0.8", ]),
        as.vector(pp.Y.A.EqNv.dat.HPDI80[[j]]["0.8|", ]),
        as.vector(pp.Y.A.EqNv.dat.HPDI60[[j]]["|0.6", ]),
        as.vector(pp.Y.A.EqNv.dat.HPDI60[[j]]["0.6|", ]),
        as.vector(pp.Y.A.EqNv.dat.median[[j]]),
        semanasplot[[j]], ############# WTF this comma, works tho
      ) %>%
      rowid_to_column()
    
    names(data_post_check[[j]]) <- c(
      "rowid",
      "hpdi_min95", "hpdi_max95",
      "hpdi_min80", "hpdi_max80",
      "hpdi_min60", "hpdi_max60",
      "post_median", "semanas_loc"
    )
    
    
    data_plot_obs_planta[[j]] <-
      bind_cols(
        1:semanasLoc[[j]],
        pull(data_merged[data_merged$nLocalidad == j, "EqNv.adulto.planta"]),
        fenologias[[j]]
      ) %>%
      rowid_to_column()
    
    names(data_plot_obs_planta[[j]]) <- c(
      "rowid",
      "semanas_obs",
      "EqNv_obs",
      "phenology"
    )
    
    datos_post_plot_full[[j]] <- left_join(
      data_post_check[[j]],
      data_plot_obs_planta[[j]],
      by = "rowid"
    )
  }
  
  
  
  

  
  # hpdi_fill_data <- matrix(
  #   c(
  #     "#e5f5e0", "#a1d99b", "#31a354",
  #     "#deebf7", "#9ecae1", "#3182bd", 
  #     "#fff7bc", "#fec44f", "#db6d23", 
  #     "#e7d4e8", "#c2a5cf", "#762a83", 
  #     "#f6e8c3", "#dfc27d", "#bf812d", 
  #     "#c7eae5", "#80cdc1", "#35978f", 
  #     "#fde0dd", "#fcc5c0", "#e86f9f", 
  #     "#fff7bc", "#ffdf70", "#fec44f", 
  #     "#e5f5e0", "#a1d99b", "#31a354",
  #     "#deebf7", "#9ecae1", "#3182bd", 
  #     "#fff7bc", "#fec44f", "#db6d23" 
  #   ),
  #   ncol = nLocalidades,
  #   byrow = TRUE #FALSE
  # )
  
  ## little trick to loop around the 8 color-triads
  modulo_op <- 8

  hpdi_fill_data <- matrix(c("#e5f5e0", "#a1d99b", "#31a354", "#deebf7", "#9ecae1",
           "#3182bd", "#fff7bc", "#fec44f", "#db6d23", "#e7d4e8",
           "#c2a5cf", "#762a83", "#f6e8c3", "#dfc27d", "#bf812d",
           "#c7eae5", "#80cdc1", "#35978f", "#fde0dd", "#fcc5c0",
           "#e86f9f", "#fff7bc", "#ffdf70", "#fec44f"),
         ncol = modulo_op)
  
  
  post_check_plots <- list()
  plots_post_names_planta <- list()

  for (j in seq_along(loc_names)) {
    ## little modulo operator trick to use only 8 color-triads for any amount of
    ## localities 
    if (j %% 8 == 0) { ## the if statement is needed to avoid 0s
      mod <- 8
    } else {
      mod <- j %% 8  
    }
    
    
    ## plots of EqNv / sample
    post_check_plots[[j]] <- ggplot(data = datos_post_plot_full[[j]][1:semanasLoc[j], ]) +

      # Posterior Values
      geom_ribbon(
        aes(
          x = semanas_obs,
          ymin = hpdi_min95,
          ymax = hpdi_max95
        ),
        alpha = 0.9,
        fill = hpdi_fill_data[[1, mod]]
      ) +
      geom_ribbon(
        aes(
          x = semanas_obs,
          ymin = hpdi_min80,
          ymax = hpdi_max80
        ),
        alpha = 0.9, 
        fill = hpdi_fill_data[[2, mod]]
      ) +
      geom_ribbon(
        aes(
          x = semanas_obs,
          ymin = hpdi_min60,
          ymax = hpdi_max60
        ),
        alpha = 0.9,
        fill = hpdi_fill_data[[3, mod]]
      ) +
      geom_line(aes(
        x = semanas_obs,
        y = post_median
      ),
      lwd = 1,
      color = "grey0"
      ) +

      # Observed Values
      geom_point(
        aes(
          x = semanas_obs,
          y = EqNv_obs
        ),
        lwd = 1,
        color = "grey7"
      ) +
      geom_line(
        aes(
          x = semanas_obs,
          y = EqNv_obs
        ),
        lwd = 0.5,
        color = "grey20"
      ) +

      # Etc
      # coord_cartesian(ylim = c(0, max(data_plot_full[[j]]$threshold, na.rm = TRUE))) +
      scale_x_continuous(
        breaks = seq(
          0, 
          max(datos_post_plot_full[[j]]$semanas_obs, na.rm = TRUE),
          1
        )
      ) +
      labs(
        x = "Week",
        y = "E density",
        subtitle = paste0(
          model_name,
          ", ",
          loc_names[j]
        ),  
        caption =  "Estimated values represented by bold line (median) and ribbons (HPDI 60, 80, and 95).
         Observed values represented by dots and solid line"
      ) +
      theme(axis.title.y = element_text(angle = 89, vjust = 0.5)) #+
      #theme_few(base_size = 20)

    #plots_post_names_planta[[j]] <- paste0(loc_names[j], "_planta")
  }


  return(post_check_plots)
  
  
  
}

