#' @param data_list named `list` with plots_lastweek_forecast_*_2017 targets.
#'   Example: 
#'     data_list = list(
#'       'M1_b0' = plots_lastweek_forecast_M1_b0_2017, 
#'       'M2' = plots_lastweek_forecast_M2_2017, 
#'       'M3' = plots_lastweek_forecast_M3_2017
#'     )  
#' @param useful_objects_2017 target
#'   

plot_lastweeks_forecast_modelcompare_2017 <- function(
  data_list, 
  useful_objects_2017
) {
  # interactive development
  # data_list = list(
  #       'M1_b0' = tar_read(plots_lastweek_forecast_M1_b0_2017), 
  #       'M2' = tar_read(plots_lastweek_forecast_M2_2017), 
  #       'M3' = tar_read(plots_lastweek_forecast_M3_2017), 
  #       'M3v2' = tar_read(plots_lastweek_forecast_M3v2_2017)#, 
  #       # 'M3v3' = plots_lastweek_forecast_M3v3_2017 

  # )
  # useful_objects_2017 <- tar_read(useful_objects_2017)
  
  
  
  
  
  Localities <- useful_objects_2017[['for_jags']][['localities']]
  time_series_length <- useful_objects_2017[['for_jags']][['time_series_length']]
  
  

  
  # get data for plotting
  data_models <- list(
    'density' = map(data_list, ~ pluck(.x, 'data_density')),
    'abundance' = map(data_list, ~ pluck(.x, 'data_abundance'))
  )
  
  
  
  # bind rows of all datasets into one for plotting. clean and re-order
  data_all_models <- data_models %>% 
    map(bind_rows) %>% 
    map(select, -rowid) %>% 
    map(select, 
        c(
          model_name, 
          locality, 
          semanas_forecast, 
          EqNv_obs, 
          phenology, 
          post_median, 
          everything()
        )
    )
  
  
  
  
  
  # Plots per locality              
  ## Density plots
  plots_per_loc_density <-  map2(
    .x = Localities,
    .y = time_series_length,
    .f = ~{
      data_all_models$density %>% 
        filter(locality == .x) %>% 
        ggplot() +
        geom_line(
          aes(
            x = semanas_forecast, 
            y = post_median,
            color = model_name
          )
        ) +
        # HPDI 95
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min95, 
            ymax = hpdi_max95,
            color = model_name
          ),
          alpha = 0.3
        ) +
        # HPDI 80
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min80, 
            ymax = hpdi_max80,
            color = model_name
          ),
          alpha = 0.3
        ) +
        # HPDI 60
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min60, 
            ymax = hpdi_max60,
            color = model_name
          ),
          alpha = 0.3
        ) +
        
        # Observed values
        geom_point(
          aes(
            x = semanas_forecast,
            y = EqNv_obs
          )
        ) +
        # Init of forecast vline
        geom_vline(
          xintercept = .y - 1,
          linetype = "dashed",
          alpha = 0.8
        ) +
        scale_x_continuous(
          breaks = seq(
            0,
            max(data_all_models$density$semanas_forecast),
            1
          )
        ) +
        labs(
          x = "Week", 
          y = "E density", 
          subtitle = .x, 
          caption = "Vertical dashed line indicated beggining of forecast"
        ) +
        theme(
          axis.title.y = element_text(
            angle = 90, 
            vjust = 0.5
          )
        )
    }
  )      
  
  
  ## Abundance plots 
  plots_per_loc_abundance <-  map2(
    .x = Localities,
    .y = time_series_length,
    .f = ~{
      data_all_models$abundance %>% 
        filter(locality == .x) %>% 
        ggplot() +
        geom_line(
          aes(
            x = semanas_forecast, 
            y = post_median,
            color = model_name
          )
        ) +
        # HPDI 95
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min95, 
            ymax = hpdi_max95,
            color = model_name
          ),
          alpha = 0.3
        ) +
        # HPDI 80
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min80, 
            ymax = hpdi_max80,
            color = model_name
          ),
          alpha = 0.3
        ) +
        # HPDI 60
        geom_errorbar(
          aes(
            x = semanas_forecast, 
            ymin = hpdi_min60, 
            ymax = hpdi_max60,
            color = model_name
          ),
          alpha = 0.3
        ) +
        
        # Observed values
        geom_point(
          aes(
            x = semanas_forecast,
            y = EqNv_obs
          )
        ) +
        # Init of forecast vline
        geom_vline(
          xintercept = .y - 1,
          linetype = "dashed",
          alpha = 0.8
        ) +
        scale_x_continuous(
          breaks = seq(
            0,
            max(data_all_models$abundance$semanas_forecast),
            1
          )
        ) +
        labs(
          x = "Week", 
          y = "E abundance", 
          subtitle = .x, 
          caption = "Vertical dashed line indicated beggining of forecast"
        ) +
        theme(
          axis.title.y = element_text(
            angle = 90, 
            vjust = 0.5
          )
        )
    }
  )      
  
  
  
  # Add crop phenology labels and save plots
  
  
  for (j in seq_along(Localities)) {
    # prep data for plotting
    ## Density data
    data_density_plot <- data_all_models$density %>% 
      filter(locality == Localities[j])
    ## Abundance data
    data_abundance_plot <- data_all_models$abundance %>% 
      filter(locality == Localities[j])
    
    for (i in 1:time_series_length[j]) {
      # Add crop phenology label
      ## density plots
      plots_per_loc_density[[j]] <- plots_per_loc_density[[j]] +
        annotate("text",
                 x = i,
                 y = max(data_density_plot[, "hpdi_max95"]) * 0.9, # add labels at top
                 label = data_density_plot$phenology[i]
        )
      
      ## abundance plots 
      plots_per_loc_abundance[[j]] <- plots_per_loc_abundance[[j]] +
        annotate("text",
                 x = i,
                 y = max(data_abundance_plot[, "hpdi_max95"]) * 0.9, # add labels at top
                 label = data_abundance_plot$phenology[i]
        )
      
    }
    
    # print plots and save them
    ## density plots
    print(plots_per_loc_density[[j]])
    ggsave(
      filename = str_c(
        "plot_forecastvsdata_modelcompare_density_",
        Localities[j], 
        ".png"
      ),
      path = here::here("output", "plots")
    )
    
    ## abundance plots
    print(plots_per_loc_abundance[[j]])
    ggsave(
      filename = str_c(
        "plot_forecastvsdata_modelcompare_abundance_",
        Localities[j], 
        ".png"
      ),
      path = here::here("output", "plots")
    )
    
  }
  
  
  
  
  
  
  
  # Density plots per locality zoomed
  
  
  ############### if u add zoom you wont see phen labs #########################
  
  
  # plots_per_loc_density_zoomed <- list()
  # 
  # for (j in 1:8) {
  # plots_per_loc_density_zoomed[[i]] <- plots_per_loc_density[[i]]                         +  coord_cartesian(
  #     ylim = c(0, max(data_all_models$density$EqNv_obs) * 1.3)
  #   )
  # 
  # }
  
  
  
  
  
  
  
  
  # All plots toghether
  
  plots_all_locs_density <- data_all_models$density %>% 
    ggplot() +
    # Posterior median values
    geom_line(
      aes(
        x = semanas_forecast, 
        y = post_median,
        color = model_name
        
      )
    ) +
    # HPDI 95
    # geom_errorbar(
    #   aes(
    #     x = semanas_forecast, 
    #     ymin = hpdi_min95, 
    #     ymax = hpdi_max95,
    #     color = model_name
    #   ),
    #   alpha = 0.5
    # ) +
    # HPDI 80
  geom_errorbar(
    aes(
      x = semanas_forecast, 
      ymin = hpdi_min80, 
      ymax = hpdi_max80,
      color = model_name
    ),
    alpha = 0.5
  ) +
    # HPDI 60
    # geom_errorbar(
    #   aes(
    #     x = semanas_forecast, 
    #     ymin = hpdi_min60, 
    #     ymax = hpdi_max60,
    #     color = model_name
    #   ),
    #   alpha = 0.5
    # ) +
    
  # Observed values
  geom_point(
    aes(
      x = semanas_forecast,
      y = EqNv_obs
    )
  ) +
    scale_x_continuous(
      breaks = seq(
        0, 
        max(data_all_models$density$semanas_forecast), 
        1
      )
    ) +
    # geom_vline(
    #   xintercept = (max(data_all_models$density$semanas_forecast) - 1),
    #   linetype = "dashed",
    #   alpha = 0.8
    # ) +
    labs(
      x = "Week", 
      y = "E density", 
      #subtitle = Localities[[j]],
      # caption = "Vertical dashed line indicated beggining of forecast"
      color = "Model"
    ) +
    theme(
      axis.title.y = element_text(
        angle = 90, 
        vjust = 0.5
      ), 
      legend.position = "top"
    ) + 
    coord_cartesian(
      ylim = c(0, max(data_all_models$density$EqNv_obs) * 1.3)
    ) +
    facet_wrap(vars(locality))
  
  
  
  # abundance plots
  
  
  plots_all_locs_abundance<- data_all_models$abundance %>% 
    ggplot() +
    # Posterior median values
    geom_line(
      aes(
        x = semanas_forecast, 
        y = post_median,
        color = model_name
        
      )
    ) +
    # HPDI 95
    # geom_errorbar(
    #   aes(
    #     x = semanas_forecast, 
    #     ymin = hpdi_min95, 
    #     ymax = hpdi_max95,
    #     color = model_name
    #   ),
    #   alpha = 0.5
    # ) +
    # HPDI 80
  geom_errorbar(
    aes(
      x = semanas_forecast, 
      ymin = hpdi_min80, 
      ymax = hpdi_max80,
      color = model_name
    ),
    alpha = 0.5
  ) +
    # HPDI 60
    # geom_errorbar(
    #   aes(
    #     x = semanas_forecast, 
    #     ymin = hpdi_min60, 
    #     ymax = hpdi_max60,
    #     color = model_name
    #   ),
    #   alpha = 0.5
    # ) +
    
  # Observed values
  geom_point(
    aes(
      x = semanas_forecast,
      y = EqNv_obs
    )
  ) +
    scale_x_continuous(
      breaks = seq(
        0, 
        max(data_all_models$abundance$semanas_forecast), 
        1
      )
    ) +
    labs(
      x = "Week", 
      y = "EqNv abundance", 
      #subtitle = Localities[[j]],
      # caption = "Vertical dashed line indicated beggining of forecast"
      color = "Model"
    ) +
    theme(
      axis.title.y = element_text(
        angle = 90, 
        vjust = 0.5
      ), 
      legend.position = "top"
    ) + 
    coord_cartesian(
      ylim = c(0, max(data_all_models$abundance$EqNv_obs) * 1.3)
    ) +
    facet_wrap(vars(locality))
  
  
    # Save all locs plots
    ## Density plots 

    print(plots_all_locs_density) 
    ggsave(
      filename = str_c(
        "plot_forecastvsdata_all_locs_density",
        ".png"
      ),
      path = here::here("output", "plots")
    )

    ## Abundance plots 
    print(plots_all_locs_abundance) 
    ggsave(
      filename = str_c(
        "plot_forecastvsdata_all_locs_abundance",
        ".png"
      ),
      path = here::here("output", "plots")
    )
  
  
  return(
    'plots' = list(
      'per_loc_density' = plots_per_loc_density, 
      'per_loc_abundance' = plots_per_loc_abundance, 
      'all_locs_density' = plots_all_locs_abundance, 
      'all_locs_abundance' = plots_all_locs_density
    )
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
}




