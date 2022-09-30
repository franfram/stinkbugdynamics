#' .. content for \description{
#'  Plot all the posterior parameters.  
#' } (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title Plot posterior parameters 
#' @param jags_draws pass string 'jags_2017_draws_model_<MODELNAME>' as argument. <MODELNAME>
#' can be `M1_b0` or `M3`
#' @param useful_objects_2017
#' @param merged_sb_noaa_ecmwf_data_2017
#' @param model_name pass string `M1_b0` or `M3`
#' @return list where each element contains the plots for a given parameter
#' @author franfram
#' @export
plot_post_params <- function(
  jags_draws = jags_2017_draws_model_M1_b0,
  useful_objects_2017,
  merged_sb_noaa_ecmwf_data_2017, 
  model_name  
) {


 
  if (model_name == "M1_b0") {

  

     # jags_draws  <- tar_read(jags_2017_draws_model_M1_b0)
     # useful_objects_2017 <- tar_read(useful_objects_2017)
     # merged_sb_noaa_ecmwf_data_2017 <- tar_read(merged_sb_noaa_ecmwf_data_2017)








    # Useful objects
    localities <- useful_objects_2017$for_jags$localities
    n_localities <- useful_objects_2017$for_jags$n_localities
    localities_names <- useful_objects_2017$for_jags$localities
    
    "this can come from the useful_objects in order to get the names of the pars to plot"
    # Define parameters that will be used for plotting
    ## Locality params
    params_to_plot_locality <- c(
      "bF" = 'bF', 
      "bT" = 'bT.',  
      "bTP" = 'bTP', 
      "bDP" = 'bDP', 
      'I' = 'I'
    )
    # Global params
    params_to_plot_global<- c(
      'muF' = 'muF', 
      'muT' = 'muT.', 
      'muTP' = 'muTP', 
      'muDP' = 'muDP', 
      'muI' = 'muI'#,

      # 'sigmaF' = 'sigmaF', 
      # 'sigmaT' = 'sigmaT.', 
      # 'sigmaTP' = 'sigmaTP', 
      # 'sigmaDP' = 'sigmaDP', 
      # 'sigmaI' = 'sigmaI'

    )

    # Grab just a few posterior samples for the plots
    "samplesplot <- 1e4!!!!!!!!!!!!!!!!!!!!" 
    ## Define amount of samples
    samplesplot  <- 1e4
    ## Sample
    jags_draws_few <- jags_draws[sample(nrow(jags_draws), samplesplot),] 

  

    # Create a list where each element contains the samples for every locality of
    # given parameter
    post_param_samples_global <- params_to_plot_global %>% 
      map(~{jags_draws_few %>% select(starts_with(.x))})

    post_param_samples_locality <- params_to_plot_locality %>% 
      map(~{jags_draws_few %>% select(starts_with(.x))})


    # Get names of parameters to plot
    ## Locality params 
    ridgeplot_params_locality <- names(params_to_plot_locality)# c("bF", "bT", "bTP", "bDP", "I")

    "this could be done better with stringr str_extract to extract from params_to plot the parameter that start with b or I for global ones, and mu or sigma for local ones"
    ## Global params
    ridgeplot_params_global <- names(params_to_plot_global)


    # Make a tuple for later use in the pivot longer
    ## Compute names of tuple
    ### Locality params names
    level_key_names_locality <- map(post_param_samples_locality, colnames)

    ### Global params names
    #level_key_names_global <- map(post_param_samples_global, colnames)

    ## Make tuple 
    ### Locality params
    level_key_map_locality <- names(params_to_plot_locality) %>% # name of each locality parameter 
      map(~{
        setNames(
          # Values
          localities,
          # Names
          level_key_names_locality[[.x]] 
        )
      })



    # Create data for plotting
    ## For locality params
    ### Make a list where each parameter has all the samples arranged per locality
    ridgeplot_data_locality <- #ridgeplot_params_global %>% # params to be plotted with ridgeplots
      map2(
        .x = ridgeplot_params_locality, 
        .y = c(1:length(ridgeplot_params_locality)), 
        # Samples of each param
        .f =  ~{post_param_samples_locality[[.x]] %>% 
          # Pivot longer
          pivot_longer(
            cols = everything(), 
            names_to = 'locality', 
            values_to = str_c(.x, '_post_samples')
          ) %>%
          # Sort values so that consequent rows have belong to the same locality 
          arrange(locality) %>% 
          # Recode locality variable so that values have the corresponding locality name
          mutate(
            locality = recode(
              locality, !!!level_key_map_locality[[.y]]
            )
          )
        }
      )

    ## For global params
    ### Merge all the samples of each parameter into a single tibble
    ridgeplot_data_merged_global <- reduce(post_param_samples_global, bind_cols) %>% 
      # Add Model column
      mutate('Model' = as.factor(c(rep('M1_b0', samplesplot)))) %>% 
      # Arrange columns
      select(Model, everything())




    # Merge all data sets inside `ridgeplot_data` list
    ## For locality params
    ridgeplot_data_merged_locality <- reduce(ridgeplot_data_locality, bind_cols) %>% 
      # Remove duplicated columns due to bind_cols() 
      select('locality...1', starts_with(ridgeplot_params_locality)) %>% 
      # Rename locality...1
      rename('locality' = 'locality...1')


    # Plot parameters per locality
    ## Base size of the plot 
    bs <- 19 
    
    viridis_options <- c("viridis", "magma", "cividis", "mako", "rocket")
    
    ## Plot
    
    ridgeplot_locality <- 
      map2(
        .x = names(params_to_plot_locality), 
        .y = viridis_options,
        .f = ~{
          ggplot(
            data = ridgeplot_data_merged_locality,
            aes_string(
              x = str_c(.x, '_post_samples'),
              y = 'locality', fill = '..x..'
            )
          ) +
          geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
          scale_fill_viridis(name = "Post", option = .y) +
          #coord_cartesian(xlim = c(-0.5, 0.5)) +
          labs(
            title = str_c(.x, " parameter per Locality"), 
            x = "Posterior value"
          ) +
          theme_bw(base_size = bs) +
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_text(angle = 45, size = 12),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(size = 20)
          )
        }
      )
    
    # Plot global parameter
    
    ridgeplot_global <- 
      map2(
        .x = names(params_to_plot_global), 
        .y = viridis_options,
        .f = ~{
          ggplot(
            ridgeplot_data_merged_global,  
            aes_string(
              x = str_c(.x, ".EqNv.dat"), 
              y = "Model", fill = "..x.."
            )
          ) +
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
            scale_fill_viridis(name = "Post", option = .y) +
            coord_cartesian(xlim = c(-5, 5)) +
            labs(
              title = .x,  
              x = "Posterior value"
            ) +
            theme_bw(base_size = bs) +
            theme(
              legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_text(angle = 90),
              plot.title = element_text(size = 22)
            )
        }
      )
    

    ridgeplot_patchwork <- map(1:5, ~{ridgeplot_locality[[.x]] + ridgeplot_global[[.x]]})






    # Pobs posterior vs prior plot
    ## Data
    post_pobs_samples <- jags_draws_few %>% select(starts_with("pobs"))

    
    ## Plot

    x_lower_norm <- 0
    x_upper_norm <- 1

    # Prior vs Posterior
    
    plot_prior_posterior <- ggplot(data.frame(x = c(x_lower_norm, x_upper_norm)), aes(x = x)) +
      xlim(c(x_lower_norm, x_upper_norm)) +
      stat_function(fun = dbeta, args = list(shape1 = 8, shape2 = 2.5)) +
      geom_density(
        data = post_pobs_samples, 
        aes(pobs.EqNv.dat), 
        fill = "#69b3a2", 
        color = "#e9ecef",
        alpha = 0.5
      ) +
      annotate("text", x = 0.64, y = 3.1, label = "P \n Posterior") +
      annotate("text", x = 0.85, y = 1.3, label = "P \n Prior") +
      labs(
        #title = "Pobs",
        x = "Prior vs Posterior",
        y = "Density"
      ) +
      theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
      )
    


    # Save plots
    ## Prior vs Posterior plots
    ggsave(
      plot = plot_prior_posterior, 
      filename = here('output', 'plots', "plot_prior_posterior_M1_b0.png"), 
      width = 12,
      height = 10
    )

    ## Ridgeplots
    map(
      1:length(ridgeplot_params_locality), 
      ~{ggsave(
        plot = ridgeplot_patchwork[[.x]],
        filename = here(
          'output', 
          'plots', 
          str_c("ridgeplot_", ridgeplot_params_locality[[.x]], ".png")
        ),
        width = 13,
        height = 10 
      )} 
    )
  } else if (model_name == "M3") {

    # Interactive targets 
    jags_draws <- tar_read(jags_2017_draws_model_M3) 
    useful_objects_2017 <- tar_read(useful_objects_2017)


    localities <- useful_objects_2017$for_jags$localities
    n_localities <- useful_objects_2017$for_jags$n_localities
    localities_names <- useful_objects_2017$for_jags$localities
    
    "this can come from the useful_objects in order to get the names of the pars to plot"
    # Define parameters that will be used for plotting
    ## Locality params
    params_to_plot_locality <- c(
      "bF.VR1.2.3" = 'bF.VR1.2.3', 
      "bF.R4.5.6.7" = 'bF.R4.5.6.7', 
      "bF.R8" = 'bF.R8', 
      "bT" = 'bT.',  
      "bTP" = 'bTP', 
      "bDP" = 'bDP'#, 
      #'I' = 'I'
    )
    # Global params
    params_to_plot_global<- c(
      'muF.VR1.2.3' = 'muF.VR1.2.3', 
      'muF.R4.5.6.7' = 'muF.R4.5.6.7', 
      'muF.R8' = 'muF.R8', 
      'muT' = 'muT.', 
      'muTP' = 'muTP', 
      'muDP' = 'muDP', 
      'muI' = 'muI'#,
    )



    samplesplot  <- 1e4
    ## Sample
    jags_draws_few <- jags_draws[sample(nrow(jags_draws), samplesplot),] 

  

    # Create a list where each element contains the samples for every locality of
    # given parameter
    post_param_samples_global <- params_to_plot_global %>% 
      map(~{jags_draws_few %>% select(starts_with(.x))})

    post_param_samples_locality <- params_to_plot_locality %>% 
      map(~{jags_draws_few %>% select(starts_with(.x))})


    # Get names of parameters to plot
    ## Locality params 
    ridgeplot_params_locality <- names(params_to_plot_locality)  %>% print# c("bF", "bT", "bTP", "bDP", "I")

    "this could be done better with stringr str_extract to extract from params_to plot the parameter that start with b or I for global ones, and mu or sigma for local ones"
    ## Global params
    ridgeplot_params_global <- names(params_to_plot_global)  %>% print


    # Make a tuple for later use in the pivot longer
    ## Compute names of tuple
    ### Locality params names
    level_key_names_locality <- map(post_param_samples_locality, colnames) %>% print

    ### Global params names
    #level_key_names_global <- map(post_param_samples_global, colnames)

    ## Make tuple 
    ### Locality params
    level_key_map_locality <- names(params_to_plot_locality) %>% # name of each locality parameter 
      map(~{
        setNames(
          # Values
          localities,
          # Names
          level_key_names_locality[[.x]] 
        )
      })





    # Create data for plotting
    ## For locality params
    ### Make a list where each parameter has all the samples arranged per locality
    ridgeplot_data_locality <- #ridgeplot_params_global %>% # params to be plotted with ridgeplots
      map2(
        .x = ridgeplot_params_locality, 
        .y = c(1:length(ridgeplot_params_locality)), 
        # Samples of each param
        .f =  ~{post_param_samples_locality[[.x]] %>% 
          # Pivot longer
          pivot_longer(
            cols = everything(), 
            names_to = 'locality', 
            values_to = str_c(.x, '_post_samples')
          ) %>%
          # Sort values so that consequent rows have belong to the same locality 
          arrange(locality) %>% 
          # Recode locality variable so that values have the corresponding locality name
          mutate(
            locality = recode(
              locality, !!!level_key_map_locality[[.y]]
            )
          )
        }
      )

    ## For global params
    ### Merge all the samples of each parameter into a single tibble
    ridgeplot_data_merged_global <- reduce(post_param_samples_global, bind_cols) %>% 
      # Add Model column
      mutate('Model' = as.factor(c(rep('M3', samplesplot)))) %>% 
      # Arrange columns
      select(Model, everything())




    # Merge all data sets inside `ridgeplot_data` list
    ## For locality params
    ridgeplot_data_merged_locality <- reduce(ridgeplot_data_locality, bind_cols) %>% 
      # Remove duplicated columns due to bind_cols() 
      select('locality...1', starts_with(ridgeplot_params_locality)) %>% 
      # Rename locality...1
      rename('locality' = 'locality...1')


    # Plot parameters per locality
    ## Base size of the plot 
    bs <- 19 
    
    viridis_options_locality <- c("viridis", "magma", "cividis", "mako", "rocket", "turbo")
    viridis_options_global <- c("viridis", "magma", "cividis", "mako", "rocket", "turbo", "plasma")


    ## Plot
    
    ridgeplot_locality <- 
      map2(
        .x = names(params_to_plot_locality), 
        .y = viridis_options_locality,
        .f = ~{
          ggplot(
            data = ridgeplot_data_merged_locality,
            aes_string(
              x = str_c(.x, '_post_samples'),
              y = 'locality', fill = '..x..'
            )
          ) +
          geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
          scale_fill_viridis(name = "Post", option = .y) +
          #coord_cartesian(xlim = c(-0.5, 0.5)) +
          labs(
            title = str_c(.x, " parameter per Locality"), 
            x = "Posterior value"
          ) +
          theme_bw(base_size = bs) +
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_text(angle = 45, size = 12),
            axis.text.x = element_text(size = 10),
            plot.title = element_text(size = 15)
          )
        }
      )
    
    # Plot global parameter
    
    ridgeplot_global <- 
      map2(
        .x = names(params_to_plot_global), 
        .y = viridis_options_global,
        .f = ~{
          ggplot(
            ridgeplot_data_merged_global,  
            aes_string(
              x = str_c(.x, ".EqNv.dat"), 
              y = "Model", fill = "..x.."
            )
          ) +
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
            scale_fill_viridis(name = "Post", option = .y) +
            coord_cartesian(xlim = c(-5, 5)) +
            labs(
              title = .x,  
              x = "Posterior value"
            ) +
            theme_bw(base_size = bs) +
            theme(
              legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_text(angle = 90),
              plot.title = element_text(size = 15)
            )
        }
      )
    

    ridgeplot_patchwork <- map(1:6, ~{ridgeplot_locality[[.x]] + ridgeplot_global[[.x]]})



    # Pobs posterior vs prior plot
    ## Data
    post_pobs_samples <- jags_draws_few %>% select(starts_with("pobs"))

    
    ## Plot

    x_lower_norm <- 0
    x_upper_norm <- 1

    # Prior vs Posterior
    
    plot_prior_posterior <- ggplot(data.frame(x = c(x_lower_norm, x_upper_norm)), aes(x = x)) +
      xlim(c(x_lower_norm, x_upper_norm)) +
      stat_function(fun = dbeta, args = list(shape1 = 8, shape2 = 2.5)) +
      geom_density(
        data = post_pobs_samples, 
        aes(pobs.EqNv.dat), 
        fill = "#69b3a2", 
        color = "#e9ecef",
        alpha = 0.5
      ) +
      annotate("text", x = 0.64, y = 3.1, label = "P \n Posterior") +
      annotate("text", x = 0.85, y = 1.3, label = "P \n Prior") +
      labs(
        #title = "Pobs",
        x = "Prior vs Posterior",
        y = "Density"
      ) +
      theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
      )
    


    # Save plots
    ## Prior vs Posterior plots
    ggsave(
      plot = plot_prior_posterior, 
      filename = here('output', 'plots', "plot_prior_posterior_M3.png"), 
      width = 12,
      height = 10
    )

    ## Ridgeplots
    map(
      1:length(ridgeplot_params_locality), 
      ~{ggsave(
        plot = ridgeplot_patchwork[[.x]],
        filename = here(
          'output', 
          'plots', 
          str_c("ridgeplot_", ridgeplot_params_locality[[.x]], ".png")
        ),
        width = 13,
        height = 10 
      )} 
    )

  }
  return(
    list(
      'ridgeplot' = ridgeplot_patchwork,
      'prior_posterior' = plot_prior_posterior
    )
   )
  


}
