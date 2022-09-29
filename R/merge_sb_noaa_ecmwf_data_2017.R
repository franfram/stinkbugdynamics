#' @title Merge 2017 data 
#' @description Merge stink bug and meteorological data. 
#' @export
#' @return tidy `tibble`, ready for creating JAGS' input data
#' @param wrangled_stinkbug_data_2017
#' @param wrangled_noaa_data_2017
#' @param wrangled_ecmwf_data_2017
merge_sb_noaa_ecmwf_data_2017 <- function(
  wrangled_stinkbug_data_2017, 
  wrangled_noaa_data_2017,
  wrangled_ecmwf_data_2017
) {

  # compute useful object for selecting columns
  col_sb <- colnames(all_of(wrangled_stinkbug_data_2017 %>% select(-week)))

  # merge stinkbug data with noaa data
  merged_sb_noaa_data_2017 <- left_join(
    x = wrangled_stinkbug_data_2017,
    y = wrangled_noaa_data_2017,
    by = c(
      "Localidad" = "Localidad",
      "Fecha" = "DATE"
    )
  )
  # select variables of interest
  merged_sb_noaa_data_2017 <- merged_sb_noaa_data_2017 %>%
    select(
      all_of(col_sb), 
      NOAA.air.temp.weekly.avg_2017
    )
  
    
  # compute another useful object for selecting columns
  col_sb_noaa <- colnames(all_of(merged_sb_noaa_data_2017))
    
    
  # merge sb-noaa data with ecmwf data
  merged_sb_noaa_ecmwf_data_2017 <- left_join(
    x = merged_sb_noaa_data_2017,
    y = wrangled_ecmwf_data_2017,
    by = c(
      "Localidad" = "Localidad",
      "Fecha" = "Date"
    )
  )
    
  merged_sb_noaa_ecmwf_data_2017 <- merged_sb_noaa_ecmwf_data_2017 %>% 
    select(
      all_of(col_sb_noaa), 
      t2m_wmean,
      tp_wmean,
      d2m_wmean
    )
  
  # ungroup
  merged_sb_noaa_ecmwf_data_2017 <- ungroup(merged_sb_noaa_ecmwf_data_2017) %>% 
    ## add week again (removed at beggining)
    mutate(
      week = wrangled_stinkbug_data_2017 %>% pull(week) 
    ) %>% 
    ## re order columns
    select(Localidad, Fecha, everything())
    
  
  # return target object
  return(merged_sb_noaa_ecmwf_data_2017)
}


