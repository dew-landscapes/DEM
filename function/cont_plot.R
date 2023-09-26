
  
  cont_plot <- function(data
                        , title = ""
                        , prob = 0
                        , diff = FALSE
                        ) {
  
    if(diff) {
    
      data <- data %>%
        dplyr::mutate(value = diff) %>%
        dplyr::filter(!as.logical(ref_flag))
      
      }
  
    ggplot(data = data %>%
             dplyr::filter(value >= quantile(value, probs = prob)
                           , value <= quantile(value, probs = 1 - prob)
                           )
           , aes(value
                 , forcats::fct_rev(short)
                 , fill = as.factor(round(original_cm, 1))
                 , height = after_stat(density)
                 )
           ) +
      geom_density_ridges(scale = 1, stat = "density", trim = TRUE) +
      facet_wrap( ~ window
                  , scales = "free"
                  ) +
      scale_fill_viridis_d() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste0(title
                          , if(diff) paste0(". Difference from reference value")
                          , if(prob > 0) paste0(". +/-"
                                                , 100 * prob
                                                , "% percentiles excluded"
                                                )
                          )
           , fill = "Original resolution (cm)"
           , y = "Sample DEM"
           )
  
  }
  