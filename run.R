
  library(magrittr)

  runs <- tibble::tibble(area = c("Bakara", "Woakwine")
                         , reference = c("Bak", "Lidar")
                         , smallest = c("wDEM Full", "Lidar")
                         ) %>%
    dplyr::mutate(target_res = 10
                  , n_sample = 9999
                  )
  
  for(i in 1:nrow(runs)) {
    
    settings <- runs[i,]

    source("sampleDEM.R")
    
    # Clean up previous knits
    
    fs::file_delete(fs::dir_ls(regexp = "_book|_main"))
    
    bookdown::render_book()
    
    to_network_path <- path(paste0("//env.sa.gov.au/dfsroot/IST/DEHProjects/Landscapes/DEM_samples")
                            , settings$area
                            )
    
    dirs_regex <- c("_book")
      
    dirs_to_copy <- tibble(path = fs::dir_ls(regexp = paste0(dirs_regex,collapse = "|"))) %>%
      dplyr::filter(!grepl("~",path)) %>%
      dplyr::mutate(to_network_path = path(to_network_path, basename(path)))
      
    fs::dir_copy(dirs_to_copy$path
                 , dirs_to_copy$to_network_path
                 , overwrite = TRUE
                 )
    
  }

  