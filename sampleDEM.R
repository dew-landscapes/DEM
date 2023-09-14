
  # Setup---------

  library(tidyverse)
  library(fs)
  library(terra)
  library(sf)
  library(ggridges)
  library(furrr)
  library(gridExtra)
  library(tmap)

  library(envRaster)
  
  library(knitr)
  library(rmarkdown)
  
  options(scipen = 999)
  
  # Need settings - see 'runs' in 'run.R'
  
  #-------Rasters--------
  
  types <- tribble(
    ~lookfor, ~short, ~original_cm,
    "AW3D_30", "AW3D", 2860,
    #"SA_Sample", "AW3D", 264,
    "bak2_bp", "Bak", 1060,
    "TOPO.NASA", "SRTM", 3170,
    "Copernicus", "Cop", 2860,
    "WorldDEM_DTM", "wDEM Full", 1200,
    "15Oct2007_22May2008", "Lidar", 210,
    #"22May_24Aug2018", "SECLidar2018",
    "S37410E140063_LT_DTM", "AW3D", 463,
    "WorldDEM_LIT_03", "wDEM Lite", 1140,
    "WorldDEM_LIT_04", "wDEM Lite", 858
  ) %>%
    dplyr::mutate(short = paste0(short
                                 , " "
                                 , stringr::str_pad(original_cm
                                                    , 4
                                                    , pad = "0"
                                                    )
                                 )
                  , ref_flag = grepl(settings$reference, short)
                  )
  
  rasters <- tibble(path = fs::dir_ls(fs::path("D:"
                                               , "env"
                                               , "data"
                                               , "raster"
                                               , "other"
                                               , "dem"
                                               , settings$area
                                               )
                                      , regexp = "\\.tif$"
                                      , recurse = TRUE
                                      )
                    ) %>%
    dplyr::mutate(lookfor = str_extract(path
                                        , paste0(types$lookfor
                                                 , collapse = "|"
                                                 )
                                        )
                  ) %>%
    dplyr::left_join(types) %>%
    dplyr::filter(!is.na(short)
                  , !grepl("SA54", path)
                  ) %>%
    dplyr::mutate(ras = purrr::map(path, terra::rast)
                  , short = fct_reorder(short
                                      , original_cm
                                      )
                  , short = fct_relevel(short, types$short[types$ref_flag])
                  ) %>%
    dplyr::arrange(short)
  
  reference <- grep(settings$reference, rasters$short, value = TRUE)
  
  
  # Minimum extent of data across all rasters
  naPolyRas <- rasters %>%
    dplyr::filter(grepl(settings$smallest, short)) %>%
    dplyr::pull(ras) %>%
    `[[`(1) %>%
    terra::classify(rcl = matrix(c(-Inf, 0, NA
                                   , 0, Inf, 1
                                   )
                                , nrow = 2
                                , byrow = TRUE
                                )
                    ) %>%
    terra::project(y = paste0("epsg:", 7850)
                   , res = settings$target_res
                   )
  
  naPoly <- naPolyRas %>%
    terra::as.polygons() %>%
    sf::st_as_sf()
  
  
  # sample points---------
  
  pts <- sf::st_sample(naPoly %>% sf::st_as_sf()
                       , settings$sample_n
                       )
  
  
  # aligned rasters--------
  
  no_function <- function(obj, ...) {
    
    return(obj)
    
  }
  
  samples <- rasters %>%
    dplyr::filter(purrr::map_lgl(ras
                                 , envRaster::test_intersection
                                 , naPoly
                                 )
                  ) %>%
    dplyr::mutate(ras = purrr::map(ras
                                    , function(x) terra::crop(x
                                                              , y = naPoly %>%
                                                                sf::st_transform(crs = sf::st_crs(x))
                                                              )
                                    )
                  , ras = purrr::map(ras
                                      , terra::project
                                      , y = paste0("epsg:", 7850)
                                      )
                  , ras = purrr::map(ras
                                      , terra::classify
                                      , rcl = cbind(-Inf, 0, NA)
                                      )
                  , original_res = purrr::map_dbl(ras, function(x) terra::res(x)[1])
                  , old_to_new = settings$target_res / original_res
                  , method = dplyr::case_when(old_to_new > 3 ~ "aggregate"
                                              , old_to_new < 0.5 ~ "disagg"
                                              , TRUE ~ "no_function"
                                              )
                  , fact = dplyr::case_when(method == "aggregate" ~ round(old_to_new, 0)
                                            , method == "disagg" ~ 3
                                            , method == "no_function" ~ Inf
                                            )
                  , ras = purrr::pmap(list(ras
                                            , method
                                            , fact
                                            )
                                       , function(a, b, c) {
                                         
                                         get(b)(a, c)
                                         
                                       }
                                       )
                  , ras = purrr::map(ras
                                      , terra::project
                                      , y = naPolyRas
                                      , method = "bilinear"
                                      )
                  , ras = purrr::map(ras
                                     , terra::mask
                                     , mask = naPolyRas
                                     )
                  ) %>%
    dplyr::arrange(short)
  
  
  #------Terrain-------
  
  windows <- tibble::tibble(window = c(3, 9))
  
  terr <- samples %>%
    dplyr::cross_join(windows) %>%
    dplyr::mutate(terr = purrr::map2(ras
                                     , window
                                     , MultiscaleDTM::Qfit
                                     , na.rm = TRUE
                                     , include_scale = TRUE
                                     )
                  )
  
  
  # values --------
  
  vals <- terr %>%
    dplyr::mutate(rasVal = purrr::map(terr
                                      , raster::extract
                                      , y = terra::vect(pts)
                                      )
                  ) %>%
    dplyr::mutate(rasVal = purrr::map(rasVal
                                      , ~as_tibble(.) %>%
                                        dplyr::bind_cols(st_coordinates(pts) %>%
                                                           as_tibble()
                                                         )
                                      )
                  ) %>%
    dplyr::select(!where(is.list), rasVal) %>%
    tidyr::unnest(cols = c(rasVal))
  
  
#------Analysis--------
  
  terr_cat <- "features"
  terr_cont <- grep(terr_cat
                    , as.character(formals(MultiscaleDTM::Qfit)$metrics)[-1]
                    , value = TRUE
                    , invert = TRUE
                    )
  
    
  # continuous-------
  
  cont <- vals %>%
    dplyr::select(any_of(names(samples))
                  , ID
                  , matches(terr_cont)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cont)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(ID, name = "count") %>%
    dplyr::filter(count == max(count)) %>%
    tidyr::separate_wider_delim(name
                                , delim = "_"
                                , names = c("name", "window")
                                ) %>%
    tidyr::nest(data = -c(name)) %>%
    dplyr::mutate(plot = purrr::map2(data
                                     , name
                                    , cont_plot
                                    , prob = 0.00
                                    )
                  , diff_plot = purrr::map2(data
                                            , name
                                            , cont_plot
                                            , prob = 0.00
                                            , diff = TRUE
                                            )
                  )
                        
    
  # categorical-------
  
  categ <- vals %>%
    dplyr::select(any_of(names(samples))
                  , ID
                  , matches(terr_cat)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cat)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(ID, name = "count") %>%
    dplyr::filter(count == max(count)) %>%
    tidyr::separate_wider_delim(name
                                , delim = "_"
                                , names = c("name", "window")
                                ) %>%
    dplyr::count(short, original_res, name, window, value) %>%
    dplyr::group_by(short, original_res, name, window) %>%
    dplyr::mutate(values = sum(n)
                  , prop = n / values
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(short, original_res, name, window, value)
    
  categ_compare_ref <- vals %>%
    dplyr::select(any_of(names(samples))
                  , ID
                  , matches(terr_cat)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cat)) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::separate_wider_delim(name
                                , delim = "_"
                                , names = c("name", "window")
                                ) %>%
    dplyr::group_by(ID, window) %>%
    dplyr::mutate(has_ref = length(short[short == settings$reference]) > 0) %>%
    dplyr::filter(has_ref) %>%
    dplyr::mutate(reference = value[short == settings$reference]) %>%
    dplyr::ungroup() %>%
    #dplyr::filter(short != settings$reference) %>%
    dplyr::count(short, original_res, name, window, value, reference) %>%
    dplyr::group_by(short, original_res, name, window, reference) %>%
    dplyr::mutate(references = sum(n)
                  , prop = n / references
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(short, original_res, name, window, reference, value)
  
  