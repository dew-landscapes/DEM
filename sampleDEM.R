
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
  
  purrr::walk(fs::dir_ls("function")
              , source
              )
  
  # settings-------
  
  options(scipen = 999)
  
  settings <- list(use_epsg = 7845
                   , target_res = 10
                   , sample_n = 9999
                   )
  
  ras_dir <- fs::path("D:", "env", "data", "raster", "other", "dem")
  
  luraster <- tibble::tribble(
    ~area, ~name, ~short, ~ref_flag, ~smallest,
    "Bakara", "BakaraCP_DEM_2009_10m_MGA54", "Ortho", 1, 0,
    "Bakara", "NASA_DEM_30M", "SRTM", 0, 0,
    "Bakara", "SA_ALOS_AW3D30v4_DSM_30m", "AW3D", 0, 0,
    "Bakara", "SA_Copernicus_GLO30v2022_1_DSM_30m", "COP-DEM DSM", 0, 0,
    "Bakara", "WorldDEM_DTM_04_S34_59_E139_92_DEM", "COP-DEM old DTM", 0, 1,
    "Bakara", "WorldDEM_LIT_04_S34_60_E139_90_DEM", "COP-DEM old LIT", 0, 0,
    "Woakwine", "SA_ALOS_AW3D30v4_DSM_30m", "AW3D", 0, 0,
    "Woakwine", "SA_Copernicus_GLO30v2022_1_DSM_30m", "COP-DEM", 0, 0,
    "Woakwine", "SouthEastLiDAR_15Oct2007_22May2008_DEM_2m_MGA54", "Lidar", 1, 1,
    "Woakwine", "WorldDEM_LIT_03_S37_41_E140_01_DEM", "COP-DEM LIT", 0, 0,
    "Woakwine", "WorldDEM_LIT_04_S37_41_E140_01_DEM", "COP-DEM LIT", 0, 0,
    "Woakwine", "WorldDEM_DTM_04_S37_41_E140_01_DEM", "COP-DEM DTM", 0, 0,
    "Woakwine", "S37365E140006_S37410E140063_LT_DTM", "AW3D", 0, 0
    )
  
  rasters <- fs::dir_ls(ras_dir
                        , regexp = "\\.tif$"
                        , recurse = TRUE
                        ) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::mutate(area = basename(dirname(path))
                  , name = gsub("\\.tif", "", basename(path))
                  , ras = purrr::map(path, terra::rast)
                  ) %>%
    dplyr::left_join(luraster) %>%
    dplyr::filter(!is.na(short)) %>%
    dplyr::mutate(ras2020 = purrr::map(ras
                                       , terra::project
                                       , y = paste0("epsg:"
                                                    , settings$use_epsg
                                                    )
                                       )
                  , original_cm = purrr::map_dbl(ras2020
                                                 , ~ round(terra::res(.)[1] * 100, 0)
                                                 )
                  , short = paste0(short, " ", original_cm)
                  , short = fct_reorder(short
                                        , original_cm
                                        )
                  , short = fct_relevel(short
                                        , grep(luraster$short[luraster$ref_flag == 1], short, value = TRUE)
                                        )
                  ) %>%
    dplyr::arrange(short)
  
  
  # minimum extent, samples and bbox------
  
  naRas <- rasters %>%
    dplyr::filter(as.logical(smallest)) %>%
    dplyr::mutate(naRas = purrr::map(ras2020
                                     , terra::classify
                                     , rcl = matrix(c(-Inf, 0, NA
                                                      , 0, Inf, 1
                                                      )
                                                    , nrow = 2
                                                    , byrow = TRUE
                                                    )
                                     )
                  , naRas = purrr::map(naRas
                                       , terra::project
                                       , y = paste0("epsg:", settings$use_epsg)
                                       , res = settings$target_res
                                       )
                  , ID = purrr::map(naRas
                                    , ~ sample(1:ncell(.)
                                               , size = settings$sample_n
                                               )
                                    )
                  ) %>%
    dplyr::select(area, naRas, ID)
  
  naRas$bbox[naRas$area == "Bakara"] <- sf::st_bbox(c(xmin = 139.981142
                                                      , ymin = -34.565524
                                                      , xmax = 139.955216
                                                      , ymax = -34.58927
                                                      )
                                                    , crs = 4326
                                                    ) %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = sf::st_crs(naRas$naRas[naRas$area == "Bakara"][[1]])) %>%
    sf::st_bbox() %>%
    list(bbox = .)
    
  
  naRas$bbox[naRas$area == "Woakwine"] <- sf::st_bbox(c(xmin = 140.029951
                                                        , ymin = -37.390504
                                                        , xmax = 140.044338
                                                        , ymax = -37.383261
                                                        )
                                                      , crs = 4326
                                                      ) %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = sf::st_crs(naRas$naRas[naRas$area == "Woakwine"][[1]])) %>%
    sf::st_bbox() %>%
    list(bbox = .)
  
  # aligned rasters--------
  
  no_function <- function(obj, ...) {
    
    return(obj)
    
  }
  
  samples <- rasters %>%
    dplyr::left_join(naRas) %>%
    dplyr::filter(purrr::map2_lgl(ras2020
                                  , naRas
                                  , envRaster::test_intersection
                                  )
                  ) %>%
    dplyr::mutate(ras2020 = purrr::map2(ras2020
                                        , naRas
                                        , terra::crop
                                        )
                  , ras2020 = purrr::map(ras2020
                                         , terra::classify
                                         , rcl = cbind(-Inf, -1, NA)
                                         )
                  , old_to_new = settings$target_res / (original_cm / 100)
                  , method = dplyr::case_when(old_to_new > 3 ~ "aggregate"
                                              , old_to_new < 0.5 ~ "disagg"
                                              , TRUE ~ "no_function"
                                              )
                  , fact = dplyr::case_when(method == "aggregate" ~ round(old_to_new, 0)
                                            , method == "disagg" ~ 3
                                            , method == "no_function" ~ Inf
                                            )
                  , ras2020 = purrr::pmap(list(ras2020
                                               , method
                                               , fact
                                               )
                                       , function(a, b, c) {
                                         
                                         get(b)(a, c)
                                         
                                       }
                                       )
                  , ras2020 = purrr::map2(ras2020
                                          , naRas
                                          , terra::project
                                          , method = "bilinear"
                                          )
                  , ras2020 = purrr::map2(ras2020
                                          , naRas
                                          , terra::mask
                                          )
                  ) %>%
    dplyr::arrange(short)
  
  
  #------Terrain-------
  
  windows <- tibble::tibble(window = c(3, 5))
  
  terr <- samples %>%
    dplyr::cross_join(windows) %>%
    dplyr::mutate(terr = purrr::map2(ras2020
                                     , window
                                     , MultiscaleDTM::Qfit
                                     , na.rm = TRUE
                                     , include_scale = TRUE
                                     )
                  )
  
  
  # values --------
  
  vals <- terr %>%
    dplyr::mutate(rasVal = purrr::map2(terr
                                       , ID
                                       , terra::extract
                                       , xy = TRUE
                                       )
                  ) %>%
    dplyr::select(!where(is.list), ID, rasVal) %>%
    tidyr::unnest(cols = c(ID, rasVal))
  
  
#------Analysis--------
  
  terr_cat <- "features"
  terr_cont <- grep(terr_cat
                    , as.character(formals(MultiscaleDTM::Qfit)$metrics)[-1]
                    , value = TRUE
                    , invert = TRUE
                    )
  
    
  # continuous-------
  
  cont <- vals %>%
    dplyr::select(ID
                  , x
                  , y
                  , any_of(names(samples))
                  , matches(terr_cont)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cont)
                        , names_to = "metric"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(area, ID, name = "count") %>%
    dplyr::group_by(area) %>%
    dplyr::filter(count == max(count)) %>%
    dplyr::ungroup() %>%
    tidyr::separate_wider_delim(metric
                                , delim = "_"
                                , names = c("metric", "window")
                                ) %>%
    dplyr::group_by(area, ID, window, metric) %>%
    dplyr::mutate(ref = value[as.logical(ref_flag)]) %>%
    dplyr::filter(!is.na(ref)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(diff = ref - value) %>%
    tidyr::nest(data = -c(area, metric)) %>%
    dplyr::mutate(plot = purrr::map2(data
                                     , metric
                                    , cont_plot
                                    , prob = 0.00
                                    )
                  , diff_plot = purrr::map2(data
                                            , metric
                                            , cont_plot
                                            , prob = 0.00
                                            , diff = TRUE
                                            )
                  )
                        
    
  # categorical-------
  
  categ <- vals %>%
    dplyr::select(area
                  , ID
                  , x
                  , y
                  , any_of(names(samples))
                  , matches(terr_cat)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cat)
                        , names_to = "metric"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(area, ID, name = "count") %>%
    dplyr::group_by(area, ID) %>%
    dplyr::filter(count == max(count)) %>%
    dplyr::ungroup() %>%
    tidyr::separate_wider_delim(metric
                                , delim = "_"
                                , names = c("metric", "window")
                                ) %>%
    dplyr::count(area, short, original_cm, metric, window, value) %>%
    dplyr::group_by(area, short, original_cm, metric, window) %>%
    dplyr::mutate(values = sum(n)
                  , prop = n / values
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(area, short, original_cm, metric, window)
    
  categ_compare_ref <- vals %>%
    dplyr::select(ID
                  , x
                  , y
                  , any_of(names(samples))
                  , matches(terr_cat)
                  ) %>%
    tidyr::pivot_longer(matches(terr_cat)
                        , names_to = "metric"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(area, ID, name = "count") %>%
    dplyr::group_by(area, ID) %>%
    dplyr::filter(count == max(count)) %>%
    dplyr::ungroup() %>%
    tidyr::separate_wider_delim(metric
                                , delim = "_"
                                , names = c("metric", "window")
                                ) %>%
    dplyr::group_by(area, ID, window) %>%
    dplyr::mutate(has_ref = sum(as.logical(ref_flag)) > 0) %>%
    dplyr::filter(has_ref) %>%
    dplyr::mutate(reference = value[ref_flag == 1]) %>%
    dplyr::ungroup() %>%
    dplyr::count(area, short, original_cm, metric, window, reference, value) %>%
    dplyr::group_by(area, short, original_cm, metric, window, reference) %>%
    dplyr::mutate(references = sum(n)
                  , prop = n / references
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(area, short, original_cm, metric, window, reference)
  
  