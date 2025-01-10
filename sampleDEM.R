
  # packages -----
  packages <- 
    sort(
      unique(
        c("base"
        
          # reports
          , "knitr"
          , "rmarkdown"
          , "bookdown"
          
          # tidyverse
          , "dplyr"
          , "tidyr"
          , "purrr"
          , "ggplot2"
          , "tibble"
          , "readr"
          , "forcats"
          , "stringr"
          , "lubridate"
          
          # misc
          , "fs"
          , "ggridges"
          
          # gis
          , "terra"
          , "sf"
          , "tmap"
          
          # env
          , "envRaster"
          )
        )
      )
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
    
  if(length(new_packages)) install.packages(new_packages)
  
  purrr::walk(packages
              , library
              , character.only = TRUE
              )
  
  
  # functions------
  
  purrr::walk(fs::dir_ls("function")
              , source
              )
  
  # settings-------
  
  options(scipen = 999)
  
  settings <- list(use_epsg = 7845
                   , target_res = 30
                   , sample_n = 5000
                   )
  
  ras_dir <- fs::path("H:", "data", "raster", "dem")
  
  luraster <- tibble::tribble(
    ~area, ~name, ~short, ~year, ~ref_flag, ~smallest, ~source, ~licence, ~paper,
    
    "Bakara", "BakaraCP_DEM_2009_10m_MGA54", "Ortho", 2009, 1, 0
    , "https://www.environment.sa.gov.au/topics/science/mapland/aerial-photography"
    , NA, NA,
    
    "Bakara", "NASA_DEM_30M", "SRTM", 2000, 0, 0
    , "https://www.earthdata.nasa.gov/sensors/srtm#:~:text=The%20Shuttle%20Radar%20Topography%20Mission,global%20dataset%20of%20land%20elevations."
    , NA, "https://www2.jpl.nasa.gov/srtm/SRTM_paper.pdf",
    
    "Bakara", "SA_ALOS_AW3D30v4_DSM_30m", "AW3D", 2011, 0, 0
    , "https://www.aw3d.jp/en/products/standard/"
    , NA, "https://www.aw3d.jp/wp/wp-content/themes/AW3DEnglish/technology/doc/pdf/technology_02.pdf",
    
    "Bakara", "SA_Copernicus_GLO30v2022_1_DSM_30m", "WldDEM 30", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Bakara", "WorldDEM_DTM_04_S34_59_E139_92_DEM", "WldDEM DTM", 2015, 0, 1
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Bakara", "WorldDEM_LIT_04_S34_60_E139_90_DEM", "WldDEM LIT", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Woakwine", "SA_ALOS_AW3D30v4_DSM_30m", "AW3D", 2011, 0, 0
    , "https://www.aw3d.jp/en/products/standard/"
    , NA, "https://www.aw3d.jp/wp/wp-content/themes/AW3DEnglish/technology/doc/pdf/technology_02.pdf",
    
    "Woakwine", "SA_Copernicus_GLO30v2022_1_DSM_30m", "WldDEM 30", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Woakwine", "SouthEastLiDAR_15Oct2007_22May2008_DEM_2m_MGA54", "Lidar", 2008, 1, 0
    , "https://data.sa.gov.au/data/dataset/elvis-digital-elevation-model-imagery-catalog"
    , NA, NA,
    
    "Woakwine", "WorldDEM_LIT_03_S37_41_E140_01_DEM", "WldDEM LIT", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Woakwine", "WorldDEM_LIT_04_S37_41_E140_01_DEM", "WldDEM LIT", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Woakwine", "WorldDEM_DTM_04_S37_41_E140_01_DEM", "WldDEM DTM", 2015, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/worlddem-thematic-layers-and-derivatives/"
    , NA, "https://spacedata.copernicus.eu/documents/20123/121239/GEO1988-CopernicusDEM-RP-001_ValidationReport_I3.0.pdf/c80c5e85-9aea-356d-c877-80d8b5e028bb?t=1668162072523",
    
    "Woakwine", "S37365E140006_S37410E140063_LT_DTM", "AW3D", 2023, 0, 0
    , "https://www.aw3d.jp/en/products/standard/"
    , NA, "https://www.aw3d.jp/wp/wp-content/themes/AW3DEnglish/technology/doc/pdf/technology_02.pdf",
    
    
    "Woakwine", "WorldDEMNeo_DTM_015_S37_41_E140_00_DEM", "WldNEO DTM", 2023, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/"
    , NA, "https://www.intelligence-airbusds.com/automne/api/docs/v1.0/document/download/ZG9jdXRoZXF1ZS1kb2N1bWVudC02ODQwNw==/ZG9jdXRoZXF1ZS1maWxlLTY4NDA0/worlddem-neo-technical-description-092021.pdf",
    
    "Woakwine", "WorldDEMNeo_DTM_030_S37_41_E140_00_DEM", "WldNEO DTM", 2023, 0, 0
    , "https://www.intelligence-airbusds.com/imagery/reference-layers/worlddem/"
    , NA, "https://www.intelligence-airbusds.com/automne/api/docs/v1.0/document/download/ZG9jdXRoZXF1ZS1kb2N1bWVudC02ODQwNw==/ZG9jdXRoZXF1ZS1maWxlLTY4NDA0/worlddem-neo-technical-description-092021.pdf",
    
    
    "Woakwine", "S37365E140006_S37410E140063_LT_DSM", "AW3D filt", 2023, 0, 0
    , "https://www.aw3d.jp/en/products/standard/"
    , NA, "https://www.aw3d.jp/wp/wp-content/themes/AW3DEnglish/technology/doc/pdf/technology_02.pdf",
    
    "Woakwine", "S37377E140027_S37398E140051_LT_DTM", "AW3D en", 2023, 0, 1
    , ""
    , NA, "",
    
    "Woakwine", "S37377E140027_S37399E140051_LT_DTM", "AW3D en", 2023, 0, 0
    , ""
    , NA, ""
    
    ) %>%
    dplyr::filter(!(area == "Woakwine" & year %in% c(2011, 2015)))
  
  rasters <- fs::dir_ls(ras_dir
                        , regexp = "\\.tif$"
                        , recurse = TRUE
                        ) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::mutate(area = basename(dirname(path))
                  , name = gsub("\\.tif", "", basename(path))
                  , file_date = fs::file_info(path)
                  , ras = purrr::map(path, terra::rast)
                  ) %>%
    dplyr::left_join(luraster %>%
                       dplyr::select(area, name, short, ref_flag, smallest)
                     ) %>%
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
                                  , test_intersection
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
  
  windows <- tibble::tibble(window = c(3))
  
  terr <- samples %>%
    dplyr::cross_join(windows) %>%
    dplyr::mutate(terr = purrr::map2(ras2020
                                     , window
                                     , MultiscaleDTM::Qfit
                                     , na.rm = TRUE
                                     , include_scale = TRUE
                                     )
                  )
  
  terr <- terr %>%
    dplyr::mutate(classes = purrr::map2(ras2020
                                          , window
                                          , ~ land_class(ras = .x
                                                         , sn = .y
                                                         , n.classes = "ten"
                                                         , ln = 11
                                                         )
                                          )
                  , classes = purrr::map2(classes, short
                                          , setNames
                                          )
                  )
  
  
  # values --------
  
  vals_cont <- terr %>%
    dplyr::mutate(rasVal = purrr::map2(terr
                                       , ID
                                       , terra::extract
                                       , xy = TRUE
                                       )
                  ) %>%
    dplyr::select(!where(is.list), ID, rasVal) %>%
    tidyr::unnest(cols = c(ID, rasVal))
  
  vals_cat <- terr %>%
    dplyr::mutate(rasVal = purrr::map2(classes
                                       , ID
                                       , terra::extract
                                       , xy = TRUE
                                       )
                  ) %>%
    dplyr::select(!where(is.list), ID, rasVal) %>%
    tidyr::unnest(cols = c(ID, rasVal))
  
  
#------Analysis--------
  
  terr_cont <- grep("feature"
                    , as.character(formals(MultiscaleDTM::Qfit)$metrics)[-1]
                    , value = TRUE
                    , invert = TRUE
                    )
  
  terr_cat <- levels(terr$classes[[1]])[[1]]$category
  
    
  # continuous-------
  
  cont <- vals_cont %>%
    dplyr::select(ID
                  , x
                  , y
                  , any_of(names(samples))
                  , matches(terr_cont)
                  , -matches("features")
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
  
  categ <- vals_cat %>%
    dplyr::select(area
                  , ID
                  , x
                  , y
                  , original_cm
                  , any_of(unique(samples$short))
                  ) %>%
    tidyr::pivot_longer(tidyselect::any_of(samples$short)
                        , names_to = "short"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(area, ID, name = "count") %>%
    dplyr::group_by(area, ID) %>%
    dplyr::filter(count == max(count)) %>%
    dplyr::ungroup() %>%
    dplyr::count(area, short, original_cm, value) %>%
    dplyr::group_by(area, short, original_cm) %>%
    dplyr::mutate(values = sum(n)
                  , prop = n / values
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(area, short, original_cm)
    
  categ_compare_ref <- vals_cat %>%
    dplyr::select(area
                  , ID
                  , ref_flag
                  , x
                  , y
                  , original_cm
                  , any_of(unique(samples$short))
                  ) %>%
    tidyr::pivot_longer(tidyselect::any_of(samples$short)
                        , names_to = "short"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::add_count(area, ID, name = "count") %>%
    dplyr::group_by(area, ID) %>%
    dplyr::filter(count == max(count)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(area, ID) %>%
    dplyr::mutate(has_ref = sum(as.logical(ref_flag)) > 0) %>%
    dplyr::filter(has_ref) %>%
    dplyr::mutate(reference = value[ref_flag == 1]) %>%
    dplyr::ungroup() %>%
    dplyr::count(area, short, original_cm, reference, value) %>%
    dplyr::group_by(area, short, original_cm, reference) %>%
    dplyr::mutate(references = sum(n)
                  , prop = n / references
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(area, short, original_cm, reference)
  
  