
  library(tmap)
  tmap_mode("view")

  use_epsg <- 8059
  target_area <- 545000
  use_min_northing <- 1563500 # taken from visual inspection of se_lidar (2007)
  use_max_northing <- seq(2330000, 2230000, -1000)
    
  bbox <- sfarrow::st_read_parquet(fs::path("H:", "data", "vector", "sa_ibrasub_xn.parquet")) |>
    sf::st_transform(crs = use_epsg) |>
    sf::st_bbox()
  
  bbox["ymin"] <- use_min_northing
  
  bbox <- purrr::map(use_max_northing
                     , \(x) {
                       
                       bbox["ymax"] = x
                       
                       return(bbox)
                       }
                     )
  
  bbox <- purrr::map(bbox
                     , \(x) x |>
                       sf::st_as_sfc() |>
                       sf::st_sf()
                     )
  
  # se_lidar <- terra::rast("V:/Samba_ImageServices_Admin/Digital_Elevation_Models/SouthEast_NRM/2007_SouthEastLiDAR_2m_DEWNR ONLY/MGA/SouthEastLiDAR_15Oct2007-22May2008_DEM_2m_MGA54.tif")

  sa <- sfarrow::st_read_parquet(fs::path("H:", "data", "vector", "aus_states.parquet")) |>
    dplyr::filter(STATE %in% c("SA")) |>
    dplyr::filter(FEATURECODE != "island") |>
    dplyr::summarise() |>
    sf::st_transform(crs = use_epsg) |>
    sf::st_make_valid()

  boundary <- purrr::map(bbox
                         , \(x) sa |>
                           sf::st_intersection(y = x) |>
                           dplyr::summarise() |>
                           sf::st_make_valid()
                         )
  
  x <- as.numeric(purrr::map(boundary, \(x) sf::st_area(x))) / 1e6
  
  result <- which(abs(x - target_area) == min(abs(x - target_area)))
  
  tm_shape(boundary[[result]]) + tm_borders()
  
  sf::st_write(boundary[[result]]
               , fs::path("rfq/rfq_boundary.shp")
               , append = FALSE
               )
  