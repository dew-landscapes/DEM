mung_ras <- function(ras_path
                     , aoi
                     , base_path
                     ) {
  
  # full dem
  dem <- terra::rast(ras_path)
  
  # full base
  base <- terra::rast(base_path)
  
  # window base
  terra::window(base) <- terra::ext(terra::vect(aoi))
  
  # aoi in crs of dem
  aoi_dem <- aoi |>
    sf::st_transform(crs = sf::st_crs(dem))
  
  # window dem
  window(dem) <- terra::ext(aoi_dem)
  
  dem <- terra::project(dem, base)
  
}