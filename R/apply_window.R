apply_window <- function(r
                         , aoi
                         ) {
  
  aoi <- aoi |>
    sf::st_transform(crs = sf::st_crs(r))
  
  terra::window(r) <- terra::ext(terra::vect(aoi))
  
  return(r)
  
}
