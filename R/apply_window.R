apply_window <- function(r_path
                         , aoi
                         , name = NULL
                         ) {
  
  r <- terra::rast(r_path)
  
  aoi <- aoi |>
    sf::st_transform(crs = sf::st_crs(r))
  
  terra::window(r) <- terra::ext(terra::vect(aoi))
  
  if(!is.null(name)) {
    
    terra::set.names(r, name)
  
  }
  
  return(r)
  
}
