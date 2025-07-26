test_intersection <- function(ras, sf, ret = c("ratio", "logical")){
  
  ret <- ret[[1]]
  
  if(! "spatRaster" %in% class(ras)) ras <- terra::rast(ras)
  
  ext_a <- terra::ext(ras) |>
    terra::as.polygons()

  terra::crs(ext_a) <- terra::crs(ras)

  ext_b <- terra::ext(sf) |>
    terra::as.polygons()

  terra::crs(ext_b) <- terra::crs(sf)

  ext_b <- ext_b |>
    terra::project(y = terra::crs(ras))

  int <- terra::intersect(ext_a, ext_b)

  ratio <- terra::expanse(int) / terra::expanse(ext_b)
  
  res <- if(ret == "ratio") {
    
    ratio
    
  } else if(ret == "logical") {
    
    if(isTRUE(ratio > 0)) TRUE else FALSE
    
  }
  
  return(res)
}
