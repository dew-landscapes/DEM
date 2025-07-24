test_intersection <- function(a, b, ret = c("ratio", "logical")){
  
  ret <- ret[[1]]

  ext_a <- terra::ext(a) |>
    terra::as.polygons()

  terra::crs(ext_a) <- terra::crs(a)

  ext_b <- terra::ext(b) |>
    terra::as.polygons()

  terra::crs(ext_b) <- terra::crs(b)

  ext_b <- ext_b |>
    terra::project(y = terra::crs(a))

  int <- terra::intersect(ext_a, ext_b)

  ratio <- terra::expanse(int) / terra::expanse(ext_b)
  
  res <- if(ret == "ratio") {
    
    ratio
    
  } else if(ret == "logical") {
    
    if(isTRUE(ratio > 0)) TRUE else FALSE
    
  }
  
  return(res)
}
