test_intersection <- function(a, b){

  ext_a <- terra::ext(a) |>
    terra::as.polygons()

  terra::crs(ext_a) <- terra::crs(a)

  ext_b <- terra::ext(b) |>
    terra::as.polygons()

  terra::crs(ext_b) <- terra::crs(b)

  ext_b <- ext_b |>
    terra::project(y = terra::crs(a))

  int <- terra::intersect(ext_a, ext_b)

  ratio <- terra::expanse(ext_a) / terra::expanse(int)

  if(isTRUE(ratio > 0)) TRUE else FALSE

}
