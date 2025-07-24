vals_to_tibble <- function(r) {
  
  terra::extract(r
                 , y = 1:terra::ncell(r)
                 , xy = TRUE
                 ) |>
    tibble::as_tibble() |>
    dplyr::mutate(cell = 1:terra::ncell(r)) |>
    dplyr::select(cell, x, y, everything())
  
}