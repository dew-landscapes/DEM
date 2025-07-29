vals_to_tibble <- function(r) {
  
  terra::extract(r
                 , y = 1:terra::ncell(r)
                 , xy = TRUE
                 ) |>
    tibble::as_tibble() |>
    dplyr::bind_rows() |>
    dplyr::select(x, y, everything())
  
}