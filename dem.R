
# source -------
targets::tar_source()

# settings ------
settings <- yaml::read_yaml("settings/setup.yaml")
analysis <- yaml::read_yaml("settings/analysis.yaml")

# base grid ---------
base <- fs::path(envFunc::name_env_out(list(context = settings$extent
                                            , grain = settings$grain
                                            )
                                       , base_dir = "I:"
                                       )$path
                 , "base.tif"
                 )

# dem path -------
dem_path <- fs::path("V:/Samba_ImageServices_Admin/Digital_Elevation_Models/Statewide")

# aois ---------
aoi <- yaml::read_yaml("settings/aoi.yaml") |>
  purrr::map(\(x) tibble::as_tibble(x)) |>
  dplyr::bind_rows(.id = "aoi_name") |>
  #dplyr::filter(grepl("Kanku", aoi_name)) |> # TESTING
  dplyr::mutate(sf = purrr::map(polygons
                                , \(x) sfarrow::st_read_parquet(fs::path("..", "..", ".."
                                                                         , "data", "vector"
                                                                         , paste0(x, ".parquet")
                                                                         )
                                                                )
                                )
                , aoi = purrr::pmap(list(sf, filt_col, filt_level, buffer)
                                    , \(a, b, c, d) envFunc::make_aoi(a, b, c, buffer = d)
                                    )
                , base = purrr::map(aoi
                                    , \(x) apply_window(terra::rast(base), x)
                                    )
                )

# dems ----------
dem <- yaml::read_yaml("settings/dem.yaml") |>
  purrr::map(\(x) tibble::as_tibble(x)) |>
  dplyr::bind_rows(.id = "dem_name") |>
  dplyr::mutate(full_path = purrr::map(file
                                       , \(x) fs::dir_ls(dem_path
                                                         , regexp = paste0(x,"$")
                                                         , recurse = TRUE
                                                         )
                                       )
                ) |>
  tidyr::unnest(cols = c(full_path)) |>
  dplyr::mutate(r = purrr::map(full_path, terra::rast))

# dem analysis -------
window <- dem |>
  dplyr::cross_join(aoi) |>
  dplyr::mutate(overlap = purrr::map2(r, aoi, test_intersection)) |>
  tidyr::unnest(cols = c(overlap)) |>
  dplyr::filter(overlap > 0.9) |>
  dplyr::mutate(r = purrr::map2(r, aoi, apply_window)
                , r = purrr::map(r
                                 , terra::project
                                 , y = paste0("epsg:", settings$crs$proj)
                                 )
                , original_cm = purrr::map_dbl(r, \(x) round(terra::res(x)[[1]] * 100, 0))
                , old_to_new = (settings$grain$res * 100) / original_cm
                , method = dplyr::case_when(old_to_new > 3 ~ "aggregate"
                                            , old_to_new < 0.5 ~ "disagg"
                                            , TRUE ~ "no_function"
                                            )
                , fact = dplyr::case_when(method == "aggregate" ~ round(old_to_new, 0)
                                          , method == "disagg" ~ 3
                                          , method == "no_function" ~ Inf
                                          )
                , r = purrr::pmap(list(r
                                       , method
                                       , fact
                                       )
                                  , \(a, b, c) {
                                    
                                    get(b)(a, c)
                                    
                                  }
                                  )
                , r = purrr::map2(r
                                  , base
                                  , terra::project
                                  , method = "bilinear"
                                  , .progress = "project"
                                  )
                , t = purrr::map(r
                                 , \(x) MultiscaleDTM::Qfit(x
                                                            , na.rm = TRUE
                                                            )
                                 , .progress = "qfit"
                                 )
                , l = purrr::map(r
                                 , \(x) land_class(x
                                                   , n.classes = analysis$classes
                                                   , sn = analysis$sn
                                                   , ln = analysis$ln
                                                   )
                                 , .progress = "land class"
                                 )
                 , vals_raw = purrr::map(r
                                         , \(x) vals_to_tibble(x)
                                         )
                 , vals_t = purrr::map(t
                                       , \(x) vals_to_tibble(x)
                                       )
                 , vals_l = purrr::map(l
                                       , \(x) vals_to_tibble(x)
                                       )
                 ) |>
  dplyr::group_by(aoi_name) |>
  dplyr::mutate(is_ref = original_cm == min(original_cm)) |>
  dplyr::ungroup()

# analysis--------

terr_cont <- grep("feature"
                  , as.character(formals(MultiscaleDTM::Qfit)$metrics)[-1]
                  , value = TRUE
                  , invert = TRUE
                  )
  
terr_cat <- levels(window$l[[1]])[[1]]$category
  
    
## continuous-------
  
cont <- window |>
  dplyr::select(aoi_name, dem_name
                , is_ref
                , vals_t
                , original_cm
                ) |>
  tidyr::unnest(cols = c(vals_t)) |>
  dplyr::select(aoi_name, dem_name, cell, is_ref, original_cm, tidyselect::matches(terr_cont)) |>
  tidyr::pivot_longer(matches(terr_cont)
                      , names_to = "metric"
                      ) %>%
  dplyr::filter(!is.na(value)) %>%
  # ensure only cells that have data from each DEM are included
  dplyr::add_count(aoi_name, metric, cell, name = "count") %>%
  dplyr::group_by(aoi_name) %>%
  dplyr::filter(count == max(count)) %>%
  dplyr::ungroup() %>%
  # find reference value
  dplyr::group_by(aoi_name, metric, cell) |>
  dplyr::mutate(ref = value[is_ref]) |>
  dplyr::ungroup() %>%
  dplyr::mutate(diff = ref - value) %>%
  tidyr::nest(data = -c(aoi_name, metric)) %>%
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
                        
    
## categorical-------

categ <- window |>
  dplyr::select(aoi_name, dem_name
                , is_ref
                , vals_l
                , original_cm
                ) |>
  tidyr::unnest(cols = c(vals_l))  %>%
  dplyr::filter(!is.na(category)) %>%
  # ensure only cells that have data from each DEM are included
  dplyr::add_count(aoi_name, cell, name = "count") %>%
  dplyr::group_by(aoi_name) %>%
  dplyr::filter(count == max(count)) %>%
  dplyr::ungroup() %>%
  dplyr::count(aoi_name, dem_name, original_cm, category) %>%
  dplyr::group_by(aoi_name, dem_name, original_cm) %>%
  dplyr::mutate(values = sum(n)
                , prop = n / values
                ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(aoi_name, dem_name, original_cm)
  
categ_compare_ref <- window |>
  dplyr::select(aoi_name, dem_name
                , is_ref
                , vals_l
                , original_cm
                ) |>
  tidyr::unnest(cols = c(vals_l)) %>%
  dplyr::filter(!is.na(category)) %>%
  # ensure only cells that have data from each DEM are included
  dplyr::add_count(aoi_name, cell, name = "count") %>%
  dplyr::group_by(aoi_name) %>%
  dplyr::filter(count == max(count)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(aoi_name, cell) %>%
  dplyr::mutate(has_ref = sum(is_ref) > 0) %>%
  dplyr::filter(has_ref) %>%
  dplyr::mutate(reference = category[is_ref]) %>%
  dplyr::ungroup() %>%
  dplyr::count(aoi_name, dem_name, original_cm, reference, category) %>%
  dplyr::group_by(aoi_name, dem_name, original_cm, reference) %>%
  dplyr::mutate(references = sum(n)
                , prop = n / references
                ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(aoi_name, dem_name, original_cm, reference)
  
  
